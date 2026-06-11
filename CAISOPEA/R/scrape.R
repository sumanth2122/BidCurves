#' Fetch CAISO OASIS bid data by day
#'
#' Fetches CAISO OASIS public bid data for DAM or RTM and writes one parquet file
#' per market day.
#'
#' @param start_date Start date. Coercible to `Date`.
#' @param end_date End date. Coercible to `Date`.
#' @param market Market type. Either `"DAM"` or `"RTM"`.
#' @param output_dir Directory where daily parquet files are written.
#' @param skip_existing If `TRUE`, skip dates whose output parquet already
#'   exists.
#' @param request_pause Seconds to sleep before each request.
#' @param retry_times Number of retries for transient request failures.
#' @param retry_pause_base Base seconds between retries.
#' @param retry_pause_cap Maximum seconds between retries.
#'
#' @return A tibble with one row per requested date and columns `date`, `market`,
#'   `status`, `path`, `n_rows`, and `message`.
#'
#' @export
fetch_oasis_bids <- function(
  start_date,
  end_date,
  market = c("DAM", "RTM"),
  output_dir = sprintf("CAISO_%s_daily", tolower(market)),
  skip_existing = TRUE,
  request_pause = 5,
  retry_times = 15,
  retry_pause_base = 3,
  retry_pause_cap = 20
) {
  market <- match.arg(market)

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if (is.na(start_date) || is.na(end_date)) {
    stop("start_date and end_date must be coercible to Date.", call. = FALSE)
  }

  if (end_date < start_date) {
    stop("end_date must be greater than or equal to start_date.", call. = FALSE)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  all_dates <- seq(start_date, end_date, by = "day")

  output_path_for_date <- function(date) {
    file.path(
      output_dir,
      sprintf("%s.parquet", format(as.Date(date), "%Y%m%d"))
    )
  }

  if (isTRUE(skip_existing)) {
    output_paths <- vapply(
      all_dates,
      output_path_for_date,
      character(1)
    )

    all_dates <- all_dates[!file.exists(output_paths)]
  }

  if (length(all_dates) == 0L) {
    return(tibble::tibble(
      date = as.Date(character()),
      market = character(),
      status = character(),
      path = character(),
      n_rows = integer(),
      message = character()
    ))
  }

  temp_dir <- tempfile("caiso_oasis_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  results <- lapply(all_dates, function(date) {
    output_path <- output_path_for_date(date)

    tryCatch(
      {
        data <- fetch_oasis_bid_day(
          date = date,
          market = market,
          temp_dir = temp_dir,
          request_pause = request_pause,
          retry_times = retry_times,
          retry_pause_base = retry_pause_base,
          retry_pause_cap = retry_pause_cap
        )

        if (is.null(data) || nrow(data) == 0L) {
          return(tibble::tibble(
            date = as.Date(date),
            market = market,
            status = "no_data",
            path = NA_character_,
            n_rows = 0L,
            message = "No bid data returned."
          ))
        }

        arrow::write_parquet(data, output_path)

        tibble::tibble(
          date = as.Date(date),
          market = market,
          status = "written",
          path = output_path,
          n_rows = nrow(data),
          message = NA_character_
        )
      },
      error = function(e) {
        tibble::tibble(
          date = as.Date(date),
          market = market,
          status = "error",
          path = NA_character_,
          n_rows = NA_integer_,
          message = conditionMessage(e)
        )
      }
    )
  })

  dplyr::bind_rows(results)
}

#' Fetch one CAISO OASIS bid-data day
#'
#' @keywords internal
fetch_oasis_bid_day <- function(
  date,
  market,
  temp_dir,
  request_pause,
  retry_times,
  retry_pause_base,
  retry_pause_cap
) {
  Sys.sleep(request_pause)

  date <- as.Date(date)

  temp_zip <- tempfile(
    pattern = sprintf("PUB_BID_%s_%s_", market, format(date, "%Y%m%d")),
    fileext = ".zip",
    tmpdir = temp_dir
  )

  startdatetime <- date |>
    lubridate::ymd(tz = "America/Los_Angeles") |>
    lubridate::with_tz("UTC") |>
    format("%Y%m%dT%H:%M-0000")

  url <- httr::modify_url(
    "https://oasis.caiso.com/oasisapi/GroupZip",
    query = list(
      resultformat = 6,
      version = 3,
      groupid = sprintf("PUB_%s_GRP", market),
      startdatetime = startdatetime
    )
  )

  response <- httr::RETRY(
    verb = "GET",
    url = url,
    httr::user_agent("Mozilla/5.0"),
    httr::config(http_version = 1),
    httr::write_disk(temp_zip, overwrite = TRUE),
    times = retry_times,
    pause_base = retry_pause_base,
    pause_cap = retry_pause_cap
  )

  httr::stop_for_status(response)

  unzipped_files <- tryCatch(
    utils::unzip(temp_zip, exdir = temp_dir),
    error = function(e) {
      stop(
        "Downloaded response was not a readable zip for ",
        market,
        " ",
        format(date, "%Y-%m-%d"),
        ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  bid_pattern <- sprintf("PUB_BID_%s", market)

  bid_files <- unzipped_files[
    stringr::str_detect(basename(unzipped_files), bid_pattern)
  ]

  if (length(bid_files) == 0L) {
    if (any(vapply(unzipped_files, is_oasis_no_data_file, logical(1)))) {
      return(NULL)
    }

    stop(
      "No ",
      bid_pattern,
      " files found for ",
      format(date, "%Y-%m-%d"),
      ".",
      call. = FALSE
    )
  }

  nonempty_bid_files <- bid_files[
    !vapply(bid_files, is_oasis_no_data_file, logical(1))
  ]

  if (length(nonempty_bid_files) == 0L) {
    return(NULL)
  }

  dfs <- lapply(
    nonempty_bid_files,
    readr::read_csv,
    show_col_types = FALSE
  )

  dplyr::bind_rows(dfs)
}

#' Detect OASIS no-data XML responses
#'
#' @keywords internal
is_oasis_no_data_file <- function(path) {
  lines <- readLines(path, n = 30L, warn = FALSE)

  if (length(lines) == 0L) {
    return(TRUE)
  }

  is_xml <- stringr::str_detect(lines[[1]], "^<\\?xml")

  is_no_data <- any(stringr::str_detect(
    lines,
    "No data returned for the specified selection"
  ))

  is_xml && is_no_data
}
