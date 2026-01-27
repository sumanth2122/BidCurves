library(tidyverse)
library(httr)
library(lubridate)

fetch_oasis_prices <- function(
  start_date,
  end_date,
  market = c("RTM", "DAM"),
  request_pause = 5,
  retry_times = 15,
  retry_pause_base = 3,
  retry_pause_cap = 20
) {
  market <- match.arg(market)
  dates <- seq.Date(as.Date(start_date), as.Date(end_date), by = "day")

  temp_dir <- tempfile("oasis_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  map(dates, \(d) {
    fetch_one(
      date = d,
      temp_dir = temp_dir,
      market = market,
      request_pause = request_pause,
      retry_times = retry_times,
      retry_pause_base = retry_pause_base,
      retry_pause_cap = retry_pause_cap
    )
  }) |>
    list_rbind()
}

fetch_one <- function(
  date,
  temp_dir,
  market,
  request_pause,
  retry_times,
  retry_pause_base,
  retry_pause_cap
) {
  # DST-safe: local midnight in America/Los_Angeles converted to UTC
  local_midnight <- ymd(as.character(date), tz = "America/Los_Angeles")
  start_datetime <- format(with_tz(local_midnight, "UTC"), "%Y%m%dT%H:%M-0000")

  temp_zip <- tempfile(fileext = ".zip", tmpdir = temp_dir)

  url <- modify_url(
    "https://oasis.caiso.com/oasisapi/GroupZip",
    query = list(
      resultformat = 6,
      version = 3,
      groupid = sprintf("PUB_%s_GRP", market),
      startdatetime = start_datetime
    )
  )

  if (request_pause > 0) {
    Sys.sleep(request_pause)
  }

  response <- RETRY(
    "GET",
    url,
    user_agent("Mozilla/5.0"),
    config(http_version = 1),
    write_disk(temp_zip, overwrite = TRUE),
    times = retry_times,
    pause_base = retry_pause_base,
    pause_cap = retry_pause_cap
  )
  stop_for_status(response)

  # Unzip into temp_dir so cleanup works
  unzipped_files <- unzip(temp_zip, exdir = temp_dir)

  # Market-aware file match
  bid_pattern <- sprintf("PUB_BID_%s", market)
  bid_files <- unzipped_files[grepl(bid_pattern, basename(unzipped_files))]

  if (length(bid_files) == 0) {
    warning(sprintf("No %s files found for %s", bid_pattern, date))
    return(tibble())
  }

  map_dfr(bid_files, \(f) read_csv(f, show_col_types = FALSE))
}
