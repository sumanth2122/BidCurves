library(tidyverse)
library(duckdb)
library(DBI)
library(gt)
use("here", "here")

#' Extract full generator outages from CAISO prior trade day data
#'
#' Reads `prior_trade_day_outages.parquet` and filters to rows where the
#' curtailed MW meets or exceeds the generator's PMAX (i.e. the whole unit is
#' down, not just derated). Overlapping outage windows for the same generator
#' are merged into contiguous intervals.
#'
#' @param generators Character vector of resource IDs to keep. If `NULL`
#'   (default), all generators are returned.
#'
#' @return A tibble with columns:
#'   - `generator`: resource ID
#'   - `outage_start`: start of the outage (POSIXct)
#'   - `outage_stop`: end of the outage (POSIXct, `NA` if still ongoing)
#'   - `pmax_mw`: generator max capacity
generator_outages <- function(generators = NULL) {
  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Build an optional WHERE clause to restrict to specific generators
  generator_filter <- if (is.null(generators)) {
    DBI::SQL("")
  } else {
    glue::glue_sql(
      "AND \"RESOURCE ID\" IN ({generators*})",
      generators = generators,
      .con = con
    )
  }

  # Pull full outages: only keep rows where the unit is completely down
  # (curtailed MW >= rated capacity). Partial derates are excluded.
  dbGetQuery(
    con,
    glue::glue_sql(
      "
      SELECT DISTINCT
        \"RESOURCE ID\" AS generator,
        \"CURTAILMENT START DATE TIME\" AS outage_start,
        \"CURTAILMENT END DATE TIME\" AS outage_stop,
        \"RESOURCE PMAX MW\" AS pmax_mw
      FROM read_parquet({here::here('prior_trade_day_outages.parquet')})
      WHERE \"RESOURCE PMAX MW\" > 0
        AND \"CURTAILMENT MW\" >= \"RESOURCE PMAX MW\"
        {generator_filter}
      ORDER BY generator, outage_start, outage_stop
      ",
      generator_filter = generator_filter,
      .con = con
    )
  ) |>
    as_tibble() |>
    # CAISO sometimes reports the same outage as multiple rows with the same
    # start time but different stop times. Collapse them -- take the latest stop.
    summarise(
      outage_stop = if (all(is.na(outage_stop))) {
        outage_start[NA_integer_]
      } else {
        max(outage_stop, na.rm = TRUE)
      },
      pmax_mw = max(pmax_mw, na.rm = TRUE),
      .by = c(generator, outage_start)
    ) |>
    arrange(generator, outage_start, outage_stop) |>
    # Merge overlapping or touching outage windows into contiguous groups.
    # merge_stop: if an outage has no end, assume it extends to the next
    # outage's start (or its own start if there is no next outage).
    # outage_group: increments whenever the current start falls after all
    # previous merged stops -- i.e. this is a new, non-overlapping interval.
    mutate(
      merge_stop = coalesce(outage_stop, lead(outage_start), outage_start),
      outage_group = cumsum(
        coalesce(
          as.numeric(outage_start) > lag(cummax(as.numeric(merge_stop))),
          TRUE
        )
      ),
      .by = generator
    ) |>
    # Collapse each group into a single row spanning the full outage window
    summarise(
      outage_start = min(outage_start),
      outage_stop = if (all(is.na(outage_stop))) {
        outage_start[NA_integer_]
      } else {
        max(outage_stop, na.rm = TRUE)
      },
      pmax_mw = max(pmax_mw, na.rm = TRUE),
      .by = c(generator, outage_group)
    ) |>
    select(generator, outage_start, outage_stop, pmax_mw) |>
    arrange(generator, outage_start, outage_stop)
}

#' Match generator outages to RTM bid gaps
#'
#' When a generator goes offline, its bid curve disappears. This function finds
#' gaps in RTM bid submissions which align with
#' reported outage windows, linking outage records to bid IDs.
#'
#' @param generators Character vector of resource IDs, passed to
#'   [generator_outages()].
#' @param edge_minutes Slack in minutes when aligning outage times to bid gaps
#'   (default 5).
#' @param top_n Max candidate bid IDs per generator, ranked by capacity match
#'   and timing offset (default 10).
#'
#' @return A named list:
#'   - `outages`: cleaned outage table from [generator_outages()]
#'   - `matches`: tibble of generator–bid ID pairs
#'   - `summary`: per-match stats (offsets, capacity gap, match count)
#'   - `details`: raw join results for debugging
outage_rtm <- function(generators = NULL, edge_minutes = 5, top_n = 10) {
  outages <- generator_outages(generators)

  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Register the outage table so DuckDB can join against it
  dbWriteTable(con, "outages", outages, temporary = TRUE, overwrite = TRUE)

  details <- dbGetQuery(
    con,
    glue::glue_sql(
      "
      -- Each bid's time interval, deduplicated. The RTM parquet has two
      -- sets of timestamp columns depending on the record type, so we
      -- coalesce to handle both.
      WITH bids AS (
        SELECT DISTINCT
          CAST(RESOURCEBID_SEQ AS BIGINT) AS id,
          coalesce(
            try_strptime(SCH_BID_TIMEINTERVALSTART, '%Y-%m-%dT%H:%M:%S'),
            try_strptime(TIMEINTERVALSTART, '%Y-%m-%dT%H:%M:%S')
          ) AS bid_start,
          coalesce(
            try_strptime(SCH_BID_TIMEINTERVALSTOP, '%Y-%m-%dT%H:%M:%S'),
            try_strptime(TIMEINTERVALEND, '%Y-%m-%dT%H:%M:%S')
          ) AS bid_stop
        FROM read_parquet({here::here('CAISO_rtm_big.parquet')})
        WHERE MARKETPRODUCTTYPE = 'EN'
          AND RESOURCEBID_SEQ IS NOT NULL
      ),
      -- Find gaps between consecutive bids for each ID. A gap is where
      -- one bid ends and the next one starts -- the generator wasn't bidding
      -- during that window, which likely means it was offline.
      gaps AS (
        SELECT
          id,
          bid_stop AS last_bid_stop,
          lead(bid_start) OVER (
            PARTITION BY id
            ORDER BY bid_start, bid_stop
          ) AS next_bid_start
        FROM bids
        WHERE bid_start IS NOT NULL
          AND bid_stop IS NOT NULL
      ),
      -- The highest MW a bid ID ever offers -- this is our best guess at
      -- that ID's rated capacity, used later to check if it matches the
      -- outage record's PMAX.
      max_mw AS (
        SELECT
          CAST(RESOURCEBID_SEQ AS BIGINT) AS id,
          MAX(SCH_BID_XAXISDATA) AS max_mw
        FROM read_parquet({here::here('CAISO_rtm_big.parquet')})
        WHERE MARKETPRODUCTTYPE = 'EN'
          AND RESOURCEBID_SEQ IS NOT NULL
        GROUP BY 1
      )
      -- Join outages to bid gaps: the gap's last_bid_stop should be near
      -- the outage start (within edge_minutes), and the gap's next_bid_start
      -- should be at or after the outage end. This pins an outage to a
      -- specific bid ID that went quiet at the right time.
      SELECT
        o.generator,
        o.outage_start,
        o.outage_stop,
        o.pmax_mw,
        g.id,
        g.last_bid_stop,
        g.next_bid_start,
        datediff('minute', o.outage_start, g.last_bid_stop) AS stop_offset,
        datediff('minute', o.outage_stop, g.next_bid_start) AS restart_offset,
        m.max_mw
      FROM outages o
      JOIN gaps g
        ON g.last_bid_stop BETWEEN
           o.outage_start - INTERVAL {edge_minutes} MINUTE
           AND o.outage_start + INTERVAL {edge_minutes} MINUTE
       AND coalesce(g.next_bid_start, TIMESTAMP '2999-12-31 00:00:00') >=
           coalesce(o.outage_stop, TIMESTAMP '2999-12-31 00:00:00') -
           INTERVAL {edge_minutes} MINUTE
      LEFT JOIN max_mw m
        ON m.id = g.id
      ORDER BY
        o.generator,
        o.outage_start,
        abs(stop_offset),
        abs(coalesce(m.max_mw, 0) - o.pmax_mw),
        g.id
      ",
      edge_minutes = edge_minutes,
      .con = con
    )
  ) |>
    as_tibble()

  # For each generator-ID pair, aggregate across all matched outages.
  # Then rank candidates: prefer IDs whose bid capacity is closest to the
  # outage PMAX, then by number of matches, then by timing precision.
  summary <- details |>
    summarise(
      n_matches = n(),
      pmax_mw = first(pmax_mw),
      max_mw = first(max_mw),
      mean_stop_offset = mean(stop_offset),
      mean_restart_offset = mean(restart_offset, na.rm = TRUE),
      .by = c(generator, id)
    ) |>
    mutate(capacity_gap = abs(max_mw - pmax_mw)) |>
    arrange(generator, capacity_gap, desc(n_matches), abs(mean_stop_offset)) |>
    slice_head(n = top_n, by = generator)

  matches <- summary |>
    select(generator, id)

  structure(
    list(
      outages = outages,
      matches = matches,
      summary = summary,
      details = details
    ),
    class = "outage_rtm_results"
  )
}

#' @export
print.outage_rtm_results <- function(x, ...) {
  n_gen <- n_distinct(x$outages$generator)
  n_outages <- nrow(x$outages)
  n_matched <- nrow(x$matches)

  cat(sprintf(
    "%d generators, %d outages, %d matched IDs\n\n",
    n_gen,
    n_outages,
    n_matched
  ))

  x$matches |>
    arrange(generator, id) |>
    mutate(generator = str_replace_all(generator, "_", " ")) |>
    group_by(generator) |>
    summarise(ids = paste(id, collapse = ", "), .groups = "drop") |>
    pwalk(~ cat(sprintf("  %-30s %s\n", ..1, ..2)))

  invisible(x)
}

#' Render outage-to-bid matches as a formatted table
#'
#' Displays the matches tibble using gt.
#'
#' @param matches A tibble with columns `generator` and `id`, typically from
#'   [outage_rtm()]`$matches`.
#'
#' @return A gt table object.
display_matches <- function(matches) {
  matches |>
    arrange(generator, id) |>
    mutate(
      group_start = generator != lag(generator, default = first(generator)),
      generator_display = if_else(
        generator == lag(generator, default = ""),
        "",
        generator |> str_replace_all("_", " ")
      )
    ) |>
    select(generator = generator_display, id, group_start) |>
    gt() |>
    cols_hide(group_start) |>
    cols_label(
      generator = "Generator",
      id = "Matched ID"
    ) |>
    tab_options(
      table.font.size = px(13),
      data_row.padding = px(4),
      column_labels.font.weight = "bold",
      column_labels.border.bottom.width = px(2),
      table_body.hlines.width = px(0)
    ) |>
    tab_style(
      style = cell_borders(
        sides = "top",
        color = "gray40",
        weight = px(2)
      ),
      locations = cells_body(
        rows = group_start
      )
    ) |>
    tab_style(
      style = cell_borders(
        sides = "top",
        color = "gray85",
        weight = px(1)
      ),
      locations = cells_body(
        columns = id,
        rows = !group_start
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = generator,
        rows = generator != ""
      )
    ) |>
    cols_align(
      align = "left",
      columns = generator
    ) |>
    cols_align(
      align = "center",
      columns = id
    )
}

# Five big units to test the matching pipeline
large_generator_matches <- outage_rtm(c(
  "DIABLO_7_UNIT 1",
  "DIABLO_7_UNIT 2",
  "HYTTHM_2_UNITS",
  "DELTA_2_PL1X4",
  "HIDSRT_2_UNITS"
))

large_generator_matches$matches |> display_matches()
