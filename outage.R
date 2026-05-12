library(tidyverse)
library(duckdb)
library(DBI)
library(gt)
use("here", "here")

generator_outages <- function(generators = NULL) {
  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  generator_filter <- if (is.null(generators)) {
    DBI::SQL("")
  } else {
    glue::glue_sql(
      "AND \"RESOURCE ID\" IN ({generators*})",
      generators = generators,
      .con = con
    )
  }

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

outage_rtm <- function(generators = NULL, edge_minutes = 5, top_n = 10) {
  outages <- generator_outages(generators)

  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dbWriteTable(con, "outages", outages, temporary = TRUE, overwrite = TRUE)

  details <- dbGetQuery(
    con,
    glue::glue_sql(
      "
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
      max_mw AS (
        SELECT
          CAST(RESOURCEBID_SEQ AS BIGINT) AS id,
          MAX(SCH_BID_XAXISDATA) AS max_mw
        FROM read_parquet({here::here('CAISO_rtm_big.parquet')})
        WHERE MARKETPRODUCTTYPE = 'EN'
          AND RESOURCEBID_SEQ IS NOT NULL
        GROUP BY 1
      )
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

  list(
    outages = outages,
    matches = matches,
    summary = summary,
    details = details
  )
}

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

large_generator_matches <- outage_rtm(c(
  "DIABLO_7_UNIT 1",
  "DIABLO_7_UNIT 2",
  "HYTTHM_2_UNITS",
  "DELTA_2_PL1X4",
  "HIDSRT_2_UNITS"
))

large_generator_matches$matches |> display_matches()
# TODO: collect IDs per generator and return vector, write a print method?

# TODO: an inverse map which goes from IDs to generators based on matched outages
