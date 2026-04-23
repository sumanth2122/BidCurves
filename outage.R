library(tidyverse)
library(duckdb)
library(DBI)

generator_outages <- function(generators) {
  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dbGetQuery(
    con,
    glue::glue_sql(
      "
      SELECT DISTINCT
        \"RESOURCE ID\" AS generator,
        \"CURTAILMENT START DATE TIME\" AS outage_start,
        \"CURTAILMENT END DATE TIME\" AS outage_stop
      FROM read_parquet({here::here('prior_trade_day_outages.parquet')})
      WHERE \"RESOURCE ID\" IN ({generators*})
      ORDER BY generator, outage_start, outage_stop
      ",
      generators = generators,
      .con = con
    )
  ) |>
    as_tibble() |>
    summarise(
      outage_stop = if (all(is.na(outage_stop))) {
        as.POSIXct(NA, tz = "UTC")
      } else {
        max(outage_stop, na.rm = TRUE)
      },
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
        as.POSIXct(NA, tz = "UTC")
      } else {
        max(outage_stop, na.rm = TRUE)
      },
      .by = c(generator, outage_group)
    ) |>
    select(generator, outage_start, outage_stop) |>
    arrange(generator, outage_start, outage_stop)
}

rtm_bid_submissions <- function(id, lead_minutes = 5) {
  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dbGetQuery(
    con,
    glue::glue_sql(
      "
      SELECT DISTINCT
        try_strptime(
          SCH_BID_TIMEINTERVALSTART_GMT,
          '%Y-%m-%dT%H:%M:%S-00:00'
        ) AS bid_start
      FROM read_parquet({here::here('CAISO_rtm_big.parquet')})
      WHERE RESOURCEBID_SEQ = {id}
      ORDER BY bid_start
      ",
      id = id,
      .con = con
    )
  ) |>
    as_tibble() |>
    mutate(bid_submitted = bid_start - lubridate::minutes(lead_minutes))
}

outage_rtm <- function(
  id,
  generators,
  lead_minutes = 5
) {
  generator_outages(generators) |>
    mutate(
      outage_stop = coalesce(
        outage_stop,
        as.POSIXct("2999-12-31 00:00:00", tz = "UTC")
      )
    ) |>
    inner_join(
      rtm_bid_submissions(id, lead_minutes),
      join_by(outage_start <= bid_submitted, outage_stop > bid_submitted),
      relationship = "many-to-many"
    ) |>
    summarise(conflicts = n_distinct(bid_submitted)) |>
    pull(conflicts)
}

generator_outages(c("DIABLO_7_UNIT 1", "DIABLO_7_UNIT 2"))
c(386484, 274516, 338858, 793549, 952796) |>
  map(\(g) outage_rtm(g, c("DIABLO_7_UNIT 1", "DIABLO_7_UNIT 2")))
