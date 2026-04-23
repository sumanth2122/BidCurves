library(tidyverse)
library(DBI)
library(duckdb)
use("here", "here")

generator_outages <- function(generator) {
  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dbGetQuery(
    con,
    glue::glue_sql(
      "
      SELECT DISTINCT
        \"RESOURCE ID\" AS generator,
        \"RESOURCE NAME\" AS generator_name,
        \"OUTAGE TYPE\" AS outage_type,
        \"NATURE OF WORK\" AS nature_of_work,
        \"CURTAILMENT START DATE TIME\" AS outage_start,
        \"CURTAILMENT END DATE TIME\" AS outage_stop,
        \"CURTAILMENT MW\" AS curtailment_mw
      FROM read_parquet({here('prior_trade_day_outages.parquet')})
      WHERE \"RESOURCE ID\" = {generator}
      ORDER BY outage_start, outage_stop
      ",
      generator = generator,
      .con = con
    )
  ) |>
    as_tibble() |>
    summarise(
      outage_stop = {
        known_stops <- outage_stop[!is.na(outage_stop)]
        if (length(known_stops) == 0) {
          as.POSIXct(NA, tz = "UTC")
        } else {
          max(known_stops)
        }
      },
      curtailment_mw = max(curtailment_mw, na.rm = TRUE),
      n_rows = n(),
      .by = c(
        generator,
        generator_name,
        outage_type,
        nature_of_work,
        outage_start
      )
    ) |>
    mutate(
      merge_stop = if_else(
        is.na(outage_stop),
        outage_start,
        outage_stop
      )
    ) |>
    arrange(outage_start, merge_stop) |>
    mutate(
      outage_group = cumsum(
        coalesce(
          as.numeric(outage_start) > lag(cummax(as.numeric(merge_stop))),
          TRUE
        )
      )
    ) |>
    summarise(
      outage_start = min(outage_start),
      outage_stop = {
        known_stops <- outage_stop[!is.na(outage_stop)]
        if (length(known_stops) == 0) {
          as.POSIXct(NA, tz = "UTC")
        } else {
          max(known_stops)
        }
      },
      .by = outage_group
    ) |>
    mutate(
      outage_start,
      outage_stop,
      outage_hours = if_else(
        is.na(outage_stop),
        NA_real_,
        as.numeric(difftime(outage_stop, outage_start, units = "hours"))
      ),
      .keep = "none"
    ) |>
    arrange(outage_start, outage_stop)
}

rtm_bidding_windows <- function(id) {
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
        ) AS bid_start,
        try_strptime(
          SCH_BID_TIMEINTERVALSTOP_GMT,
          '%Y-%m-%dT%H:%M:%S-00:00'
        ) AS bid_stop
      FROM read_parquet({here('CAISO_rtm_big.parquet')})
      WHERE RESOURCEBID_SEQ = {id}
      ORDER BY bid_start
      ",
      id = id,
      .con = con
    )
  ) |>
    as_tibble()
}

rtm_non_bidding_windows <- function(id) {
  rtm_bidding_windows(id) |>
    as_tibble() |>
    mutate(
      gap_start = bid_stop,
      gap_stop = lead(bid_start),
      .keep = "none"
    ) |>
    filter(gap_stop > gap_start) |>
    mutate(
      gap_hours = as.numeric(difftime(gap_stop, gap_start, units = "hours"))
    )
}

outage_rtm <- function(id, generators) {
  bid_starts <- rtm_bidding_windows(id)$bid_start

  generators |>
    set_names() |>
    map_lgl(\(generator) {
      outages <- generator_outages(generator)

      if (nrow(outages) == 0 || length(bid_starts) == 0) {
        return(FALSE)
      }

      starts_before_bid <- outer(outages$outage_start, bid_starts, `<=`)
      stops_after_bid <- outer(
        coalesce(
          outages$outage_stop,
          as.POSIXct("2999-12-31 00:00:00", tz = "UTC")
        ),
        bid_starts,
        `>`
      )

      any(starts_before_bid & stops_after_bid, na.rm = TRUE)
    }) |>
    (\(blocked) generators[!blocked])()
}

# RTM and DAM RESOURCEBID_SEQ are not guaranteed to match.

# 653256 is a 750 MW RTM generator; Ormond is an exact NQC match there.
# Exact capacity match does not mean it survives the outage screen.
outage_rtm(
  653256,
  c("DIABLO_7_UNIT 1", "DIABLO_7_UNIT 2", "ORMOND_7_UNIT 1", "ORMOND_7_UNIT 2")
)

# Mixed case: blocked units drop out, controls stay in.
outage_rtm(
  653256,
  c(
    "DIABLO_7_UNIT 1",
    "ORMOND_7_UNIT 2",
    "ALAMIT_2_AESBT2",
    "BREGGO_6_DSEBT1",
    "CABLRO_2_CBSBT1",
    "CAMERN_6_BSPBT1",
    "CAMERN_6_BSPSR1"
  )
)

generator_outages("ORMOND_7_UNIT 2")
rtm_bidding_windows(653256)
rtm_non_bidding_windows(653256)
