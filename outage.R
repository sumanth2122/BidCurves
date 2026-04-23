library(tidyverse)
library(duckdb)
library(DBI)

outage_rtm <- function(id, generators) {
  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dbExecute(
    con,
    sprintf('PRAGMA threads=%d', max(1L, parallel::detectCores() - 1L))
  )

  generator_values <- paste0(
    '(',
    DBI::dbQuoteString(con, generators),
    ')',
    collapse = ','
  )

  dbGetQuery(
    con,
    glue::glue(
      "
      WITH generators AS (
        SELECT col0 AS generator
        FROM (VALUES {generator_values})
      ),
      bid_windows AS (
        SELECT DISTINCT
          try_strptime(SCH_BID_TIMEINTERVALSTART_GMT, '%Y-%m-%dT%H:%M:%S-00:00') AS bid_start,
          try_strptime(SCH_BID_TIMEINTERVALSTOP_GMT, '%Y-%m-%dT%H:%M:%S-00:00') AS bid_stop
        FROM read_parquet('CAISO_rtm_big.parquet')
        WHERE RESOURCEBID_SEQ = {id}
      ),
      blocked AS (
        SELECT DISTINCT o.\"RESOURCE ID\" AS generator
        FROM read_parquet('prior_trade_day_outages.parquet') o
        JOIN bid_windows b
          ON o.\"CURTAILMENT START DATE TIME\" < b.bid_stop
         AND COALESCE(o.\"CURTAILMENT END DATE TIME\", TIMESTAMP '2999-12-31') > b.bid_start
        WHERE o.\"RESOURCE ID\" IN (SELECT generator FROM generators)
      )
      SELECT g.generator
      FROM generators g
      LEFT JOIN blocked b USING (generator)
      WHERE b.generator IS NULL
      ORDER BY g.generator
      "
    )
  ) |>
    as_tibble()
}

diablo_canyon_usage <- outage_rtm(
  204933,
  c('DIABLO_7_UNIT 1', 'DIABLO_7_UNIT 2')
)
