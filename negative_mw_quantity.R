library(tidyverse)

default_casio_parquet_path <- function() {
  if (requireNamespace("here", quietly = TRUE)) {
    return(here::here("CASIO_dam_big.parquet"))
  }
  "CASIO_dam_big.parquet"
}

negative_mw_quantity_summary_duckdb <- function(
  parquet_path = default_casio_parquet_path(),
  market_run_id = NULL,
  threads = max(1L, parallel::detectCores(logical = TRUE) - 1L)
) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package `DBI` is required. Install with install.packages('DBI').")
  }
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop(
      "Package `duckdb` is required. Install with install.packages('duckdb')."
    )
  }

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, sprintf("PRAGMA threads = %d", as.integer(threads)))

  # sqlInterpolate() cannot quote raw NULL; use NA -> SQL NULL.
  market_run_id_sql <- if (is.null(market_run_id)) {
    NA_character_
  } else {
    market_run_id
  }

  query <- DBI::sqlInterpolate(
    con,
    "
    WITH points_base AS (
      SELECT
        CAST(RESOURCEBID_SEQ AS BIGINT) AS seq_id,
        TRY_CAST(STARTTIME AS TIMESTAMP) AS start_ts,
        TRY_CAST(SCH_BID_XAXISDATA AS DOUBLE) AS point_mw
      FROM read_parquet(?parquet_path)
      WHERE RESOURCEBID_SEQ IS NOT NULL
        AND SCH_BID_XAXISDATA IS NOT NULL
        AND (?market_run_id IS NULL OR MARKET_RUN_ID = ?market_run_id)
    ),
    curves AS (
      SELECT
        CAST(start_ts AS DATE) AS trade_date,
        CAST(EXTRACT(HOUR FROM start_ts) AS INTEGER) AS hour_num,
        seq_id,
        MAX(CASE WHEN point_mw < 0 THEN 1 ELSE 0 END) AS has_negative_mw
      FROM points_base
      WHERE start_ts IS NOT NULL
      GROUP BY 1, 2, 3
    ),
    point_totals AS (
      SELECT
        COUNT(*) AS n_bid_points,
        SUM(CASE WHEN point_mw < 0 THEN 1 ELSE 0 END) AS n_negative_bid_points,
        COUNT(DISTINCT seq_id) AS n_ids,
        COUNT(DISTINCT CASE WHEN point_mw < 0 THEN seq_id END) AS n_ids_with_negative_mw
      FROM points_base
    ),
    curve_totals AS (
      SELECT
        COUNT(*) AS n_curves,
        SUM(has_negative_mw) AS n_curves_with_negative_mw
      FROM curves
    )
    SELECT
      p.n_bid_points,
      p.n_negative_bid_points,
      p.n_ids,
      p.n_ids_with_negative_mw,
      c.n_curves,
      c.n_curves_with_negative_mw
    FROM point_totals p
    CROSS JOIN curve_totals c
    ",
    parquet_path = parquet_path,
    market_run_id = market_run_id_sql
  )

  DBI::dbGetQuery(con, query) |>
    mutate(
      across(
        c(
          n_bid_points,
          n_negative_bid_points,
          n_ids,
          n_ids_with_negative_mw,
          n_curves,
          n_curves_with_negative_mw
        ),
        ~ as.integer(replace_na(., 0))
      ),
      pct_negative_bid_points = if_else(
        n_bid_points > 0,
        n_negative_bid_points / n_bid_points,
        NA_real_
      ),
      pct_ids_with_negative_mw = if_else(
        n_ids > 0,
        n_ids_with_negative_mw / n_ids,
        NA_real_
      ),
      pct_curves_with_negative_mw = if_else(
        n_curves > 0,
        n_curves_with_negative_mw / n_curves,
        NA_real_
      )
    )
}

negative_mw_sentence <- function(summary_df, digits = 2) {
  sprintf(
    "Negative MW quantity appears in %s of bid curves and %s of generators (RESOURCEBID_SEQ).",
    scales::percent(
      summary_df$pct_curves_with_negative_mw[[1]],
      accuracy = 10^-digits
    ),
    scales::percent(
      summary_df$pct_ids_with_negative_mw[[1]],
      accuracy = 10^-digits
    )
  )
}

negative_mw_summary <- negative_mw_quantity_summary_duckdb(
  parquet_path = default_casio_parquet_path()
)

print(negative_mw_summary)
cat(negative_mw_sentence(negative_mw_summary), "\n")
