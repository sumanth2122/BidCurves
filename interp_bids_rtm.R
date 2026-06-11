library(tidyverse)
library(duckdb)
library(DBI)
library(progressr)
library(arrow)
library(glue)

con <- dbConnect(duckdb())

# dbExecute(con, "
# COPY (
#   WITH bids AS (
#     SELECT
#       RESOURCEBID_SEQ,
#       STARTTIME,
#       date_part('hour', CAST(STARTTIME AS TIMESTAMP)) AS hour,
#       COALESCE(CAST(SELFSCHEDMW AS DOUBLE), SCH_BID_XAXISDATA) AS mw,
#       SCH_BID_Y1AXISDATA AS price
#     FROM read_parquet('CAISO_rtm_big.parquet')
#     WHERE SCH_BID_Y1AXISDATA IS NOT NULL
#   ),

#   max_mw AS (
#     SELECT
#       RESOURCEBID_SEQ,
#       MAX(mw) AS max_mw
#     FROM bids
#     WHERE mw IS NOT NULL
#     GROUP BY RESOURCEBID_SEQ
#   ),

#   rounded AS (
#     SELECT
#       b.RESOURCEBID_SEQ,
#       CAST(b.STARTTIME AS TIMESTAMP) AS date,
#       b.hour,
#       ROUND((b.mw / m.max_mw) * 100) AS mw_pct_round,
#       AVG(b.price) AS price
#     FROM bids b
#     JOIN max_mw m
#       ON b.RESOURCEBID_SEQ = m.RESOURCEBID_SEQ
#     WHERE b.mw IS NOT NULL
#       AND m.max_mw > 0
#     GROUP BY
#       b.RESOURCEBID_SEQ,
#       CAST(b.STARTTIME AS TIMESTAMP),
#       b.hour,
#       ROUND((b.mw / m.max_mw) * 100)
#   ),

#   pct_grid AS (
#     SELECT range AS pct_grid
#     FROM range(0, 101)
#   ),

#   expanded AS (
#     SELECT
#       r.RESOURCEBID_SEQ,
#       r.date,
#       r.hour,
#       g.pct_grid
#     FROM (
#       SELECT DISTINCT RESOURCEBID_SEQ, date, hour
#       FROM rounded
#     ) r
#     CROSS JOIN pct_grid g
#   ),

#   joined AS (
#     SELECT
#       e.RESOURCEBID_SEQ,
#       e.date,
#       e.hour,
#       e.pct_grid,
#       r.price
#     FROM expanded e
#     LEFT JOIN rounded r
#       ON e.RESOURCEBID_SEQ = r.RESOURCEBID_SEQ
#      AND e.date = r.date
#      AND e.hour = r.hour
#      AND e.pct_grid = r.mw_pct_round
#   ),

#   filled AS (
#     SELECT
#       *,
#       LAST_VALUE(price IGNORE NULLS) OVER (
#         PARTITION BY RESOURCEBID_SEQ, date, hour
#         ORDER BY pct_grid
#         ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
#       ) AS price_interp,
#       MIN(CASE WHEN price IS NOT NULL THEN pct_grid END) OVER (
#         PARTITION BY RESOURCEBID_SEQ, date, hour
#       ) AS min_pct,
#       MAX(CASE WHEN price IS NOT NULL THEN pct_grid END) OVER (
#         PARTITION BY RESOURCEBID_SEQ, date, hour
#       ) AS max_pct
#     FROM joined
#   )

#   SELECT
#     RESOURCEBID_SEQ,
#     date,
#     hour,
#     pct_grid,
#     CASE
#       WHEN pct_grid < min_pct OR pct_grid > max_pct THEN NULL
#       ELSE price_interp
#     END AS price_interp,
#     hour * 100 + pct_grid AS daily_x
#   FROM filled
# )
# TO 'all_resource_interp.parquet'
# (FORMAT PARQUET);
# ")

# interp_tbl <- tbl(con, "read_parquet('all_resource_interp.parquet')")

# get all ids
resource_ids <- tbl(con, "read_parquet('CAISO_rtm_big.parquet')") |>
  distinct(RESOURCEBID_SEQ) |>
  arrange(RESOURCEBID_SEQ) |>
  collect() |>
  pull(RESOURCEBID_SEQ)

# unlink("interp_dataset_rtm", recursive = TRUE)
# dir.create("interp_dataset_rtm")

# empty_ids <- c()

# handlers(global = TRUE)
# handlers("progress")

# with_progress({

#   p <- progressor(along = resource_ids)

#   walk(resource_ids, function(id) {

#     p(sprintf("Processing ID %s", id))

#     id_sql <- dbQuoteLiteral(con, id)

#     query <- glue("
#     WITH bids AS (
#       SELECT
#         RESOURCEBID_SEQ,
#         STARTTIME,
#         CAST(STARTTIME AS TIMESTAMP) AS date,
#         date_part('hour', CAST(STARTTIME AS TIMESTAMP)) AS hour,
#         COALESCE(CAST(SELFSCHEDMW AS DOUBLE), SCH_BID_XAXISDATA) AS mw,
#         SCH_BID_Y1AXISDATA AS price
#       FROM read_parquet('CAISO_rtm_big.parquet')
#       WHERE RESOURCEBID_SEQ = {id_sql}
#     ),

#     max_mw AS (
#       SELECT
#         MAX(mw) AS max_mw
#       FROM bids
#     ),

#     test_pct AS (
#       SELECT
#         RESOURCEBID_SEQ,
#         date,
#         hour,
#         ROUND((mw / max_mw) * 100) AS mw_pct_round,
#         price
#       FROM bids, max_mw
#     ),

#     rounded AS (
#       SELECT
#         RESOURCEBID_SEQ,
#         date,
#         hour,
#         mw_pct_round,
#         AVG(price) AS price
#       FROM test_pct
#       WHERE mw_pct_round IS NOT NULL
#         AND price IS NOT NULL
#       GROUP BY
#         RESOURCEBID_SEQ,
#         date,
#         hour,
#         mw_pct_round
#     ),

#     pct_grid AS (
#       SELECT range AS pct_grid
#       FROM range(0, 101)
#     ),

#     groups AS (
#       SELECT DISTINCT
#         RESOURCEBID_SEQ,
#         date,
#         hour
#       FROM rounded
#     ),

#     expanded AS (
#       SELECT
#         g.RESOURCEBID_SEQ,
#         g.date,
#         g.hour,
#         p.pct_grid
#       FROM groups g
#       CROSS JOIN pct_grid p
#     ),

#     joined AS (
#       SELECT
#         e.RESOURCEBID_SEQ,
#         e.date,
#         e.hour,
#         e.pct_grid,
#         r.price
#       FROM expanded e
#       LEFT JOIN rounded r
#         ON e.RESOURCEBID_SEQ = r.RESOURCEBID_SEQ
#        AND e.date = r.date
#        AND e.hour = r.hour
#        AND e.pct_grid = r.mw_pct_round
#     ),

#     filled AS (
#       SELECT
#         *,
#         LAST_VALUE(price IGNORE NULLS) OVER (
#           PARTITION BY RESOURCEBID_SEQ, date, hour
#           ORDER BY pct_grid
#           ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
#         ) AS price_interp,
#         MIN(CASE WHEN price IS NOT NULL THEN pct_grid END) OVER (
#           PARTITION BY RESOURCEBID_SEQ, date, hour
#         ) AS min_pct,
#         MAX(CASE WHEN price IS NOT NULL THEN pct_grid END) OVER (
#           PARTITION BY RESOURCEBID_SEQ, date, hour
#         ) AS max_pct
#       FROM joined
#     )

#     SELECT
#       RESOURCEBID_SEQ,
#       date,
#       hour,
#       pct_grid,
#       CASE
#         WHEN pct_grid < min_pct OR pct_grid > max_pct THEN NULL
#         ELSE price_interp
#       END AS price_interp,
#       hour * 100 + pct_grid AS daily_x
#     FROM filled
#     ")

#     result <- dbGetQuery(con, query)

#     if (nrow(result) == 0) {
#       empty_ids <<- c(empty_ids, id)
#     } else {
#       outfile <- file.path("interp_dataset_rtm", paste0("id_", id, ".parquet"))
#       write_parquet(result, outfile)
#     }

#   })
# })

# # open_dataset("interp_dataset_rtm")
# interp_tbl <- open_dataset("interp_dataset_rtm")

# interp_tbl |>
#   filter(RESOURCEBID_SEQ == 952796) |>
#   collect()

unlink("interp_dataset_rtm", recursive = TRUE)
dir.create("interp_dataset_rtm")

resource_ids <- tbl(con, "read_parquet('CAISO_rtm_big.parquet')") |>
  filter(
    MARKETPRODUCTTYPE == "EN",
    RESOURCE_TYPE == "GENERATOR",
    !is.na(RESOURCEBID_SEQ)
  ) |>
  distinct(RESOURCEBID_SEQ) |>
  arrange(RESOURCEBID_SEQ) |>
  collect() |>
  pull(RESOURCEBID_SEQ)

empty_ids <- c()
failed_ids <- c()

with_progress({
  p <- progressor(along = resource_ids)

  walk(resource_ids, function(id) {
    p(sprintf("Processing ID %s", id))

    id_sql <- dbQuoteLiteral(con, id)

    outfile <- file.path("interp_dataset_rtm", paste0("id_", id, ".parquet"))

    query <- glue::glue(
      "
      COPY (
        WITH bids AS (
          SELECT
            RESOURCEBID_SEQ,
            STARTTIME,
            CAST(STARTTIME AS TIMESTAMP) AS date,
            date_part('hour', CAST(STARTTIME AS TIMESTAMP)) AS hour,
            COALESCE(CAST(SELFSCHEDMW AS DOUBLE), SCH_BID_XAXISDATA) AS mw,
            SCH_BID_Y1AXISDATA AS price
          FROM read_parquet('CAISO_rtm_big.parquet')
          WHERE RESOURCEBID_SEQ = {id_sql}
        ),

        max_mw AS (
          SELECT MAX(mw) AS max_mw
          FROM bids
        ),

        test_pct AS (
          SELECT
            RESOURCEBID_SEQ,
            date,
            hour,
            ROUND((mw / max_mw) * 100) AS mw_pct_round,
            price
          FROM bids, max_mw
        ),

        rounded AS (
          SELECT
            RESOURCEBID_SEQ,
            date,
            hour,
            mw_pct_round,
            AVG(price) AS price
          FROM test_pct
          WHERE mw_pct_round IS NOT NULL
            AND price IS NOT NULL
          GROUP BY
            RESOURCEBID_SEQ,
            date,
            hour,
            mw_pct_round
        ),

        pct_grid AS (
          SELECT range AS pct_grid
          FROM range(0, 101)
        ),

        groups AS (
          SELECT DISTINCT
            RESOURCEBID_SEQ,
            date,
            hour
          FROM rounded
        ),

        expanded AS (
          SELECT
            g.RESOURCEBID_SEQ,
            g.date,
            g.hour,
            p.pct_grid
          FROM groups g
          CROSS JOIN pct_grid p
        ),

        joined AS (
          SELECT
            e.RESOURCEBID_SEQ,
            e.date,
            e.hour,
            e.pct_grid,
            r.price
          FROM expanded e
          LEFT JOIN rounded r
            ON e.RESOURCEBID_SEQ = r.RESOURCEBID_SEQ
           AND e.date = r.date
           AND e.hour = r.hour
           AND e.pct_grid = r.mw_pct_round
        ),

        filled AS (
          SELECT
            *,
            LAST_VALUE(price IGNORE NULLS) OVER (
              PARTITION BY RESOURCEBID_SEQ, date, hour
              ORDER BY pct_grid
              ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
            ) AS price_interp,
            MIN(CASE WHEN price IS NOT NULL THEN pct_grid END) OVER (
              PARTITION BY RESOURCEBID_SEQ, date, hour
            ) AS min_pct,
            MAX(CASE WHEN price IS NOT NULL THEN pct_grid END) OVER (
              PARTITION BY RESOURCEBID_SEQ, date, hour
            ) AS max_pct
          FROM joined
        )

        SELECT
          RESOURCEBID_SEQ,
          date,
          hour,
          pct_grid,
          CASE
            WHEN pct_grid < min_pct OR pct_grid > max_pct THEN NULL
            ELSE price_interp
          END AS price_interp,
          hour * 100 + pct_grid AS daily_x
        FROM filled
      )
      TO '{outfile}'
      (FORMAT PARQUET, COMPRESSION ZSTD);
    "
    )

    tryCatch(
      {
        dbExecute(con, query)
      },
      error = function(e) {
        failed_ids <<- c(failed_ids, id)
        message("Failed ID: ", id, " | ", e$message)
      }
    )
  })
})

interp_tbl <- arrow::open_dataset("interp_dataset_rtm")

interp_tbl |>
  filter(RESOURCEBID_SEQ == 952796) |>
  collect() |>
  head()
