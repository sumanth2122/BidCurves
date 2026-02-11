library(tidyverse)
library(duckplyr)
use("here", "here")

# Inspect parquet columns before writing SQL against the file.
inspect_parquet_schema_duckdb <- function(parquet_path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  query <- DBI::sqlInterpolate(
    con,
    "DESCRIBE SELECT * FROM read_parquet(?parquet_path)",
    parquet_path = parquet_path
  )

  DBI::dbGetQuery(con, query)
}

single_point_seq_hours_duckdb <- function(
  parquet_path = here("CASIO_dam_big.parquet"),
  threads = max(1L, parallel::detectCores(logical = TRUE) - 1L)
) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, sprintf("PRAGMA threads = %d", as.integer(threads)))

  schema_query <- DBI::sqlInterpolate(
    con,
    "DESCRIBE SELECT * FROM read_parquet(?parquet_path)",
    parquet_path = parquet_path
  )
  schema_df <- DBI::dbGetQuery(con, schema_query)
  cols <- schema_df$column_name

  required_cols <- c("RESOURCEBID_SEQ", "SCH_BID_XAXISDATA")
  missing_cols <- setdiff(required_cols, cols)
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "Missing required columns in %s: %s",
        parquet_path,
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  interval_candidates <- c(
    "SCH_BID_TIMEINTERVALSTART",
    "TIMEINTERVALSTART",
    "STARTTIME"
  )
  interval_cols <- intersect(interval_candidates, cols)
  if (length(interval_cols) == 0) {
    stop(
      sprintf(
        "Need at least one interval column from: %s",
        paste(interval_candidates, collapse = ", ")
      )
    )
  }

  interval_expr <- sprintf(
    "COALESCE(%s)",
    paste(sprintf("TRY_CAST(%s AS TIMESTAMP)", interval_cols), collapse = ", ")
  )

  has_market_product <- "MARKETPRODUCTTYPE" %in% cols
  has_market_run <- "MARKET_RUN_ID" %in% cols
  has_y1 <- "SCH_BID_Y1AXISDATA" %in% cols

  product_select <- if (has_market_product) {
    ", MARKETPRODUCTTYPE AS market_product_type"
  } else {
    ""
  }

  product_group_select <- if (has_market_product) {
    ", market_product_type"
  } else {
    ""
  }

  product_group_by <- if (has_market_product) ", 4" else ""
  market_filter <- if (has_market_run) "AND MARKET_RUN_ID = 'DAM'" else ""
  product_filter <- if (has_market_product) {
    "AND MARKETPRODUCTTYPE IS NOT NULL"
  } else {
    ""
  }
  y_filter <- if (has_y1) "AND SCH_BID_Y1AXISDATA IS NOT NULL" else ""

  query <- DBI::sqlInterpolate(
    con,
    sprintf(
      "
      WITH base AS (
        SELECT
          CAST(RESOURCEBID_SEQ AS BIGINT) AS seq_id,
          %s AS interval_start,
          SCH_BID_XAXISDATA AS point_x
          %s
        FROM read_parquet(?parquet_path)
        WHERE RESOURCEBID_SEQ IS NOT NULL
          AND SCH_BID_XAXISDATA IS NOT NULL
          %s
          %s
          %s
      ),
      curves AS (
        SELECT
          CAST(interval_start AS DATE) AS trade_date,
          CAST(EXTRACT(HOUR FROM interval_start) AS INTEGER) AS hour_num,
          seq_id
          %s,
          COUNT(DISTINCT point_x) AS n_points
        FROM base
        WHERE interval_start IS NOT NULL
        GROUP BY 1, 2, 3%s
      )
      SELECT
        trade_date,
        hour_num,
        seq_id
      FROM curves
      WHERE n_points = 1
      ORDER BY trade_date, hour_num, seq_id
      ",
      interval_expr,
      product_select,
      y_filter,
      market_filter,
      product_filter,
      product_group_select,
      product_group_by
    ),
    parquet_path = parquet_path
  )

  DBI::dbGetQuery(con, query) |>
    mutate(
      trade_date = as.Date(trade_date),
      hour_num = as.integer(hour_num),
      seq_id = as.integer(seq_id)
    )
}

count_single_point_unique_generators <- function(single_point_seq_hours_df) {
  single_point_seq_hours_df |>
    summarise(n_single_point_generators = n_distinct(seq_id))
}

count_total_unique_generators_duckdb <- function(
  parquet_path = here("CASIO_dam_big.parquet"),
  threads = max(1L, parallel::detectCores(logical = TRUE) - 1L)
) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, sprintf("PRAGMA threads = %d", as.integer(threads)))

  query <- DBI::sqlInterpolate(
    con,
    "
    SELECT
      COUNT(DISTINCT CAST(RESOURCEBID_SEQ AS BIGINT)) AS n_total_generators
    FROM read_parquet(?parquet_path)
    WHERE RESOURCEBID_SEQ IS NOT NULL
    ",
    parquet_path = parquet_path
  )

  DBI::dbGetQuery(con, query) |>
    mutate(n_total_generators = as.integer(n_total_generators))
}

count_single_point_curves_by_hour <- function(single_point_curves_df) {
  single_point_curves_df |>
    count(hour_num, name = "n_single_point_curves") |>
    complete(hour_num = 0:23, fill = list(n_single_point_curves = 0L)) |>
    arrange(hour_num)
}

plot_single_point_seqid_histogram <- function(single_point_curves_df) {
  count_single_point_curves_by_hour(single_point_curves_df) |>
    ggplot(aes(x = hour_num, y = n_single_point_curves)) +
    geom_col(fill = "#2f6da3", color = "white") +
    scale_x_continuous(breaks = 0:23, limits = c(-0.5, 23.5)) +
    labs(
      x = "Hour",
      y = "Single-point bid curves",
      title = "Single-Point Bid Curves by Hour"
    ) +
    theme_minimal()
}

bid_curve_point_distribution_duckdb <- function(
  parquet_path = here("CASIO_dam_big.parquet"),
  threads = max(1L, parallel::detectCores(logical = TRUE) - 1L),
  curve_level = FALSE
) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, sprintf("PRAGMA threads = %d", as.integer(threads)))

  schema_query <- DBI::sqlInterpolate(
    con,
    "DESCRIBE SELECT * FROM read_parquet(?parquet_path)",
    parquet_path = parquet_path
  )
  schema_df <- DBI::dbGetQuery(con, schema_query)
  cols <- schema_df$column_name

  required_cols <- c("RESOURCEBID_SEQ", "SCH_BID_XAXISDATA")
  missing_cols <- setdiff(required_cols, cols)
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "Missing required columns in %s: %s",
        parquet_path,
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  interval_candidates <- c(
    "SCH_BID_TIMEINTERVALSTART",
    "TIMEINTERVALSTART",
    "STARTTIME"
  )
  interval_cols <- intersect(interval_candidates, cols)
  if (length(interval_cols) == 0) {
    stop(
      sprintf(
        "Need at least one interval column from: %s",
        paste(interval_candidates, collapse = ", ")
      )
    )
  }

  interval_expr <- sprintf(
    "COALESCE(%s)",
    paste(sprintf("TRY_CAST(%s AS TIMESTAMP)", interval_cols), collapse = ", ")
  )

  has_market_run <- "MARKET_RUN_ID" %in% cols
  has_y1 <- "SCH_BID_Y1AXISDATA" %in% cols

  optional_dims <- c()
  base_optional_select <- c()
  curve_optional_select <- c()

  if ("MARKETPRODUCTTYPE" %in% cols) {
    optional_dims <- c(optional_dims, "market_product_type")
    base_optional_select <- c(
      base_optional_select,
      "MARKETPRODUCTTYPE AS market_product_type"
    )
    curve_optional_select <- c(curve_optional_select, "market_product_type")
  }
  if ("SCH_BID_CURVETYPE" %in% cols) {
    optional_dims <- c(optional_dims, "curve_type")
    base_optional_select <- c(
      base_optional_select,
      "SCH_BID_CURVETYPE AS curve_type"
    )
    curve_optional_select <- c(curve_optional_select, "curve_type")
  }
  if ("PRODUCTBID_MRID" %in% cols) {
    optional_dims <- c(optional_dims, "product_bid_mrid")
    base_optional_select <- c(
      base_optional_select,
      "CAST(PRODUCTBID_MRID AS VARCHAR) AS product_bid_mrid"
    )
    curve_optional_select <- c(curve_optional_select, "product_bid_mrid")
  }

  base_optional_sql <- if (length(base_optional_select) > 0) {
    paste0(
      ",\n          ",
      paste(base_optional_select, collapse = ",\n          ")
    )
  } else {
    ""
  }

  curve_optional_sql <- if (length(curve_optional_select) > 0) {
    paste0(
      ",\n          ",
      paste(curve_optional_select, collapse = ",\n          ")
    )
  } else {
    ""
  }

  group_by_sql <- paste(seq_len(3 + length(optional_dims)), collapse = ", ")
  market_filter <- if (has_market_run) "AND MARKET_RUN_ID = 'DAM'" else ""
  y_filter <- if (has_y1) "AND SCH_BID_Y1AXISDATA IS NOT NULL" else ""

  query <- DBI::sqlInterpolate(
    con,
    sprintf(
      "
      WITH base AS (
        SELECT
          CAST(RESOURCEBID_SEQ AS BIGINT) AS seq_id,
          %s AS interval_start,
          SCH_BID_XAXISDATA AS point_x
          %s
        FROM read_parquet(?parquet_path)
        WHERE RESOURCEBID_SEQ IS NOT NULL
          AND SCH_BID_XAXISDATA IS NOT NULL
          %s
          %s
      ),
      curves AS (
        SELECT
          CAST(interval_start AS DATE) AS trade_date,
          CAST(EXTRACT(HOUR FROM interval_start) AS INTEGER) AS hour_num,
          seq_id
          %s,
          COUNT(DISTINCT point_x) AS n_points
        FROM base
        WHERE interval_start IS NOT NULL
        GROUP BY %s
      )
      SELECT
        trade_date,
        hour_num,
        seq_id
        %s,
        n_points
      FROM curves
      ORDER BY trade_date, hour_num, seq_id
      ",
      interval_expr,
      base_optional_sql,
      y_filter,
      market_filter,
      curve_optional_sql,
      group_by_sql,
      curve_optional_sql
    ),
    parquet_path = parquet_path
  )

  curve_counts <- DBI::dbGetQuery(con, query) |>
    mutate(
      trade_date = as.Date(trade_date),
      hour_num = as.integer(hour_num),
      seq_id = as.integer(seq_id),
      n_points = as.integer(n_points)
    )

  if (curve_level) {
    return(curve_counts)
  }

  curve_counts |>
    count(n_points, name = "n_curves") |>
    arrange(n_points) |>
    mutate(pct_curves = n_curves / sum(n_curves))
}

count_bid_curves_over_pair_limit_duckdb <- function(
  parquet_path = here("CASIO_dam_big.parquet"),
  pair_limit = 11L,
  threads = max(1L, parallel::detectCores(logical = TRUE) - 1L)
) {
  curve_counts <- bid_curve_point_distribution_duckdb(
    parquet_path = parquet_path,
    threads = threads,
    curve_level = TRUE
  )

  overall <- curve_counts |>
    summarise(
      pair_limit = as.integer(pair_limit),
      total_curves = n(),
      n_over_pair_limit = sum(n_points > pair_limit),
      pct_over_pair_limit = if_else(
        total_curves > 0,
        n_over_pair_limit / total_curves,
        NA_real_
      )
    )

  by_hour <- curve_counts |>
    group_by(hour_num) |>
    summarise(
      total_curves = n(),
      n_over_pair_limit = sum(n_points > pair_limit),
      pct_over_pair_limit = if_else(
        total_curves > 0,
        n_over_pair_limit / total_curves,
        NA_real_
      ),
      .groups = "drop"
    ) |>
    complete(
      hour_num = 0:23,
      fill = list(
        total_curves = 0L,
        n_over_pair_limit = 0L,
        pct_over_pair_limit = 0
      )
    ) |>
    mutate(
      hour_num = as.integer(hour_num),
      pair_limit = as.integer(pair_limit)
    ) |>
    arrange(hour_num)

  list(
    overall = overall,
    by_hour = by_hour
  )
}

count_nonmonotonic_bid_curves_duckdb <- function(
  parquet_path = here("CASIO_dam_big.parquet"),
  threads = max(1L, parallel::detectCores(logical = TRUE) - 1L),
  curve_level = FALSE
) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, sprintf("PRAGMA threads = %d", as.integer(threads)))

  schema_query <- DBI::sqlInterpolate(
    con,
    "DESCRIBE SELECT * FROM read_parquet(?parquet_path)",
    parquet_path = parquet_path
  )
  schema_df <- DBI::dbGetQuery(con, schema_query)
  cols <- schema_df$column_name

  required_cols <- c(
    "RESOURCEBID_SEQ",
    "SCH_BID_XAXISDATA",
    "SCH_BID_Y1AXISDATA"
  )
  missing_cols <- setdiff(required_cols, cols)
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "Missing required columns in %s: %s",
        parquet_path,
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  interval_candidates <- c(
    "SCH_BID_TIMEINTERVALSTART",
    "TIMEINTERVALSTART",
    "STARTTIME"
  )
  interval_cols <- intersect(interval_candidates, cols)
  if (length(interval_cols) == 0) {
    stop(
      sprintf(
        "Need at least one interval column from: %s",
        paste(interval_candidates, collapse = ", ")
      )
    )
  }

  interval_expr <- sprintf(
    "COALESCE(%s)",
    paste(sprintf("TRY_CAST(%s AS TIMESTAMP)", interval_cols), collapse = ", ")
  )

  has_market_run <- "MARKET_RUN_ID" %in% cols
  optional_dims <- c()
  base_optional_select <- c()

  if ("MARKETPRODUCTTYPE" %in% cols) {
    optional_dims <- c(optional_dims, "market_product_type")
    base_optional_select <- c(
      base_optional_select,
      "MARKETPRODUCTTYPE AS market_product_type"
    )
  }
  if ("SCH_BID_CURVETYPE" %in% cols) {
    optional_dims <- c(optional_dims, "curve_type")
    base_optional_select <- c(
      base_optional_select,
      "SCH_BID_CURVETYPE AS curve_type"
    )
  }
  if ("PRODUCTBID_MRID" %in% cols) {
    optional_dims <- c(optional_dims, "product_bid_mrid")
    base_optional_select <- c(
      base_optional_select,
      "CAST(PRODUCTBID_MRID AS VARCHAR) AS product_bid_mrid"
    )
  }

  base_optional_sql <- if (length(base_optional_select) > 0) {
    paste0(
      ",\n          ",
      paste(base_optional_select, collapse = ",\n          ")
    )
  } else {
    ""
  }

  curve_key_cols <- c("trade_date", "hour_num", "seq_id", optional_dims)
  curve_key_group <- paste(seq_along(curve_key_cols), collapse = ", ")
  curve_key_partition <- paste(curve_key_cols, collapse = ", ")
  curve_key_select <- if (length(optional_dims) > 0) {
    paste0(",\n        ", paste(optional_dims, collapse = ",\n        "))
  } else {
    ""
  }
  market_filter <- if (has_market_run) "AND MARKET_RUN_ID = 'DAM'" else ""

  query <- DBI::sqlInterpolate(
    con,
    sprintf(
      "
      WITH base AS (
        SELECT
          CAST(RESOURCEBID_SEQ AS BIGINT) AS seq_id,
          %s AS interval_start,
          SCH_BID_XAXISDATA AS point_x,
          SCH_BID_Y1AXISDATA AS point_y
          %s
        FROM read_parquet(?parquet_path)
        WHERE RESOURCEBID_SEQ IS NOT NULL
          AND SCH_BID_XAXISDATA IS NOT NULL
          AND SCH_BID_Y1AXISDATA IS NOT NULL
          %s
      ),
      points_by_x AS (
        SELECT
          CAST(interval_start AS DATE) AS trade_date,
          CAST(EXTRACT(HOUR FROM interval_start) AS INTEGER) AS hour_num,
          seq_id
          %s,
          point_x,
          MAX(point_y) AS point_y
        FROM base
        WHERE interval_start IS NOT NULL
        GROUP BY %s, %s
      ),
      point_steps AS (
        SELECT
          *,
          LAG(point_y) OVER (
            PARTITION BY %s
            ORDER BY point_x
          ) AS prev_y
        FROM points_by_x
      ),
      curve_flags AS (
        SELECT
          trade_date,
          hour_num,
          seq_id
          %s,
          COUNT(*) AS n_points,
          SUM(CASE WHEN prev_y IS NOT NULL AND point_y < prev_y THEN 1 ELSE 0 END) AS n_down_steps
        FROM point_steps
        GROUP BY %s
      )
      SELECT
        trade_date,
        hour_num,
        seq_id
        %s,
        n_points,
        n_down_steps,
        (n_down_steps > 0) AS is_nonmonotonic
      FROM curve_flags
      ORDER BY trade_date, hour_num, seq_id
      ",
      interval_expr,
      base_optional_sql,
      market_filter,
      curve_key_select,
      curve_key_group,
      length(curve_key_cols) + 1L,
      curve_key_partition,
      curve_key_select,
      curve_key_group,
      curve_key_select
    ),
    parquet_path = parquet_path
  )

  curve_flags <- DBI::dbGetQuery(con, query) |>
    mutate(
      trade_date = as.Date(trade_date),
      hour_num = as.integer(hour_num),
      seq_id = as.integer(seq_id),
      n_points = as.integer(n_points),
      n_down_steps = as.integer(n_down_steps),
      is_nonmonotonic = as.logical(is_nonmonotonic)
    )

  if (curve_level) {
    return(curve_flags)
  }

  overall <- curve_flags |>
    summarise(
      total_curves = n(),
      n_nonmonotonic = sum(is_nonmonotonic, na.rm = TRUE),
      pct_nonmonotonic = if_else(
        total_curves > 0,
        n_nonmonotonic / total_curves,
        NA_real_
      )
    )

  by_hour <- curve_flags |>
    group_by(hour_num) |>
    summarise(
      total_curves = n(),
      n_nonmonotonic = sum(is_nonmonotonic, na.rm = TRUE),
      pct_nonmonotonic = if_else(
        total_curves > 0,
        n_nonmonotonic / total_curves,
        NA_real_
      ),
      .groups = "drop"
    ) |>
    complete(
      hour_num = 0:23,
      fill = list(
        total_curves = 0L,
        n_nonmonotonic = 0L,
        pct_nonmonotonic = 0
      )
    ) |>
    mutate(hour_num = as.integer(hour_num)) |>
    arrange(hour_num)

  list(
    overall = overall,
    by_hour = by_hour
  )
}

single_point_seq_hours <- single_point_seq_hours_duckdb(here(
  "CASIO_dam_big.parquet"
))
single_point_unique_generator_count <- count_single_point_unique_generators(
  single_point_seq_hours
)
single_point_curve_count <- single_point_seq_hours |>
  summarise(n_single_point_curves = n())
total_generator_count <- count_total_unique_generators_duckdb(
  here("CASIO_dam_big.parquet")
)
single_point_generator_summary <- tibble(
  n_single_point_curves = single_point_curve_count$n_single_point_curves,
  n_single_point_generators = single_point_unique_generator_count$n_single_point_generators,
  n_total_generators = total_generator_count$n_total_generators
) |>
  mutate(
    pct_single_point_generators = if_else(
      n_total_generators > 0,
      n_single_point_generators / n_total_generators,
      NA_real_
    )
  )
print(single_point_generator_summary)
plot_single_point_seqid_histogram(single_point_seq_hours)

point_dist <- bid_curve_point_distribution_duckdb(here("CASIO_dam_big.parquet"))
point_dist |>
  ggplot(aes(x = n_points, y = n_curves)) +
  geom_col(fill = "#2f6da3") +
  labs(
    title = "Bid Curve Point Distribution",
    x = "Number of points in curve",
    y = "Number of curves"
  ) +
  theme_minimal()

point_dist |>
  filter(n_points > 11) |>
  ggplot(aes(x = n_points, y = n_curves)) +
  geom_col(fill = "#d95f02") +
  labs(
    title = "Bid Curve Point Distribution (n_points > 11)",
    x = "Number of points in curve",
    y = "Number of curves"
  ) +
  theme_minimal()

over_11 <- count_bid_curves_over_pair_limit_duckdb(
  here("CASIO_dam_big.parquet"),
  pair_limit = 11
)
print(over_11$overall)

nonmono <- count_nonmonotonic_bid_curves_duckdb(here("CASIO_dam_big.parquet"))
print(nonmono$overall)
