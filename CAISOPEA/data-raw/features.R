library(dplyr)
library(tidyr)
library(duckplyr)

data <- read_parquet_duckdb(
  "CAISO_dam_big.parquet",
  prudence = "stingy"
) |>
  filter(
    MARKETPRODUCTTYPE == "EN",
    RESOURCE_TYPE == "GENERATOR",
    !is.na(RESOURCEBID_SEQ)
  ) |>
  select(
    RESOURCEBID_SEQ,
    STARTTIME,
    SCH_BID_XAXISDATA,
    SCH_BID_Y1AXISDATA,
    SELFSCHEDMW
  )

resource_features <- data |>
  summarise(
    max_bid_curve_mw = max(SCH_BID_XAXISDATA, na.rm = TRUE),
    max_self_sched_mw = max(SELFSCHEDMW, na.rm = TRUE),

    has_null = any(
      is.na(SELFSCHEDMW) &
        is.na(SCH_BID_XAXISDATA) &
        is.na(SCH_BID_Y1AXISDATA)
    ),

    has_negative = any(
      (!is.na(SCH_BID_XAXISDATA) & SCH_BID_XAXISDATA < 0) |
        (!is.na(SELFSCHEDMW) & SELFSCHEDMW < 0)
    ),

    pct_self_sched = 100 * sum(as.integer(!is.na(SELFSCHEDMW))) / n(),

    start_time = min(STARTTIME),
    end_time = max(STARTTIME),

    .by = RESOURCEBID_SEQ
  )

knot_features <- data |>
  summarise(
    knots = n(),
    .by = c(RESOURCEBID_SEQ, STARTTIME)
  ) |>
  summarise(
    mean_knots = mean(knots),
    median_knots = median(knots),
    pct_single = 100 * sum(as.integer(knots == 1)) / n(),
    .by = RESOURCEBID_SEQ
  )

hour_features <- data |>
  select(RESOURCEBID_SEQ, STARTTIME) |>
  collect() |>
  mutate(
    hour = as.integer(substr(as.character(STARTTIME), 12, 13))
  ) |>
  summarise(
    hour_00 = 100 * sum(hour == 0) / n(),
    hour_01 = 100 * sum(hour == 1) / n(),
    hour_02 = 100 * sum(hour == 2) / n(),
    hour_03 = 100 * sum(hour == 3) / n(),
    hour_04 = 100 * sum(hour == 4) / n(),
    hour_05 = 100 * sum(hour == 5) / n(),
    hour_06 = 100 * sum(hour == 6) / n(),
    hour_07 = 100 * sum(hour == 7) / n(),
    hour_08 = 100 * sum(hour == 8) / n(),
    hour_09 = 100 * sum(hour == 9) / n(),
    hour_10 = 100 * sum(hour == 10) / n(),
    hour_11 = 100 * sum(hour == 11) / n(),
    hour_12 = 100 * sum(hour == 12) / n(),
    hour_13 = 100 * sum(hour == 13) / n(),
    hour_14 = 100 * sum(hour == 14) / n(),
    hour_15 = 100 * sum(hour == 15) / n(),
    hour_16 = 100 * sum(hour == 16) / n(),
    hour_17 = 100 * sum(hour == 17) / n(),
    hour_18 = 100 * sum(hour == 18) / n(),
    hour_19 = 100 * sum(hour == 19) / n(),
    hour_20 = 100 * sum(hour == 20) / n(),
    hour_21 = 100 * sum(hour == 21) / n(),
    hour_22 = 100 * sum(hour == 22) / n(),
    hour_23 = 100 * sum(hour == 23) / n(),
    .by = RESOURCEBID_SEQ
  )

features <- knot_features |>
  left_join(resource_features, by = "RESOURCEBID_SEQ") |>
  collect() |>
  left_join(hour_features, by = "RESOURCEBID_SEQ") |>
  mutate(
    max_bid_curve_mw = na_if(max_bid_curve_mw, -Inf),
    max_self_sched_mw = na_if(max_self_sched_mw, -Inf),

    max_mw = if_else(
      is.na(max_bid_curve_mw) & is.na(max_self_sched_mw),
      NA_real_,
      pmax(
        replace_na(max_bid_curve_mw, -Inf),
        replace_na(max_self_sched_mw, -Inf)
      )
    ),

    start = as.Date(start_time),
    end = as.Date(end_time)
  ) |>
  select(
    RESOURCEBID_SEQ,
    mean_knots,
    median_knots,
    pct_single,
    has_null,
    has_negative,
    pct_self_sched,
    starts_with("hour_"),
    start,
    end,
    max_mw
  )

hour_cols <- grep("^hour_", names(features), value = TRUE)

cosine_feature_weights <- c(
  mean_knots = 3,
  median_knots = 2,
  pct_single = 4,
  has_null = 4,
  pct_self_sched = 4,
  has_negative = 1,
  max_mw = 1,
  days_active = Inf,
  stats::setNames(
    rep(2 * length(hour_cols), length(hour_cols)),
    hour_cols
  )
) |>
  (\(ranks) 1 / ranks)() |>
  prop.table()

prepare_cosine_matrix <- function(features_df, weights) {
  feature_mat <- features_df |>
    mutate(
      RESOURCEBID_SEQ = as.character(RESOURCEBID_SEQ),
      start = as.Date(start),
      end = as.Date(end),
      days_active = as.numeric(end - start)
    ) |>
    select(
      -RESOURCEBID_SEQ,
      -start,
      -end
    ) |>
    as.matrix()

  storage.mode(feature_mat) <- "double"

  scaled <- scale(feature_mat)

  scaled[is.nan(scaled)] <- 0
  scaled[is.na(scaled)] <- 0

  weights <- weights[colnames(scaled)]

  if (anyNA(weights)) {
    missing <- colnames(scaled)[is.na(weights)]
    stop(
      "weights must include every feature column. Missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  scaled <- sweep(scaled, 2, sqrt(weights), `*`)

  row_norms <- sqrt(rowSums(scaled^2))
  row_norms[row_norms == 0] <- 1

  normed <- scaled / row_norms

  cosine_similarity <- tcrossprod(normed)

  ids <- as.character(features_df$RESOURCEBID_SEQ)
  rownames(cosine_similarity) <- ids
  colnames(cosine_similarity) <- ids

  cosine_similarity
}

cosine_similarity <- prepare_cosine_matrix(
  features_df = features,
  weights = cosine_feature_weights
)

usethis::use_data(
  features,
  cosine_similarity,
  cosine_feature_weights,
  internal = TRUE,
  overwrite = TRUE,
  compress = "xz"
)
