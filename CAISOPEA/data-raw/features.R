library(dplyr)
library(tidyr)
library(lubridate)
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
  ) |>
  collect()

# Max MW--bid curve or self schedule
safe_max <- \(...) {
  x <- c(...)
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

observed_max <- data |>
  summarize(
    max_mw = safe_max(SCH_BID_XAXISDATA, SELFSCHEDMW),
    .by = RESOURCEBID_SEQ
  )

# Mean and median number of knots as well as frequency of single-point curves
knots <- data |>
  summarise(
    knots = n(),
    .by = c(RESOURCEBID_SEQ, STARTTIME)
  ) |>
  summarise(
    mean_knots = mean(knots),
    median_knots = median(knots),
    pct_single = 100 * mean(knots == 1),
    .by = RESOURCEBID_SEQ
  )

# whether any bids are NULL bids
null <- data |>
  filter(is.na(SELFSCHEDMW)) |>
  group_by(RESOURCEBID_SEQ) |>
  summarise(
    has_null = any((is.na(SCH_BID_XAXISDATA)) & (is.na(SCH_BID_Y1AXISDATA)))
  )

# % of self scheduled bids
self <- data |>
  mutate(self_sched = !is.na(SELFSCHEDMW)) |>
  group_by(RESOURCEBID_SEQ) |>
  summarise(pct_self_sched = 100 * mean(self_sched))

# % of bids per hour
hours_tbl <- data |>
  mutate(hour = hour(ymd_hms(STARTTIME))) |>
  select(RESOURCEBID_SEQ, hour) |>
  group_by(RESOURCEBID_SEQ, hour) |>
  summarise(num_bids = n(), .groups = "drop") |>
  group_by(RESOURCEBID_SEQ) |>
  mutate(pct_per_hour = 100 * num_bids / sum(num_bids)) |>
  select(!num_bids) |>
  pivot_wider(
    names_from = hour,
    values_from = pct_per_hour,
    names_prefix = "hour_",
    values_fill = 0
  )

# whether any bids are negative
negatives <- data |>
  summarize(
    has_negative = any(SCH_BID_XAXISDATA < 0, SELFSCHEDMW < 0, na.rm = TRUE),
    .by = RESOURCEBID_SEQ
  )

# dates of first and last date ID was observed
start_end <- data |>
  select(RESOURCEBID_SEQ, STARTTIME) |>
  mutate(STARTTIME = as.Date(STARTTIME)) |>
  group_by(RESOURCEBID_SEQ) |>
  summarise(start = min(STARTTIME), end = max(STARTTIME), .groups = "drop")

# join
features <- knots |>
  left_join(null, by = "RESOURCEBID_SEQ") |>
  left_join(negatives, by = "RESOURCEBID_SEQ") |>
  left_join(self, by = "RESOURCEBID_SEQ") |>
  left_join(hours_tbl, by = "RESOURCEBID_SEQ") |>
  left_join(start_end, by = "RESOURCEBID_SEQ") |>
  left_join(observed_max, by = "RESOURCEBID_SEQ") |>
  replace_na(list(has_null = TRUE)) |>
  arrow::write_parquet(here::here("ID_attributes.parquet"))

usethis::use_data(features, internal = TRUE, overwrite = TRUE)
