library(tidyverse)
library(duckplyr)
use("here", "here")

count_new_ids_by_day <- function(parquet_path) {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  quoted_path <- as.character(DBI::dbQuoteString(con, parquet_path))
  query <- paste0(
    "WITH first_seen AS (",
    "  SELECT",
    "    RESOURCEBID_SEQ,",
    "    MIN(CAST(STARTTIME AS DATE)) AS first_trade_date",
    "  FROM read_parquet(",
    quoted_path,
    ")",
    "  WHERE RESOURCEBID_SEQ IS NOT NULL",
    "    AND STARTTIME IS NOT NULL",
    "  GROUP BY RESOURCEBID_SEQ",
    "), daily_new AS (",
    "  SELECT",
    "    first_trade_date AS trade_date,",
    "    COUNT(*) AS new_id_count",
    "  FROM first_seen",
    "  GROUP BY first_trade_date",
    ")",
    "SELECT trade_date, new_id_count",
    " FROM daily_new",
    " WHERE new_id_count > 0",
    " ORDER BY trade_date"
  )

  DBI::dbGetQuery(con, query) |>
    mutate(trade_date = as.Date(trade_date))
}

new_ids_by_day <- count_new_ids_by_day(here("CASIO_dam_big.parquet"))
new_ids_by_day |>
  tail(-1) |>
  ggplot(aes(x = trade_date, y = new_id_count)) +
  geom_col(fill = "#2b8cbe", width = 0.8) +
  labs(
    title = "Daily New RESOURCEBID_SEQ",
    x = "Trade Date",
    y = "New IDs"
  ) +
  theme_minimal()

read_bid_data <- function(parquet_path) {
  arrow::read_parquet(
    parquet_path,
    as_data_frame = TRUE,
    col_select = c("RESOURCEBID_SEQ", "SELFSCHEDMW", "STARTTIME")
  ) |>
    mutate(trade_date = as.Date(STARTTIME)) |>
    filter(!is.na(trade_date))
}

map_days <- function(raw_df, fn) {
  trade_dates <- raw_df |>
    distinct(trade_date) |>
    arrange(trade_date) |>
    pull(trade_date)

  results <- trade_dates |>
    map(\(day) {
      raw_df |>
        filter(trade_date == day) |>
        fn()
    })

  names(results) <- as.character(trade_dates)
  results
}

OHE_df <- function(df) {
  max_mw_df <- df |>
    summarise(
      max_mw = {
        vals <- SELFSCHEDMW[!is.na(SELFSCHEDMW)]
        if (length(vals) == 0) NA_real_ else max(vals)
      },
      .by = RESOURCEBID_SEQ
    )

  hours_df <- df |>
    transmute(
      RESOURCEBID_SEQ,
      hour = factor(
        sprintf("hour_%02d", hour(STARTTIME)),
        levels = sprintf("hour_%02d", 0:23)
      ),
      exists = TRUE
    ) |>
    pivot_wider(
      id_cols = RESOURCEBID_SEQ,
      names_from = hour,
      values_from = exists,
      values_fn = list(exists = any),
      values_fill = list(exists = FALSE),
      names_expand = TRUE
    )

  hours_df |>
    left_join(max_mw_df, by = "RESOURCEBID_SEQ") |>
    rename(ID = RESOURCEBID_SEQ, MAX_MW = max_mw) |>
    relocate(MAX_MW, .after = ID)
}

plot_hourly_mw <- function(data, filter_00 = FALSE) {
  data |>
    pivot_longer(
      cols = starts_with("hour_"),
      names_to = "hour",
      values_to = "on"
    ) |>
    mutate(
      hour = factor(hour, levels = sprintf("hour_%02d", 0:23)),
      on = if_else(on, "YES", "NO")
    ) |>
    (\(d) {
      if (filter_00) {
        d <- d |>
          filter(hour != "hour_00")
      }
      d
    })() |>
    group_by(on, hour) |>
    summarise(mw = sum(MAX_MW, na.rm = TRUE), .groups = "drop") |>
    mutate(hour_num = as.integer(sub("hour_", "", as.character(hour)))) |>
    (\(d) {
      max_mw <- max(d$mw, na.rm = TRUE)
      ggplot(d, aes(x = hour_num, y = mw, color = on)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_x_continuous(breaks = 0:23) +
        scale_y_continuous(
          name = "Total MW",
          sec.axis = sec_axis(
            ~ . / max_mw * 100,
            name = "Percent of Total MW",
            labels = scales::label_percent(scale = 1)
          )
        ) +
        labs(x = "Hour", color = NULL) +
        theme_minimal()
    })()
}

summarise_on_off_hourly_mw <- function(df) {
  df |>
    pivot_longer(
      cols = starts_with("hour_"),
      names_to = "hour",
      values_to = "on"
    ) |>
    mutate(
      hour = factor(hour, levels = sprintf("hour_%02d", 0:23)),
      on = if_else(on, "YES", "NO")
    ) |>
    group_by(on, hour) |>
    summarise(mw = sum(MAX_MW, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(
      names_from = hour,
      values_from = mw,
      values_fill = 0
    ) |>
    arrange(desc(on))
}

daily_on_off_tables_from_parquet <- function(parquet_path) {
  raw_df <- read_bid_data(parquet_path)
  map_days(raw_df, \(day_df) day_df |> OHE_df() |> summarise_on_off_hourly_mw())
}

run_hourly_available_mw_anova <- function(df) {
  long_df <- df |>
    pivot_longer(
      cols = starts_with("hour_"),
      names_to = "hour",
      values_to = "on"
    ) |>
    mutate(
      hour = factor(hour, levels = sprintf("hour_%02d", 0:23)),
      available_mw = if_else(on, MAX_MW, 0)
    )

  fit <- aov(available_mw ~ hour, data = long_df)
  fit_tbl <- summary(fit)[[1]]

  tibble(
    test = "anova",
    statistic = unname(fit_tbl[1, "F value"]),
    p_value = unname(fit_tbl[1, "Pr(>F)"]),
    df_between = unname(fit_tbl[1, "Df"]),
    df_within = unname(fit_tbl[2, "Df"]),
    n_resources = n_distinct(df$ID),
    n_obs = nrow(long_df)
  )
}

daily_hourly_anova_from_parquet <- function(parquet_path) {
  raw_df <- read_bid_data(parquet_path)
  day_results <- map_days(raw_df, \(day_df) {
    day_df |> OHE_df() |> run_hourly_available_mw_anova()
  })

  day_results |>
    bind_rows(.id = "trade_date") |>
    mutate(trade_date = as.Date(trade_date), .before = 1) |>
    arrange(trade_date) |>
    mutate(p_value_bh = p.adjust(p_value, method = "BH"))
}

# SEQID 204933 is the nuke?
df <- here("20250701_20250701_PUB_BID_DAM_v3.csv") |>
  read_csv() |>
  OHE_df()

df |> plot_hourly_mw(TRUE)
df |> summarise_on_off_hourly_mw()
df |> run_hourly_available_mw_anova()

# casio_daily_on_off <- daily_on_off_tables_from_parquet(here(
#   "CASIO_dam_big.parquet"
# ))
# casio_daily_hourly_anova <- daily_hourly_anova_from_parquet(here(
#   "CASIO_dam_big.parquet"
# ))

# qs2::qs_save(
#   casio_daily_on_off,
#   here("casio_daily_on_off.qs"),
#   compress_level = 10,
#   nthreads = 16
# )
# qs2::qs_save(
#   casio_daily_hourly_anova,
#   here("casio_daily_hourly_anova.qs"),
#   compress_level = 10,
#   nthreads = 16
# )

casio_daily_on_off <- qs2::qs_read(here("casio_daily_on_off.qs"))
casio_daily_hourly_anova <- qs2::qs_read(here("casio_daily_hourly_anova.qs"))
