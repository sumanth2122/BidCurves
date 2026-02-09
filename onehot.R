library(tidyverse)
library(duckplyr)
use("here", "here")

OHE_df <- function(df) {
  max_mw_df <- df |>
    summarise(
      max_mw = max(SELFSCHEDMW, na.rm = TRUE),
      .by = RESOURCEBID_SEQ
    ) |>
    mutate(max_mw = if_else(is.infinite(max_mw), NA_real_, max_mw)) # all-NA -> NA

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
  hour_stats <- read_daily_hour_stats_duckdb(parquet_path)
  daily_on_off_tables_from_hour_stats(hour_stats)
}

read_daily_hour_stats_duckdb <- function(
  parquet_path,
  threads = max(1L, parallel::detectCores(logical = TRUE) - 1L)
) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, sprintf("PRAGMA threads = %d", as.integer(threads)))

  query <- DBI::sqlInterpolate(
    con,
    "
    WITH base AS (
      SELECT
        CAST(STARTTIME AS DATE) AS trade_date,
        RESOURCEBID_SEQ AS id,
        CAST(EXTRACT(HOUR FROM STARTTIME) AS INTEGER) AS hour_num,
        SELFSCHEDMW
      FROM read_parquet(?parquet_path)
      WHERE STARTTIME IS NOT NULL
    ),
    resource_day AS (
      SELECT
        trade_date,
        id,
        MAX(SELFSCHEDMW) AS max_mw
      FROM base
      GROUP BY 1, 2
    ),
    resource_day_hour AS (
      SELECT DISTINCT
        trade_date,
        id,
        hour_num
      FROM base
    ),
    day_totals AS (
      SELECT
        trade_date,
        COALESCE(SUM(max_mw), 0.0) AS total_mw,
        COUNT(*) AS n_resources
      FROM resource_day
      GROUP BY 1
    ),
    yes_stats AS (
      SELECT
        h.trade_date,
        h.hour_num,
        SUM(r.max_mw) AS yes_mw,
        SUM(r.max_mw * r.max_mw) AS yes_mw_sq,
        SUM(CASE WHEN r.max_mw IS NULL THEN 1 ELSE 0 END) AS on_na_count
      FROM resource_day_hour h
      JOIN resource_day r
        ON h.trade_date = r.trade_date
       AND h.id = r.id
      GROUP BY 1, 2
    ),
    days AS (
      SELECT DISTINCT
        trade_date
      FROM day_totals
    ),
    hours AS (
      SELECT
        range AS hour_num
      FROM range(24)
    )
    SELECT
      d.trade_date,
      h.hour_num,
      COALESCE(y.yes_mw, 0.0) AS yes_mw,
      COALESCE(y.yes_mw_sq, 0.0) AS yes_mw_sq,
      t.total_mw,
      t.n_resources,
      t.n_resources - COALESCE(y.on_na_count, 0) AS n_obs_hour
    FROM days d
    CROSS JOIN hours h
    JOIN day_totals t
      ON d.trade_date = t.trade_date
    LEFT JOIN yes_stats y
      ON d.trade_date = y.trade_date
     AND h.hour_num = y.hour_num
    ORDER BY d.trade_date, h.hour_num
    ",
    parquet_path = parquet_path
  )

  DBI::dbGetQuery(con, query) |>
    mutate(
      trade_date = as.Date(trade_date),
      hour_num = as.integer(hour_num),
      n_resources = as.integer(n_resources),
      n_obs_hour = as.integer(n_obs_hour)
    )
}

daily_on_off_tables_from_hour_stats <- function(hour_stats_df) {
  hour_stats_df |>
    transmute(
      trade_date,
      hour = factor(
        sprintf("hour_%02d", hour_num),
        levels = sprintf("hour_%02d", 0:23)
      ),
      YES = yes_mw,
      NO = total_mw - yes_mw
    ) |>
    pivot_longer(
      cols = c(YES, NO),
      names_to = "on",
      values_to = "mw"
    ) |>
    split(x = _, f = .$trade_date) |>
    map(\(day_df) {
      day_df |>
        select(on, hour, mw) |>
        pivot_wider(
          names_from = hour,
          values_from = mw,
          values_fill = 0
        ) |>
        arrange(desc(on))
    })
}

run_hourly_available_mw_anova <- function(df, alpha = 0.05) {
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
  tukey_tbl <- TukeyHSD(fit, "hour")$hour |>
    as.data.frame() |>
    rownames_to_column("contrast") |>
    as_tibble() |>
    separate(contrast, into = c("hour_1", "hour_2"), sep = "-") |>
    rename(
      diff_mw = diff,
      conf_low = lwr,
      conf_high = upr,
      p_adj = `p adj`
    ) |>
    filter(p_adj < alpha) |>
    arrange(p_adj)

  different_hours <- tukey_tbl |>
    select(hour_1, hour_2) |>
    unlist(use.names = FALSE) |>
    unique() |>
    sort()

  tibble(
    test = "anova",
    statistic = unname(fit_tbl[1, "F value"]),
    p_value = unname(fit_tbl[1, "Pr(>F)"]),
    df_between = unname(fit_tbl[1, "Df"]),
    df_within = unname(fit_tbl[2, "Df"]),
    n_resources = n_distinct(df$ID),
    n_obs = nrow(long_df),
    n_significant_pairs = nrow(tukey_tbl),
    different_hours = list(different_hours),
    significant_pairs = list(tukey_tbl)
  )
}

daily_hourly_anova_from_parquet <- function(parquet_path) {
  hour_stats <- read_daily_hour_stats_duckdb(parquet_path)
  daily_hourly_anova_from_hour_stats(hour_stats)
}

daily_hourly_anova_from_hour_stats <- function(hour_stats_df, alpha = 0.05) {
  anova_core <- hour_stats_df |>
    group_by(trade_date) |>
    summarise(
      k = sum(n_obs_hour > 0),
      n_obs = sum(n_obs_hour),
      total_sum = sum(yes_mw),
      ss_between_raw = sum(if_else(n_obs_hour > 0, yes_mw^2 / n_obs_hour, 0)),
      ss_within = sum(
        if_else(
          n_obs_hour > 0,
          yes_mw_sq - (yes_mw^2 / n_obs_hour),
          0
        )
      ),
      n_resources = first(n_resources),
      .groups = "drop"
    ) |>
    mutate(
      ss_between = ss_between_raw - (total_sum^2 / n_obs),
      df_between = k - 1,
      df_within = n_obs - k,
      ms_between = ss_between / df_between,
      ms_within = ss_within / df_within,
      statistic = if_else(
        df_between > 0 & df_within > 0 & ms_within > 0,
        ms_between / ms_within,
        NA_real_
      ),
      p_value = if_else(
        is.na(statistic),
        NA_real_,
        pf(statistic, df_between, df_within, lower.tail = FALSE)
      ),
      test = "anova"
    )

  pairwise_sig <- hour_stats_df |>
    filter(n_obs_hour > 0) |>
    transmute(
      trade_date,
      hour_num,
      hour = sprintf("hour_%02d", hour_num),
      n_obs_hour,
      mean_mw = yes_mw / n_obs_hour
    ) |>
    inner_join(
      hour_stats_df |>
        filter(n_obs_hour > 0) |>
        transmute(
          trade_date,
          hour_num,
          hour = sprintf("hour_%02d", hour_num),
          n_obs_hour,
          mean_mw = yes_mw / n_obs_hour
        ),
      by = "trade_date",
      suffix = c("_1", "_2")
    ) |>
    filter(hour_num_1 < hour_num_2) |>
    left_join(
      anova_core |>
        select(trade_date, k, df_within, ms_within),
      by = "trade_date"
    ) |>
    mutate(
      diff_mw = mean_mw_1 - mean_mw_2,
      se = sqrt((ms_within / 2) * ((1 / n_obs_hour_1) + (1 / n_obs_hour_2))),
      q_stat = if_else(se > 0, abs(diff_mw) / se, NA_real_),
      p_adj = if_else(
        !is.na(q_stat) & k > 1 & df_within > 0,
        ptukey(q_stat, nmeans = k, df = df_within, lower.tail = FALSE),
        NA_real_
      ),
      q_crit = if_else(
        k > 1 & df_within > 0,
        qtukey(1 - alpha, nmeans = k, df = df_within),
        NA_real_
      ),
      half_width = q_crit * se,
      conf_low = diff_mw - half_width,
      conf_high = diff_mw + half_width
    ) |>
    filter(!is.na(p_adj), p_adj < alpha) |>
    transmute(
      trade_date,
      hour_1,
      hour_2,
      diff_mw,
      conf_low,
      conf_high,
      p_adj
    ) |>
    arrange(trade_date, p_adj)

  pairwise_by_day <- pairwise_sig |>
    group_by(trade_date) |>
    summarise(
      n_significant_pairs = n(),
      different_hours = list(sort(unique(c(hour_1, hour_2)))),
      significant_pairs = list(
        tibble(
          hour_1 = hour_1,
          hour_2 = hour_2,
          diff_mw = diff_mw,
          conf_low = conf_low,
          conf_high = conf_high,
          p_adj = p_adj
        )
      ),
      .groups = "drop"
    )

  empty_pairs <- tibble(
    hour_1 = character(),
    hour_2 = character(),
    diff_mw = numeric(),
    conf_low = numeric(),
    conf_high = numeric(),
    p_adj = numeric()
  )

  anova_core |>
    transmute(
      trade_date,
      test,
      statistic,
      p_value,
      df_between,
      df_within,
      n_resources,
      n_obs
    ) |>
    left_join(pairwise_by_day, by = "trade_date") |>
    mutate(
      n_significant_pairs = coalesce(n_significant_pairs, 0L),
      different_hours = map(
        different_hours,
        \(x) {
          if (is.null(x)) character() else x
        }
      ),
      significant_pairs = map(
        significant_pairs,
        \(x) {
          if (is.null(x)) empty_pairs else x
        }
      )
    ) |>
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

build_casio_daily_outputs <- function(
  parquet_path = here("CASIO_dam_big.parquet"),
  threads = max(1L, parallel::detectCores(logical = TRUE) - 1L)
) {
  hour_stats <- read_daily_hour_stats_duckdb(
    parquet_path = parquet_path,
    threads = threads
  )

  out <- list(
    casio_daily_on_off = daily_on_off_tables_from_hour_stats(hour_stats),
    casio_daily_hourly_anova = daily_hourly_anova_from_hour_stats(hour_stats)
  )

  rm(hour_stats)
  gc(verbose = FALSE)
  out
}
