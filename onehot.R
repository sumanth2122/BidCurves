library(tidyverse)
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

df <- here("20250701_20250701_PUB_BID_DAM_v3.csv") |>
  read_csv() |>
  OHE_df()

# df |>
#   pivot_longer(
#     cols = starts_with("hour_"),
#     names_to = "hour",
#     values_to = "on"
#   ) |>
#   mutate(
#     hour = factor(hour, levels = sprintf("hour_%02d", 0:23)),
#     on = if_else(on, "YES", "NO")
#   ) |>
#   group_by(on, hour) |>
#   summarise(mw = sum(MAX_MW, na.rm = TRUE), .groups = "drop") |>
#   pivot_wider(
#     names_from = hour,
#     values_from = mw,
#     values_fill = 0
#   ) |>
#   arrange(desc(on))

df |> plot_hourly_mw(TRUE)
