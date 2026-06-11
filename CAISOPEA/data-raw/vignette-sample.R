library(dplyr)
library(readr)

final_matches <- read_csv(
  here::here(
    "final_outputs",
    "curtailment_matches_final.csv"
  ),
  show_col_types = FALSE
) |>
  arrange(desc(n_matches), desc(confidence), generator) |>
  slice_head(n = 20)

selected_resources <- final_matches$generator

match_detail <- read_csv(
  here::here(
    "final_outputs",
    "curtailment_match_detail.csv"
  ),
  show_col_types = FALSE
) |>
  filter(generator %in% selected_resources)

id_attrs <- arrow::read_parquet("ID_attributes.parquet") |>
  semi_join(final_matches, by = "RESOURCEBID_SEQ")

outages_raw <- arrow::read_parquet("prior_trade_day_outages.parquet")

curtailment_outages <- outages_raw |>
  mutate(
    curtailment_date = as.Date(`CURTAILMENT START DATE TIME`),
    expected_available_mw = `RESOURCE PMAX MW` - `CURTAILMENT MW`
  ) |>
  inner_join(
    match_detail |>
      distinct(
        generator,
        curtailment_date,
        expected_available_mw
      ),
    by = c(
      "RESOURCE ID" = "generator",
      "curtailment_date",
      "expected_available_mw"
    )
  ) |>
  filter(
    `OUTAGE TYPE` == "PLANNED",
    `RESOURCE PMAX MW` > 0,
    `CURTAILMENT MW` < `RESOURCE PMAX MW`
  ) |>
  select(-curtailment_date, -expected_available_mw)

full_outage_events <- outages_raw |>
  filter(
    `RESOURCE ID` %in% selected_resources,
    `RESOURCE PMAX MW` > 0,
    `CURTAILMENT MW` >= `RESOURCE PMAX MW`,
    !is.na(`CURTAILMENT START DATE TIME`),
    !is.na(`CURTAILMENT END DATE TIME`)
  ) |>
  arrange(`RESOURCE ID`, `CURTAILMENT START DATE TIME`) |>
  distinct(
    `RESOURCE ID`,
    `RESOURCE NAME`,
    `CURTAILMENT START DATE TIME`,
    `CURTAILMENT END DATE TIME`,
    `RESOURCE PMAX MW`,
    `CURTAILMENT MW`,
    .keep_all = TRUE
  ) |>
  slice_head(n = 2, by = `RESOURCE ID`)

outages <- bind_rows(curtailment_outages, full_outage_events) |>
  mutate(
    `CURTAILMENT START DATE TIME` = format(
      as.POSIXct(`CURTAILMENT START DATE TIME`),
      "%Y-%m-%d %H:%M:%S"
    ),
    `CURTAILMENT END DATE TIME` = format(
      as.POSIXct(`CURTAILMENT END DATE TIME`),
      "%Y-%m-%d %H:%M:%S"
    )
  )

dam_big_selected <- arrow::open_dataset("CAISO_dam_big.parquet") |>
  filter(
    RESOURCEBID_SEQ %in% final_matches$RESOURCEBID_SEQ,
    MARKETPRODUCTTYPE == "EN"
  ) |>
  select(
    RESOURCEBID_SEQ,
    STARTTIME,
    MARKETPRODUCTTYPE,
    RESOURCE_TYPE,
    SCH_BID_XAXISDATA,
    SCH_BID_Y1AXISDATA,
    SELFSCHEDMW
  ) |>
  collect() |>
  mutate(STARTTIME_DATE = as.Date(substr(STARTTIME, 1, 10)))

curtailment_dam_data <- dam_big_selected |>
  semi_join(
    match_detail |>
      distinct(RESOURCEBID_SEQ, STARTTIME_DATE = curtailment_date),
    by = c("RESOURCEBID_SEQ", "STARTTIME_DATE")
  ) |>
  select(
    RESOURCEBID_SEQ,
    STARTTIME = STARTTIME_DATE,
    SCH_BID_XAXISDATA,
    SELFSCHEDMW,
    SCH_BID_Y1AXISDATA
  )

plot_choice <- dam_big_selected |>
  filter(
    !is.na(SCH_BID_XAXISDATA),
    !is.na(SCH_BID_Y1AXISDATA)
  ) |>
  count(RESOURCEBID_SEQ, STARTTIME_DATE, name = "n_rows") |>
  filter(n_rows >= 2L) |>
  arrange(abs(n_rows - 100L), RESOURCEBID_SEQ, STARTTIME_DATE) |>
  slice_head(n = 1)

plot_resource <- plot_choice$RESOURCEBID_SEQ[[1]]

dam_plot_data <- dam_big_selected |>
  inner_join(
    plot_choice |>
      select(RESOURCEBID_SEQ, STARTTIME_DATE),
    by = c("RESOURCEBID_SEQ", "STARTTIME_DATE")
  ) |>
  select(
    RESOURCEBID_SEQ,
    STARTTIME,
    SCH_BID_XAXISDATA,
    SCH_BID_Y1AXISDATA,
    SELFSCHEDMW
  )

if (nrow(dam_plot_data) == 0L) {
  dam_plot_data <- dam_big_selected |>
    filter(
      !is.na(SCH_BID_XAXISDATA),
      !is.na(SCH_BID_Y1AXISDATA)
    ) |>
    add_count(RESOURCEBID_SEQ, STARTTIME_DATE) |>
    arrange(desc(n), RESOURCEBID_SEQ, STARTTIME_DATE) |>
    filter(
      RESOURCEBID_SEQ == first(RESOURCEBID_SEQ),
      STARTTIME_DATE == first(STARTTIME_DATE)
    ) |>
    select(
      RESOURCEBID_SEQ,
      STARTTIME,
      SCH_BID_XAXISDATA,
      SCH_BID_Y1AXISDATA,
      SELFSCHEDMW
    )

  plot_resource <- unique(dam_plot_data$RESOURCEBID_SEQ)[[1]]
}

rtm_outage_lookup <- full_outage_events |>
  inner_join(
    final_matches |>
      select(generator, RESOURCEBID_SEQ),
    by = c("RESOURCE ID" = "generator")
  ) |>
  transmute(
    generator = `RESOURCE ID`,
    generator_name = `RESOURCE NAME`,
    RESOURCEBID_SEQ,
    pmax_mw = `RESOURCE PMAX MW`,
    outage_start = as.POSIXct(`CURTAILMENT START DATE TIME`),
    outage_stop = as.POSIXct(`CURTAILMENT END DATE TIME`)
  ) |>
  filter(!is.na(outage_start), !is.na(outage_stop)) |>
  mutate(
    window_start = as.Date(outage_start) - 1,
    window_stop = as.Date(outage_stop) + 1
  )

rtm_windows <- rtm_outage_lookup |>
  select(RESOURCEBID_SEQ, window_start, window_stop)

rtm_data <- arrow::open_dataset("CAISO_rtm_big.parquet") |>
  filter(
    RESOURCEBID_SEQ %in% unique(rtm_windows$RESOURCEBID_SEQ),
    MARKETPRODUCTTYPE == "EN"
  ) |>
  select(
    MARKETPRODUCTTYPE,
    RESOURCEBID_SEQ,
    SCH_BID_XAXISDATA,
    SCH_BID_TIMEINTERVALSTART,
    SCH_BID_TIMEINTERVALSTOP,
    TIMEINTERVALSTART,
    TIMEINTERVALEND
  ) |>
  collect() |>
  mutate(
    interval_date = as.Date(substr(
      coalesce(SCH_BID_TIMEINTERVALSTART, TIMEINTERVALSTART),
      1,
      10
    ))
  ) |>
  inner_join(
    rtm_windows,
    by = "RESOURCEBID_SEQ",
    relationship = "many-to-many"
  ) |>
  filter(
    interval_date >= window_start,
    interval_date <= window_stop
  ) |>
  select(
    MARKETPRODUCTTYPE,
    RESOURCEBID_SEQ,
    SCH_BID_XAXISDATA,
    SCH_BID_TIMEINTERVALSTART,
    SCH_BID_TIMEINTERVALSTOP,
    TIMEINTERVALSTART,
    TIMEINTERVALEND
  ) |>
  distinct()

if (nrow(curtailment_dam_data) == 0L) {
  stop("No DAM rows were sampled from CAISO_dam_big.parquet.", call. = FALSE)
}

if (nrow(dam_plot_data) == 0L) {
  stop(
    "No DAM plotting rows were sampled from CAISO_dam_big.parquet.",
    call. = FALSE
  )
}

if (nrow(rtm_data) == 0L) {
  stop("No RTM rows were sampled from CAISO_rtm_big.parquet.", call. = FALSE)
}

caisopea_vignette_sample <- list(
  curtailment_dam_data = curtailment_dam_data,
  rtm_data = rtm_data,
  dam_plot_data = dam_plot_data,
  outages = outages,
  id_attrs = id_attrs,
  final_matches = final_matches,
  match_detail = match_detail,
  rtm_outage_lookup = rtm_outage_lookup,
  plot_resource = plot_resource,
  source = list(
    outages = "prior_trade_day_outages.parquet",
    id_attrs = "ID_attributes.parquet",
    match_detail = "curtailment_match_detail.csv",
    final_matches = "curtailment_matches_final.csv",
    curtailment_dam_data = "CAISO_dam_big.parquet",
    dam_plot_data = "CAISO_dam_big.parquet",
    rtm_data = "CAISO_rtm_big.parquet"
  )
)

usethis::use_data(
  caisopea_vignette_sample,
  overwrite = TRUE,
  compress = "xz"
)
