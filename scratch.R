library(DBI)
library(duckdb)
library(tidyverse)

con <- dbConnect(duckdb())

rtm_tbl_pointer <- tbl(con, "read_parquet('CAISO_rtm_big.parquet')")
dam_tbl_pointer <- tbl(con, "read_parquet('CAISO_dam_big.parquet')")

generator_energy_bids <- function(tbl_pointer) {
  tbl_pointer |>
    filter(
      !is.na(SCH_BID_XAXISDATA),
      RESOURCE_TYPE == "GENERATOR",
      MARKETPRODUCTTYPE == "EN"
    ) |>
    mutate(SELFSCHEDMW = as.numeric(SELFSCHEDMW))
}

unique_ids <- function(tbl_pointer) {
  generator_energy_bids(tbl_pointer) |>
    distinct(RESOURCEBID_SEQ) |>
    collect()
}

top_5_ids_by_max_mw <- function(tbl_pointer) {
  generator_energy_bids(tbl_pointer) |>
    summarise(
      max_mw = coalesce(
        max(SCH_BID_XAXISDATA, na.rm = TRUE),
        max(SELFSCHEDMW, na.rm = TRUE)
      ),
      .by = RESOURCEBID_SEQ
    ) |>
    slice_max(order_by = max_mw, n = 5, with_ties = FALSE) |>
    arrange(desc(max_mw)) |>
    collect()
}

rtm_ids <- unique_ids(rtm_tbl_pointer) |> print()
dam_ids <- unique_ids(dam_tbl_pointer) |> print()

top_5_rtm <- top_5_ids_by_max_mw(rtm_tbl_pointer) |> print()
top_5_dam <- top_5_ids_by_max_mw(dam_tbl_pointer) |> print()
