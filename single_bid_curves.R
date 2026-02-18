library(dplyr)
library(duckplyr)
use("here", "here")

# Single Point Curves
here("CASIO_dam_big.parquet") |>
  read_parquet_duckdb() |>
  summarise(
    n_points = n(),
    .by = c(RESOURCEBID_SEQ, STARTTIME)
  ) |>
  filter(n_points == 1L) |>
  summarise(one_point_curves = n()) |>
  pull(one_point_curves)
# 582849

# Single Point Curves with nonnull x and y
here("CASIO_dam_big.parquet") |>
  read_parquet_duckdb() |>
  filter(
    !is.na(SCH_BID_XAXISDATA),
    !is.na(SCH_BID_Y1AXISDATA)
  ) |>
  summarise(
    n_points = n(),
    .by = c(STARTTIME, RESOURCEBID_SEQ)
  ) |>
  filter(n_points == 1L) |>
  summarise(n_single = n()) |>
  pull(n_single)
# 452964
