# Single Point Bid Curve Numbers
Nicole, Sumanth, and Visruth

# Single Point Curves

``` r
here("CASIO_dam_big.parquet") |>
  read_parquet_duckdb() |>
  summarise(
    n_points = n(),
    .by = c(RESOURCEBID_SEQ, STARTTIME)
  ) |>
  filter(n_points == 1L) |>
  summarise(one_point_curves = n()) |>
  pull(one_point_curves)
```

    [1] 582849

# Single Point Curves with nonnull x and y

``` r
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
```

    [1] 452964

# Number of unique generators which submitted single point curves

``` r
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
  summarise(n_generators_single_point = n_distinct(RESOURCEBID_SEQ)) |>
  pull(n_generators_single_point)
```

    [1] 250
