library(tidyverse)
use("here", "here")

OHE_df <- \(df) {
  df |>
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
    ) |>
    rename(ID = RESOURCEBID_SEQ)
}

here("20250701_20250701_PUB_BID_DAM_v3.csv") |>
  read_csv() |>
  OHE_df()
