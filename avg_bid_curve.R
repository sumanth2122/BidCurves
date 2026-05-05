library(tidyverse)
library(duckdb)

con <- dbConnect(duckdb())
tbl_pointer <- tbl(con, "read_parquet('CAISO_dam_big.parquet')")

# average bid curve for one id
  # tbl_pointer |>
  #   select(RESOURCEBID_SEQ, SCH_BID_XAXISDATA, SCH_BID_Y1AXISDATA, STARTTIME) |>
  #   # filter(RESOURCEBID_SEQ == RESOURCE_ID) |>
  #   mutate(hour = hour(as_datetime(STARTTIME))
  #         #  group = if_else(hour <= 12, "0-12", "12-23"),
  #         ) |>
  #   group_by(group, SCH_BID_XAXISDATA) |>
  #   summarise(avg_price = mean(SCH_BID_Y1AXISDATA, na.rm = TRUE)) |>
  #   arrange(SCH_BID_XAXISDATA)

get_mean_bid_curve <- function(RESOURCE_ID, type = "deprecated"){

  if(type == "deprecated"){

    end_date <- tbl_pointer |>
      # filter(RESOURCEBID_SEQ == RESOURCE_ID) |>
      mutate(date = as_datetime(STARTTIME)) |>
      pull(date) |>
      max()
    
    tbl_pointer |>
      mutate(SCH_BID_XAXISDATA = coalesce(as.double(SELFSCHEDMW), SCH_BID_XAXISDATA)) |>
      select(RESOURCEBID_SEQ, SCH_BID_XAXISDATA, SCH_BID_Y1AXISDATA, STARTTIME) |>
      filter(RESOURCEBID_SEQ == RESOURCE_ID) |>
      mutate(date = as_datetime(STARTTIME),
            hour = hour(as_datetime(STARTTIME)),
            group = if_else(hour %in% 12:20, "12-20", "else")) |>
      filter(end_date >= sql("date - INTERVAL '3 months'")) |>
      group_by(group, SCH_BID_XAXISDATA) |>
      summarise(avg_price = mean(SCH_BID_Y1AXISDATA, na.rm = TRUE),
                num_bids = n()) |>
      mutate(percent_group = num_bids/sum(num_bids)) |>
      ungroup() |>
      mutate(percent_total = num_bids/sum(num_bids)) |>
      arrange(group, SCH_BID_XAXISDATA)

  }
  else {

    start_date <- tbl_pointer |>
      # filter(RESOURCEBID_SEQ == RESOURCE_ID) |>
      mutate(date = as_datetime(STARTTIME)) |>
      pull(date) |>
      min()

    tbl_pointer |>
      select(RESOURCEBID_SEQ, SCH_BID_XAXISDATA, SCH_BID_Y1AXISDATA, STARTTIME) |>
      filter(RESOURCEBID_SEQ == RESOURCE_ID) |>
      mutate(date = as_datetime(STARTTIME),
            hour = hour(as_datetime(STARTTIME)),
            group = if_else(hour %in% 12:20, "12-20", "else")) |>
      filter(start_date <= sql("date + INTERVAL '3 months'")) |>
      group_by(group, SCH_BID_XAXISDATA) |>
      summarise(avg_price = mean(SCH_BID_Y1AXISDATA, na.rm = TRUE)) |>
      arrange(group, SCH_BID_XAXISDATA)
  }
  
}

test <- get_mean_bid_curve(774713, "deprecated")
view(test)

# % of total bids, % of total bids in group, instead of each MW, do quantiles of max MW







