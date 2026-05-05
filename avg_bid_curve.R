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

# avg over entire lifespan
get_mean_bid_curve <- function(RESOURCE_ID){

  tbl_pointer |>
      mutate(SCH_BID_XAXISDATA = coalesce(as.double(SELFSCHEDMW), SCH_BID_XAXISDATA)) |>
      select(RESOURCEBID_SEQ, SCH_BID_XAXISDATA, SCH_BID_Y1AXISDATA, STARTTIME) |>
      filter(RESOURCEBID_SEQ == RESOURCE_ID) |>
      mutate(date = as_datetime(STARTTIME),
            hour = hour(as_datetime(STARTTIME)),
            group = if_else(hour %in% 12:20, "12-20", "else")) |>
      group_by(group, SCH_BID_XAXISDATA) |>
      summarise(avg_price = mean(SCH_BID_Y1AXISDATA, na.rm = TRUE),
                num_bids = n(),
                sd_price = sd(SCH_BID_Y1AXISDATA, na.rm = TRUE)) |>
      mutate(percent_group = num_bids/sum(num_bids)) |>
      ungroup() |>
      mutate(percent_total = num_bids/sum(num_bids),
             RESOURCE_ID = RESOURCE_ID) |>
      arrange(group, SCH_BID_XAXISDATA)

}

# filters based on if id is introduced/deprecated
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
                num_bids = n(),
                sd_price = sd(SCH_BID_Y1AXISDATA, na.rm = TRUE)) |>
      mutate(percent_group = num_bids/sum(num_bids)) |>
      ungroup() |>
      mutate(percent_total = num_bids/sum(num_bids),
             RESOURCE_ID = RESOURCE_ID) |>
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
      summarise(avg_price = mean(SCH_BID_Y1AXISDATA, na.rm = TRUE),
                sd_price = sd(SCH_BID_Y1AXISDATA, na.rm = TRUE)) |>
      mutate(RESOURCE_ID = RESOURCE_ID) |>
      arrange(group, SCH_BID_XAXISDATA)
  }

}

test <- get_mean_bid_curve(777449, "deprecated")
view(test)
get_mean_bid_curve(777449)
# % of total bids, % of total bids in group, instead of each MW, do quantiles of max MW



# FOR SHENANIGANS - average bid curve ----------------------------------------------------------------------------------

id <- test |>
  pull(RESOURCE_ID) |>
  first()

test |>
  ggplot(aes(x = SCH_BID_XAXISDATA,
             y = avg_price,
             group = group)) +
  geom_ribbon(aes(x = SCH_BID_XAXISDATA, y = avg_price,
              ymin = avg_price - 2*sd_price,
              ymax = avg_price + 2*sd_price),
              fill = "grey50",
            alpha = 0.5) +
  geom_step() +
  # geom_point() +
  theme_bw() +
  labs(x = "Quantity (MW)",
       y = "Bid Price ($/MWh)",
       title = glue::glue("Average Bid Curve for ID {id}")) +
  facet_wrap(~group, nrow = 2,
             labeller = as_labeller(c(`12-20` = "12PM-8PM",
                                    `else` = "8PM-12PM")))



plot_curves_to_right <- function(market_data, resource_id){
       date_val <- as.Date(market_data$STARTTIME[1])
       
       plot_data <- market_data |>
              mutate(STARTDATETIME = as.factor(STARTTIME),
                     STARTTIME = hms::as_hms(STARTTIME)) |>
              filter(RESOURCEBID_SEQ == resource_id,
                     !is.na(SCH_BID_XAXISDATA)) |>
              arrange(STARTDATETIME, SCH_BID_XAXISDATA) |>
              group_by(STARTTIME) |>
              arrange(SCH_BID_XAXISDATA, .by_group = TRUE) |>
              mutate(xend = lag(SCH_BID_XAXISDATA),
                     yend = SCH_BID_Y1AXISDATA,
                     xend = if_else(is.na(xend), 0, xend),
                     xend = if_else(row_number() == 1 & (n() > 1), SCH_BID_XAXISDATA, xend),
                     # if using lead() then >= 0!!!
                     is_pos = if_else(SCH_BID_XAXISDATA  > 0, "Positive MW Bids", "Negative MW Bids"),
                     end_point = (SCH_BID_XAXISDATA == xend),
                     single_point = (n() == 1)
                     )
       plot_data |>
              ggplot(aes(x=SCH_BID_XAXISDATA, y=SCH_BID_Y1AXISDATA,
                     #     xend=xend, yend=yend, 
                         group=STARTTIME, color=factor(STARTTIME))) +
              geom_point(aes(
                     shape = ifelse(single_point, "Single-point bid",
                            ifelse(end_point, "End point", "Submitted bid")),
                     size = ifelse(single_point, 3, 1.5))) +
              scale_shape_manual(
                     name = "Point Type",
                     values = c(
                     "Single-point bid" = 17,  # triangle
                     "End point" = 1,          # hollow circle
                     "Submitted bid" = 16         # filled circle
                     ), drop=FALSE
              ) +
              # geom_point(aes(
              #     shape = ifelse(single_point, 17, ifelse(end_point, 1, 16)),
              #     size = ifelse(single_point, 3, 1.5)
              # )) +
              # scale_shape_identity() +
              scale_size_identity() + 
              geom_segment(aes(xend=xend, yend=yend), na.rm = TRUE) +
              geom_point(aes(x=xend, y=SCH_BID_Y1AXISDATA), shape=1, size=1.5) +
              labs(x="Quantity (MW)",
                   y = "Bid Price ($/MWh)",
                   title = sprintf("ID: %s Date: %s", resource_id, date_val),
                   color="Hour") +
              facet_wrap(~is_pos) +
              theme_bw()
       # return(plot_data)
}

plot_curves_to_right(test, 774713)





