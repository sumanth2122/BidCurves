#' Plot bid curves for a resource
#'
#' Creates hourly bid-curve plots for a single resource from market bid data.
#' Each curve is drawn as a step function using horizontal quantity-price
#' segments and vertical price changes. Curves are grouped and colored by
#' `STARTTIME`.
#'
#' @param market_data A data frame containing market bid data. Must include
#'   `STARTTIME`, `RESOURCEBID_SEQ`, `SCH_BID_XAXISDATA`, and
#'   `SCH_BID_Y1AXISDATA`.
#' @param resource_id Resource identifier to filter on. Compared against
#'   `RESOURCEBID_SEQ`.
#'
#' @return A [ggplot2::ggplot()] object showing bid price in dollars per MWh
#'   against bid quantity in MW for the selected resource.
#'
#' @export
plot_curves <- function(market_data, resource_id) {
  market_data |>
    dplyr::filter(
      RESOURCEBID_SEQ == resource_id,
      !is.na(STARTTIME),
      !is.na(SCH_BID_XAXISDATA),
      !is.na(SCH_BID_Y1AXISDATA)
    ) |>
    dplyr::mutate(
      STARTDATETIME = as.POSIXct(STARTTIME),
      Hour = format(STARTDATETIME, "%H:%M:%S"),
      x = as.numeric(SCH_BID_XAXISDATA),
      y = as.numeric(SCH_BID_Y1AXISDATA)
    ) |>
    dplyr::arrange(STARTDATETIME, x, y) |>
    dplyr::group_by(STARTDATETIME) |>
    dplyr::mutate(
      x_next = dplyr::lead(x),
      y_next = dplyr::lead(y),
      single_point = dplyr::n() == 1
    ) |>
    dplyr::ungroup() |>
    ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = \(x) dplyr::filter(x, !is.na(x_next), x != x_next),
      ggplot2::aes(
        x = x,
        xend = x_next,
        y = y,
        yend = y,
        color = Hour,
        group = STARTDATETIME
      )
    ) +
    ggplot2::geom_segment(
      data = \(x) dplyr::filter(x, !is.na(x_next), y != y_next),
      ggplot2::aes(
        x = x_next,
        xend = x_next,
        y = y,
        yend = y_next,
        color = Hour,
        group = STARTDATETIME
      )
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = x,
        y = y,
        color = Hour,
        shape = single_point
      ),
      size = 2,
      stroke = 0.7
    ) +
    ggplot2::scale_shape_manual(
      values = c(`FALSE` = 16, `TRUE` = 2),
      guide = "none"
    ) +
    ggplot2::labs(
      x = "Quantity (MW)",
      y = "Bid Price ($/MWh)",
      title = sprintf(
        "ID: %s Date: %s",
        resource_id,
        as.Date(market_data$STARTTIME[1])
      ),
      color = "Hour"
    ) +
    ggplot2::theme_bw()
  # TODO: fix hours theme match dashboard
}
