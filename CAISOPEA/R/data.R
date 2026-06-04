#' Real CAISO sample data for package vignettes
#'
#' A compact real-data sample used by the package vignettes. It was built from
#' repository-level CAISO prior-trade-day outage data, ID attributes,
#' curtailment match outputs, and sampled rows from the big DAM and RTM parquet
#' files.
#'
#' @format A named list with:
#' \describe{
#'   \item{curtailment_dam_data}{Compact DAM daily maximum MW rows for
#'   curtailment matching.}
#'   \item{rtm_data}{Sampled RTM bid interval rows from `CAISO_rtm_big.parquet`
#'   near the sampled full-outage windows.}
#'   \item{dam_plot_data}{One-day DAM bid rows for plotting a bid curve.}
#'   \item{outages}{Prior trade day outage rows supporting the curtailment
#'   sample.}
#'   \item{id_attrs}{ID attribute rows for the sampled sequence IDs.}
#'   \item{final_matches}{Reference curtailment match rows for the sampled
#'   resources.}
#'   \item{match_detail}{Per-day curtailment match evidence for the sampled
#'   resources.}
#'   \item{plot_resource}{The sampled `RESOURCEBID_SEQ` used for plotting.}
#'   \item{source}{Source file names used to build the sample.}
#' }
#'
#' @source Built by `data-raw/vignette-sample.R` from repository data files.
"caisopea_vignette_sample"
