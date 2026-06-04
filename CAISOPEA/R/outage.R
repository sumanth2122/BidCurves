#' Get curtailment-based RESOURCEBID_SEQ matches
#'
#' CAISO's Day-Ahead Market (DAM) bid data anonymizes generators using a numeric
#' `RESOURCEBID_SEQ` identifier, concealing which physical resource is behind
#' each bid curve. However, the Prior Trade Day Outage Report publishes named
#' resources (`RESOURCE ID`) along with their curtailment windows and curtailed
#' MW.
#'
#' When a generator files a planned partial curtailment, it knows its reduced
#' operating limit before the DAM closes. Its day-ahead bid curve should
#' therefore reflect a maximum MW of approximately `RESOURCE PMAX MW -
#' CURTAILMENT MW` -- the expected available capacity. If we find an anonymous
#' `RESOURCEBID_SEQ` whose daily max bid MW closely matches this expected
#' available capacity on the same date, that sequence ID is a candidate match
#' for the named resource.
#'
#' Filter outages to `PLANNED` + partial curtailments (`CURTAILMENT MW <
#' RESOURCE PMAX MW`). Compute `expected_available_mw = RESOURCE PMAX MW -
#' CURTAILMENT MW` per resource per curtailment date. Compute `daily_max_mw` per
#' `RESOURCEBID_SEQ` from the DAM bid data. Pre-filter sequence IDs using
#' `id_attrs`: only consider IDs whose all-time `max_mw` is within
#' `pmax_tolerance` of `RESOURCE PMAX MW`. This eliminates implausible size
#' mismatches before the expensive join. Join on date -- every size-compatible
#' sequence ID active on a curtailment day becomes a candidate. Filter to
#' candidates whose `daily_max_mw` is within `tolerance` of
#' `expected_available_mw`.
#'
#' Score each (`RESOURCE ID`, `RESOURCEBID_SEQ`) pair using a combined metric
#' that rewards both more matching days and lower MW error.
#' Compute a confidence score from 0 to 1 comparing the top candidate's score to
#' the runner-up: 0 means tied, 1 means no competing candidate existed.
#' Resolve collisions where the same `RESOURCEBID_SEQ` is claimed by
#' multiple named resources -- keep only the highest-confidence claim.
#'
#' Planned curtailments are filed in advance and published on the prior trade day
#' report before the DAM closes, so the generator has every reason to reflect
#' the derating in their day-ahead bids. This makes the signal clean and
#' predictable, unlike forced outages which may not appear in DAM bids at all.
#'
#' @param dam_data DAM bid data. Must include `RESOURCEBID_SEQ`, `STARTTIME`,
#'   `SCH_BID_XAXISDATA`, and `SELFSCHEDMW`.
#' @param outages Prior trade day outage data. Must include `OUTAGE TYPE`,
#'   `RESOURCE ID`, `RESOURCE NAME`, `RESOURCE PMAX MW`, `CURTAILMENT MW`, and
#'   `CURTAILMENT START DATE TIME`.
#' @param id_attrs Sequence ID attributes. Must include `RESOURCEBID_SEQ` and
#'   all-time observed `max_mw`.
#' @param tolerance Tolerance used when comparing `daily_max_mw` to
#'   `expected_available_mw`.
#' @param pmax_tolerance Tolerance used when comparing all-time observed
#'   `max_mw` to `RESOURCE PMAX MW`.
#' @param min_matches Minimum number of matching days.
#' @param resolve_collisions If `TRUE`, resolve collisions where the same
#'   `RESOURCEBID_SEQ` is claimed by multiple named resources.
#' @param return_detail If `TRUE`, return final matches, per-day match detail,
#'   scores, and raw matches.
#'
#' @return If `return_detail = FALSE`, a tibble with the final deanonymization
#'   map: one row per named resource to seq ID pair, joined with resource
#'   nameplate PMAX and the seq ID's all-time observed max MW. If
#'   `return_detail = TRUE`, a named list with `final_matches`, `match_detail`,
#'   `scores`, and `raw_matches`.
#'
#' @export
get_curtailment_matches <- function(
  dam_data,
  outages,
  id_attrs,
  tolerance = 0.05,
  pmax_tolerance = 0.10,
  min_matches = 2L,
  resolve_collisions = TRUE,
  return_detail = FALSE
) {
  dam_data <- duckplyr::as_duckdb_tibble(dam_data)
  outages <- duckplyr::as_duckdb_tibble(outages)
  id_attrs <- duckplyr::as_duckdb_tibble(id_attrs)

  planned_partial <- outages |>
    dplyr::filter(
      `OUTAGE TYPE` == "PLANNED",
      `RESOURCE PMAX MW` > 0,
      `CURTAILMENT MW` < `RESOURCE PMAX MW`
    ) |>
    dplyr::mutate(
      curtailment_date = as.Date(`CURTAILMENT START DATE TIME`),
      expected_available_mw = `RESOURCE PMAX MW` - `CURTAILMENT MW`
    ) |>
    dplyr::distinct(
      `RESOURCE ID`,
      `RESOURCE NAME`,
      curtailment_date,
      `RESOURCE PMAX MW`,
      `CURTAILMENT MW`,
      expected_available_mw
    )

  pmax_lookup <- planned_partial |>
    dplyr::distinct(`RESOURCE ID`, `RESOURCE PMAX MW`) |>
    dplyr::cross_join(
      id_attrs |>
        dplyr::select(RESOURCEBID_SEQ, max_mw)
    ) |>
    dplyr::filter(
      abs(max_mw - `RESOURCE PMAX MW`) / `RESOURCE PMAX MW` <= pmax_tolerance
    ) |>
    dplyr::select(`RESOURCE ID`, RESOURCEBID_SEQ)

  matches <- planned_partial |>
    dplyr::inner_join(
      pmax_lookup,
      by = "RESOURCE ID",
      relationship = "many-to-many"
    ) |>
    dplyr::inner_join(
      dam_data |>
        dplyr::mutate(
          date = as.Date(STARTTIME),
          row_max_mw = pmax(SCH_BID_XAXISDATA, SELFSCHEDMW, na.rm = TRUE)
        ) |>
        dplyr::summarise(
          daily_max_mw = max(row_max_mw, na.rm = TRUE),
          .by = c(RESOURCEBID_SEQ, date)
        ) |>
        dplyr::filter(is.finite(daily_max_mw)) |>
        dplyr::semi_join(
          pmax_lookup |>
            dplyr::distinct(RESOURCEBID_SEQ),
          by = "RESOURCEBID_SEQ"
        ),
      by = dplyr::join_by(curtailment_date == date, RESOURCEBID_SEQ),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      expected_available_mw > 0,
      abs(daily_max_mw - expected_available_mw) / expected_available_mw <=
        tolerance
    ) |>
    dplyr::select(
      `RESOURCE ID`,
      `RESOURCE NAME`,
      curtailment_date,
      `RESOURCE PMAX MW`,
      `CURTAILMENT MW`,
      expected_available_mw,
      RESOURCEBID_SEQ,
      daily_max_mw
    )

  scores <- score_curtailment_matches(matches)

  top_matches <- scores |>
    dplyr::filter(n_matches >= min_matches) |>
    dplyr::arrange(
      `RESOURCE ID`,
      dplyr::desc(score),
      mean_pct_err,
      RESOURCEBID_SEQ
    ) |>
    dplyr::mutate(
      rank = dplyr::row_number(),
      runner_up = dplyr::nth(score, 2L),
      confidence = dplyr::case_when(
        is.na(runner_up) ~ 1,
        score <= 0 ~ 0,
        TRUE ~ pmax(0, pmin(1, 1 - runner_up / score))
      ),
      .by = `RESOURCE ID`
    ) |>
    dplyr::filter(rank == 1L) |>
    dplyr::select(-rank, -runner_up)

  final_matches <- {
    if (resolve_collisions) {
      top_matches |>
        dplyr::arrange(
          RESOURCEBID_SEQ,
          dplyr::desc(confidence),
          dplyr::desc(score),
          dplyr::desc(n_matches),
          mean_pct_err
        ) |>
        dplyr::slice_head(n = 1L, by = RESOURCEBID_SEQ)
    } else {
      top_matches
    }
  } |>
    dplyr::left_join(
      planned_partial |>
        dplyr::summarise(
          `RESOURCE PMAX MW` = max(`RESOURCE PMAX MW`, na.rm = TRUE),
          .by = `RESOURCE ID`
        ),
      by = "RESOURCE ID"
    ) |>
    dplyr::left_join(
      id_attrs |>
        dplyr::select(RESOURCEBID_SEQ, seq_max_mw = max_mw),
      by = "RESOURCEBID_SEQ"
    ) |>
    dplyr::select(
      `RESOURCE ID`,
      `RESOURCE NAME`,
      `RESOURCE PMAX MW`,
      RESOURCEBID_SEQ,
      seq_max_mw,
      n_matches,
      mean_pct_err,
      score,
      confidence
    ) |>
    dplyr::arrange(dplyr::desc(confidence), dplyr::desc(score))

  if (return_detail) {
    return(list(
      final_matches = final_matches,
      match_detail = matches |>
        dplyr::semi_join(
          final_matches,
          by = c("RESOURCE ID", "RESOURCEBID_SEQ")
        ) |>
        dplyr::mutate(
          mw_error = daily_max_mw - expected_available_mw,
          pct_error = mw_error / expected_available_mw
        ) |>
        dplyr::arrange(`RESOURCE ID`, curtailment_date),
      scores = scores,
      raw_matches = matches
    ))
  }

  final_matches
}

#' Match full generator outages to RTM bid gaps
#'
#' When a generator goes offline, its bid curve disappears. This function finds
#' gaps in RTM bid submissions which align with reported outage windows, linking
#' outage records to bid IDs.
#'
#' Each bid's time interval, deduplicated. The RTM parquet has two sets of
#' timestamp columns depending on the record type, so we coalesce to handle both.
#'
#' Find gaps between consecutive bids for each ID. A gap is where one bid ends
#' and the next one starts -- the generator wasn't bidding during that window,
#' which likely means it was offline.
#'
#' The highest MW a bid ID ever offers -- this is our best guess at that ID's
#' rated capacity, used later to check if it matches the outage record's PMAX.
#'
#' Join outages to bid gaps: the gap's `last_bid_stop` should be near the outage
#' start (within `edge_minutes`), and the gap's `next_bid_start` should be at or
#' after the outage end. This pins an outage to a specific bid ID that went quiet
#' at the right time.
#'
#' For each generator-ID pair, aggregate across all matched outages. Score each
#' (`generator`, `RESOURCEBID_SEQ`) pair using a combined metric that rewards
#' both more matches and lower MW error. Compute a confidence score from 0 to 1
#' comparing the top candidate's score to the runner-up: 0 means tied, 1 means
#' no competing candidate existed. Resolve collisions where the same
#' `RESOURCEBID_SEQ` is claimed by multiple named resources--keep only the
#' highest-confidence claim.
#'
#' @param rtm_data RTM bid data. Must include `MARKETPRODUCTTYPE`,
#'   `RESOURCEBID_SEQ`, `SCH_BID_XAXISDATA`, `SCH_BID_TIMEINTERVALSTART`,
#'   `SCH_BID_TIMEINTERVALSTOP`, `TIMEINTERVALSTART`, and `TIMEINTERVALEND`.
#' @param outages CAISO prior trade day outage data.
#' @param generators Character vector of resource IDs, passed to
#'   [get_generator_outages()].
#' @param edge_minutes Slack in minutes when aligning outage times to bid gaps
#'   (default 5).
#' @param min_matches Minimum number of matched outages.
#' @param resolve_collisions If `TRUE`, resolve collisions where the same
#'   `RESOURCEBID_SEQ` is claimed by multiple named resources.
#' @param return_detail If `TRUE`, return final matches, match detail, scores,
#'   raw matches, and cleaned outages.
#'
#' @return If `return_detail = FALSE`, a tibble of generator-bid ID pairs. If
#'   `return_detail = TRUE`, a named list with `final_matches`, `match_detail`,
#'   `scores`, `raw_matches`, and `outages`.
#'
#' @export
get_outage_rtm_matches <- function(
  rtm_data,
  outages,
  generators = NULL,
  edge_minutes = 5,
  min_matches = 1L,
  resolve_collisions = TRUE,
  return_detail = FALSE
) {
  rtm_data <- duckplyr::as_duckdb_tibble(rtm_data)

  outages_clean <- get_generator_outages(
    outages = outages,
    generators = generators
  )

  raw_matches <- outages_clean |>
    dplyr::mutate(
      outage_stop_cmp = dplyr::coalesce(
        outage_stop,
        as.POSIXct("2999-12-31 00:00:00")
      ),
      outage_stop_min = outage_stop_cmp - edge_minutes * 60
    ) |>
    dplyr::inner_join(
      rtm_data |>
        dplyr::filter(
          MARKETPRODUCTTYPE == "EN",
          !is.na(RESOURCEBID_SEQ)
        ) |>
        dplyr::transmute(
          RESOURCEBID_SEQ,
          bid_start = as.POSIXct(
            dplyr::coalesce(SCH_BID_TIMEINTERVALSTART, TIMEINTERVALSTART)
          ),
          bid_stop = as.POSIXct(
            dplyr::coalesce(SCH_BID_TIMEINTERVALSTOP, TIMEINTERVALEND)
          )
        ) |>
        dplyr::distinct() |>
        dplyr::filter(
          !is.na(bid_start),
          !is.na(bid_stop)
        ) |>
        dplyr::arrange(RESOURCEBID_SEQ, bid_start, bid_stop) |>
        dplyr::mutate(
          last_bid_stop = bid_stop,
          next_bid_start = dplyr::lead(bid_start),
          .by = RESOURCEBID_SEQ
        ) |>
        dplyr::filter(is.na(next_bid_start) | next_bid_start > last_bid_stop) |>
        dplyr::mutate(
          gap_start_min = last_bid_stop - edge_minutes * 60,
          gap_start_max = last_bid_stop + edge_minutes * 60,
          next_bid_start_cmp = dplyr::coalesce(
            next_bid_start,
            as.POSIXct("2999-12-31 00:00:00")
          )
        ) |>
        dplyr::select(
          RESOURCEBID_SEQ,
          last_bid_stop,
          next_bid_start,
          next_bid_start_cmp,
          gap_start_min,
          gap_start_max
        ),
      by = dplyr::join_by(
        outage_start >= gap_start_min,
        outage_start <= gap_start_max
      ),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(next_bid_start_cmp >= outage_stop_min) |>
    dplyr::left_join(
      rtm_data |>
        dplyr::filter(
          MARKETPRODUCTTYPE == "EN",
          !is.na(RESOURCEBID_SEQ)
        ) |>
        dplyr::summarise(
          max_mw = max(SCH_BID_XAXISDATA, na.rm = TRUE),
          .by = RESOURCEBID_SEQ
        ),
      by = "RESOURCEBID_SEQ"
    ) |>
    dplyr::mutate(
      stop_offset = as.numeric(difftime(
        last_bid_stop,
        outage_start,
        units = "mins"
      )),
      restart_offset = as.numeric(difftime(
        next_bid_start,
        outage_stop,
        units = "mins"
      ))
    ) |>
    dplyr::select(
      generator,
      outage_start,
      outage_stop,
      pmax_mw,
      RESOURCEBID_SEQ,
      last_bid_stop,
      next_bid_start,
      stop_offset,
      restart_offset,
      max_mw
    ) |>
    dplyr::arrange(
      generator,
      outage_start,
      abs(stop_offset),
      abs(max_mw - pmax_mw),
      RESOURCEBID_SEQ
    )

  scores <- score_outage_matches(raw_matches)

  top_matches <- scores |>
    dplyr::filter(n_matches >= min_matches) |>
    dplyr::arrange(
      generator,
      dplyr::desc(score),
      mean_pct_err,
      mean_abs_stop_offset,
      RESOURCEBID_SEQ
    ) |>
    dplyr::mutate(
      rank = dplyr::row_number(),
      runner_up = dplyr::nth(score, 2L),
      confidence = dplyr::case_when(
        is.na(runner_up) ~ 1,
        score <= 0 ~ 0,
        TRUE ~ pmax(0, pmin(1, 1 - runner_up / score))
      ),
      .by = generator
    ) |>
    dplyr::filter(rank == 1L) |>
    dplyr::select(-rank, -runner_up)

  final_matches <- {
    if (resolve_collisions) {
      top_matches |>
        dplyr::arrange(
          RESOURCEBID_SEQ,
          dplyr::desc(confidence),
          dplyr::desc(score),
          dplyr::desc(n_matches),
          mean_pct_err,
          mean_abs_stop_offset
        ) |>
        dplyr::slice_head(n = 1L, by = RESOURCEBID_SEQ)
    } else {
      top_matches
    }
  } |>
    dplyr::arrange(dplyr::desc(confidence), dplyr::desc(score))

  if (return_detail) {
    return(list(
      final_matches = final_matches,
      match_detail = raw_matches |>
        dplyr::semi_join(
          final_matches,
          by = c("generator", "RESOURCEBID_SEQ")
        ) |>
        dplyr::arrange(generator, outage_start, RESOURCEBID_SEQ),
      scores = scores,
      raw_matches = raw_matches,
      outages = outages_clean
    ))
  }

  final_matches
}

#' Score curtailment match candidates
#'
#' The raw count of matching days (`n_matches`) alone is insufficient as a
#' ranking signal because it ignores how closely a sequence ID's bids track the
#' expected available capacity on those days. Two candidates might both match 71
#' days, but one always bids within 0.1% of the target while the other
#' consistently sits at 4.9% -- the first is a much stronger match.
#'
#' For each (`RESOURCE ID`, `RESOURCEBID_SEQ`) pair, compute `n_matches`,
#' `mean_pct_err`, and `score = n_matches * (1 - mean_pct_err)`. This penalises
#' candidates with high average MW error, so a candidate with 71 matches at 0%
#' error outscores one with 71 matches at 4% error.
#'
#' @param matches Candidate match rows with `daily_max_mw`,
#'   `expected_available_mw`, `RESOURCE ID`, `RESOURCE NAME`, and
#'   `RESOURCEBID_SEQ`.
#'
#' @return A tibble of candidate scores with `n_matches`, `mean_pct_err`, and
#'   `score`.
#'
#' @keywords internal
score_curtailment_matches <- function(matches) {
  matches |>
    dplyr::mutate(
      abs_pct_err = abs(daily_max_mw - expected_available_mw) /
        expected_available_mw
    ) |>
    dplyr::summarise(
      n_matches = dplyr::n(),
      mean_pct_err = mean(abs_pct_err, na.rm = TRUE),
      score = n_matches * (1 - mean_pct_err),
      .by = c(`RESOURCE ID`, `RESOURCE NAME`, RESOURCEBID_SEQ)
    ) |>
    dplyr::arrange(dplyr::desc(score), mean_pct_err)
}

#' Score full-outage RTM gap candidates
#'
#' For each generator-ID pair, aggregate across all matched outages.
#'
#' The raw count of matching days (`n_matches`) alone is insufficient as a
#' ranking signal because it ignores how closely a sequence ID's bids track the
#' expected available capacity on those days.
#'
#' For each (`generator`, `RESOURCEBID_SEQ`) pair, compute `n_matches`,
#' `mean_pct_err`, and `score = n_matches * (1 - mean_pct_err)`. This penalises
#' candidates with high average MW error.
#'
#' @param matches Raw join results with `generator`, `RESOURCEBID_SEQ`,
#'   `pmax_mw`, `max_mw`, `stop_offset`, and `restart_offset`.
#'
#' @return A tibble of per-match stats with offsets, match count, mean absolute
#'   percentage error, and score.
#'
#' @keywords internal
score_outage_matches <- function(matches) {
  matches |>
    dplyr::filter(
      pmax_mw > 0,
      is.finite(max_mw)
    ) |>
    dplyr::mutate(
      abs_pct_err = abs(max_mw - pmax_mw) / pmax_mw,
      abs_stop_offset = abs(stop_offset),
      abs_restart_offset = abs(restart_offset)
    ) |>
    dplyr::summarise(
      n_matches = dplyr::n(),
      pmax_mw = max(pmax_mw, na.rm = TRUE),
      max_mw = max(max_mw, na.rm = TRUE),
      mean_pct_err = mean(abs_pct_err, na.rm = TRUE),
      mean_stop_offset = mean(stop_offset, na.rm = TRUE),
      mean_restart_offset = mean(restart_offset, na.rm = TRUE),
      mean_abs_stop_offset = mean(abs_stop_offset, na.rm = TRUE),
      mean_abs_restart_offset = mean(abs_restart_offset, na.rm = TRUE),
      score = n_matches * (1 - mean_pct_err),
      .by = c(generator, RESOURCEBID_SEQ)
    ) |>
    dplyr::arrange(
      generator,
      dplyr::desc(score),
      mean_pct_err,
      mean_abs_stop_offset,
      RESOURCEBID_SEQ
    )
}

#' Extract full generator outages from CAISO prior trade day data
#'
#' Reads CAISO prior trade day data and filters to rows where the curtailed MW
#' meets or exceeds the generator's PMAX (i.e. the whole unit is down, not just
#' derated). Overlapping outage windows for the same generator are merged into
#' contiguous intervals.
#'
#' Pull full outages: only keep rows where the unit is completely down (curtailed
#' MW >= rated capacity). Partial derates are excluded.
#'
#' CAISO sometimes reports the same outage as multiple rows with the same start
#' time but different stop times. Collapse them -- take the latest stop.
#'
#' Merge overlapping or touching outage windows into contiguous groups.
#' `merge_stop`: if an outage has no end, assume it extends to the next outage's
#' start (or its own start if there is no next outage). `outage_group`:
#' increments whenever the current start falls after all previous merged stops --
#' i.e. this is a new, non-overlapping interval.
#'
#' Collapse each group into a single row spanning the full outage window.
#'
#' @param outages CAISO prior trade day outage data.
#' @param generators Character vector of resource IDs to keep. If `NULL`
#'   (default), all generators are returned.
#'
#' @return A tibble with columns:
#'   - `generator`: resource ID
#'   - `outage_start`: start of the outage (POSIXct)
#'   - `outage_stop`: end of the outage (POSIXct, `NA` if still ongoing)
#'   - `pmax_mw`: generator max capacity
#'
#' @keywords internal
get_generator_outages <- function(outages, generators = NULL) {
  duckplyr::as_duckdb_tibble(outages) |>
    dplyr::filter(
      `RESOURCE PMAX MW` > 0,
      `CURTAILMENT MW` >= `RESOURCE PMAX MW`
    ) |>
    dplyr::filter(
      if (is.null(generators)) TRUE else `RESOURCE ID` %in% generators
    ) |>
    dplyr::transmute(
      generator = `RESOURCE ID`,
      outage_start = as.POSIXct(`CURTAILMENT START DATE TIME`),
      outage_stop = as.POSIXct(`CURTAILMENT END DATE TIME`),
      pmax_mw = `RESOURCE PMAX MW`
    ) |>
    dplyr::distinct() |>
    dplyr::summarise(
      outage_stop = if (all(is.na(outage_stop))) {
        outage_start[NA_integer_]
      } else {
        max(outage_stop, na.rm = TRUE)
      },
      pmax_mw = max(pmax_mw, na.rm = TRUE),
      .by = c(generator, outage_start)
    ) |>
    dplyr::arrange(generator, outage_start, outage_stop) |>
    dplyr::mutate(
      merge_stop = dplyr::coalesce(
        outage_stop,
        dplyr::lead(outage_start),
        outage_start
      ),
      outage_group = cumsum(
        dplyr::coalesce(
          as.numeric(outage_start) > dplyr::lag(cummax(as.numeric(merge_stop))),
          TRUE
        )
      ),
      .by = generator
    ) |>
    dplyr::summarise(
      outage_start = min(outage_start),
      outage_stop = if (all(is.na(outage_stop))) {
        outage_start[NA_integer_]
      } else {
        max(outage_stop, na.rm = TRUE)
      },
      pmax_mw = max(pmax_mw, na.rm = TRUE),
      .by = c(generator, outage_group)
    ) |>
    dplyr::select(generator, outage_start, outage_stop, pmax_mw) |>
    dplyr::arrange(generator, outage_start, outage_stop)
}
