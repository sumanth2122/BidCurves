#' Find similar RESOURCEBID_SEQ feature profiles
#'
#' Uses the package's internal precomputed weighted cosine similarity matrix.
#'
#' @param id DAM RESOURCEBID_SEQ to query.
#' @param n Number of matches to return.
#' @param start_filter One of `"none"`, `"after"`, or `"before"`.
#'
#' @return A tibble with `RESOURCEBID_SEQ` and `cosine_similarity`.
#'
#' @export
find_similar_ids <- function(
  id,
  n = 10L,
  start_filter = c("none", "after", "before")
) {
  start_filter <- match.arg(start_filter)
  id <- as.character(id)

  if (!id %in% rownames(cosine_similarity)) {
    stop("id not found in internal cosine_similarity: ", id, call. = FALSE)
  }

  scores <- cosine_similarity[id, ]
  scores <- scores[names(scores) != id]

  if (start_filter != "none") {
    starts <- features |>
      dplyr::mutate(
        RESOURCEBID_SEQ = as.character(RESOURCEBID_SEQ),
        start = as.Date(start)
      ) |>
      dplyr::select(RESOURCEBID_SEQ, start)

    id_start <- starts$start[starts$RESOURCEBID_SEQ == id]

    if (length(id_start) != 1L) {
      stop("id must match exactly one row in internal features", call. = FALSE)
    }

    candidate_starts <- starts$start[
      match(names(scores), starts$RESOURCEBID_SEQ)
    ]

    keep <- switch(
      start_filter,
      after = candidate_starts > id_start,
      before = candidate_starts < id_start
    )

    scores <- scores[keep]
  }

  tibble::enframe(
    sort(scores, decreasing = TRUE),
    name = "RESOURCEBID_SEQ",
    value = "cosine_similarity"
  ) |>
    dplyr::slice_head(n = n)
}
