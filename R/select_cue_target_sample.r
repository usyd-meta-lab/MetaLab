#' Select a Random Sample of Cue–Target Pairs by Approximate Mean FSG
#'
#' This function samples cue–target pairs (e.g., from the Florida Free
#' Association Norms) to achieve an approximate target mean forward
#' strength of association (FSG). The sample contains no duplicate cues
#' or targets, and only includes pairs whose cue and target fall within
#' specified character‐length limits.
#'
#' @param df A data frame containing cue–target pairs and FSG values.
#' @param n Integer. Number of cue–target pairs to select.
#' @param target_mean_fsg Numeric. Desired (approximate) mean FSG for the sample.
#' @param cue_col Character. Name of the column containing cue words.
#'   Default is `"CUE"`.
#' @param target_col Character. Name of the column containing target words.
#'   Default is `"TARGET"`.
#' @param fsg_col Character. Name of the column containing the forward
#'   strength of association (FSG) values.
#'   Default is `"FSG"`.
#' @param cue_min_chars, cue_max_chars Integer. Minimum and maximum
#'   allowable cue lengths (in characters). Defaults are `1` and `Inf`.
#' @param target_min_chars, target_max_chars Integer. Minimum and maximum
#'   allowable target lengths (in characters). Defaults are `1` and `Inf`.
#' @param attempts Integer. Number of random draws to attempt in search
#'   of a sample whose mean FSG is closest to `target_mean_fsg`.
#'   Default is `5000`.
#' @param seed Optional integer to set the random seed for reproducibility.
#'
#' @details
#' The function enforces a one-to-one mapping between cues and targets:
#' each selected pair has a unique cue and a unique target. Sampling is
#' repeated up to `attempts` times; the solution with the smallest
#' absolute difference between the achieved and target mean FSG is
#' returned.
#'
#' Character-length filters are applied before sampling to restrict the
#' vocabulary to stimuli of appropriate length for presentation or
#' experimental constraints.
#'
#' @return
#' A tibble containing the selected cue–target pairs and their
#' FSG values, plus helper columns `.cue_nchar` and `.tgt_nchar`.
#' The following attributes are attached:
#' \itemize{
#'   \item `target_mean_fsg`: the user-specified target mean.
#'   \item `achieved_mean_fsg`: the sample’s actual mean FSG.
#'   \item `mean_abs_diff`: absolute difference between achieved and target means.
#'   \item `attempts_used`: total number of search iterations performed.
#' }
#'
#' @examples
#' \dontrun{
#' library(readxl)
#' florida <- read_xlsx("Florida Norms.xlsx")
#'
#' sample40 <- select_cue_target_sample(
#'   df = florida,
#'   n = 40,
#'   target_mean_fsg = 0.25,
#'   cue_min_chars = 3, cue_max_chars = 10,
#'   target_min_chars = 3, target_max_chars = 10,
#'   attempts = 8000,
#'   seed = 42
#' )
#'
#' attr(sample40, "achieved_mean_fsg")
#' head(sample40)
#' }
#'
#' @author Kit Double
#' @export
select_cue_target_sample <- function(
    df,
    n,
    target_mean_fsg,
    cue_col    = "CUE",
    target_col = "TARGET",
    fsg_col    = "FSG",
    cue_min_chars    = 1,
    cue_max_chars    = Inf,
    target_min_chars = 1,
    target_max_chars = Inf,
    attempts = 5000,
    seed = NULL
) {
  stopifnot(is.data.frame(df), n > 0, attempts >= 1)

  cue_sym <- rlang::sym(cue_col)
  tgt_sym <- rlang::sym(target_col)
  fsg_sym <- rlang::sym(fsg_col)

  if (!all(c(cue_col, target_col, fsg_col) %in% names(df))) {
    stop("Data frame must contain columns: ",
         paste(c(cue_col, target_col, fsg_col), collapse = ", "))
  }

  library(dplyr)

  prep <- df |>
    mutate(
      !!cue_sym := as.character(!!cue_sym),
      !!tgt_sym := as.character(!!tgt_sym),
      !!fsg_sym := suppressWarnings(as.numeric(!!fsg_sym))
    ) |>
    filter(!is.na(!!cue_sym), !is.na(!!tgt_sym), !is.na(!!fsg_sym)) |>
    mutate(
      .cue_nchar = nchar(!!cue_sym),
      .tgt_nchar = nchar(!!tgt_sym)
    ) |>
    filter(
      .cue_nchar >= cue_min_chars,
      .cue_nchar <= cue_max_chars,
      .tgt_nchar >= target_min_chars,
      .tgt_nchar <= target_max_chars
    )

  if (nrow(prep) < n) stop("Not enough rows after filtering to sample ", n, " pairs.")
  if (dplyr::n_distinct(prep[[cue_col]]) < n)
    stop("Fewer than n unique cues after filtering.")
  if (dplyr::n_distinct(prep[[target_col]]) < n)
    stop("Fewer than n unique targets after filtering.")

  make_one_sample <- function(rows, size) {
    idx <- sample(seq_len(nrow(rows)))
    seen_cue <- new.env(parent = emptyenv())
    seen_tgt <- new.env(parent = emptyenv())
    pick <- integer(0)

    for (i in idx) {
      cval <- rows[[cue_col]][i]
      tval <- rows[[target_col]][i]
      if (!exists(cval, envir = seen_cue, inherits = FALSE) &&
          !exists(tval, envir = seen_tgt, inherits = FALSE)) {
        pick <- c(pick, i)
        assign(cval, TRUE, envir = seen_cue)
        assign(tval, TRUE, envir = seen_tgt)
        if (length(pick) == size) break
      }
    }
    if (length(pick) == size) rows[pick, , drop = FALSE] else NULL
  }

  if (!is.null(seed)) set.seed(seed)

  best_sample <- NULL
  best_diff <- Inf

  prep <- prep |>
    mutate(.abs_dev = abs(!!fsg_sym - target_mean_fsg)) |>
    arrange(.abs_dev)

  for (a in seq_len(attempts)) {
    pool_size <- min(nrow(prep), max(3 * n, n + 20))
    pool <- prep[seq_len(pool_size), , drop = FALSE]
    pool <- pool[sample.int(nrow(pool)), , drop = FALSE]

    smp <- make_one_sample(pool, n)
    if (!is.null(smp)) {
      m <- mean(smp[[fsg_col]])
      diff <- abs(m - target_mean_fsg)
      if (diff < best_diff) {
        best_diff <- diff
        best_sample <- smp
        if (best_diff == 0) break
      }
    }
  }

  if (is.null(best_sample)) {
    stop("Failed to construct a sample with unique cues and targets in ", attempts,
         " attempts. Try relaxing character limits or reducing n.")
  }

  best_sample <- best_sample |>
    select(all_of(c(cue_col, target_col, fsg_col)),
           .cue_nchar, .tgt_nchar)

  attr(best_sample, "target_mean_fsg") <- target_mean_fsg
  attr(best_sample, "achieved_mean_fsg") <- mean(best_sample[[fsg_col]])
  attr(best_sample, "mean_abs_diff") <- best_diff
  attr(best_sample, "attempts_used") <- attempts

  best_sample
}
