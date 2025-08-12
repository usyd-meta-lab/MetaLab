#' Sample related or unrelated cue–target word pairs from association norms
#'
#' Draws a set of cue–target pairs from the Florida Free Association Norms
#' (or a compatible table) under constraints on association strength,
#' cross-talk isolation, and word length. Modes:
#' \itemize{
#'   \item \strong{related}: sample rows with `fsg` within `fas_range`,
#'     matching `desired_mean`/`desired_sd`.
#'   \item \strong{unrelated}: pair distinct words from the vocabulary and
#'     reject if any association (forward/backward) exceeds `unrelated_cutoff`
#'     within pairs or across pairs (cross-talk).
#' }
#'
#' @param path Path to the Excel file with columns `cue`, `target`, `fsg`, `bsg`.
#' @param n_pairs Number of pairs to return.
#' @param pair_type `"related"` or `"unrelated"`.
#' @param desired_mean,desired_sd Target mean/SD of `fsg` (related mode only).
#' @param tol_mean,tol_sd Absolute tolerances for mean/SD (related mode only).
#' @param fas_range Inclusive range of eligible `fsg` (related mode).
#' @param fas_unique_cutoff,bsg_unique_cutoff Cross-talk thresholds.
#' @param unrelated_cutoff In unrelated mode, any association strictly greater
#'   than this (forward/backward) counts as related and causes rejection.
#' @param min_char,max_char Integer bounds on allowed word length (applied to
#'   vocabulary/rows before sampling).
#' @param max_iter Maximum stochastic search iterations.
#' @param seed RNG seed.
#'
#' @return Tibble with columns `cue`, `target`, `fsg`, `bsg` and attributes.
#' @export
sample_word_pairs <- function(path              = "Florida Norms.xlsx",
                              n_pairs           = 40,
                              pair_type         = c("related", "unrelated"),
                              desired_mean      = 0.47,
                              desired_sd        = 0.14,
                              tol_mean          = 0.02,
                              tol_sd            = 0.03,
                              fas_range         = c(.30, .75),
                              fas_unique_cutoff = .05,
                              bsg_unique_cutoff = .05,
                              unrelated_cutoff  = 0,
                              min_char          = 1,
                              max_char          = Inf,
                              max_iter          = 20000,
                              seed              = 123,
                              na_strings        = c(
                                "", "NA", "N/A", "#N/A", "#N", "#NUM!", "#DIV/0!",
                                "#VALUE!", "#REF!", "#NAME?", "#NULL!", "—", "–", "-", ".", "#G", "TMC"
                              )) {
  pair_type <- match.arg(pair_type)
  set.seed(seed)
  
  # ---- internal helper: cross-talk isolation (forward + backward) ----
  is_isolated <- function(pair_set, all_assoc, fas_cut = .05, bsg_cut = .05) {
    pairs <- pair_set |>
      dplyr::select(cue, target)
    
    # Cartesian self-join (explicitly allow many-to-many)
    cross <- pairs |>
      dplyr::mutate(dummy = 1L) |>
      dplyr::inner_join(
        dplyr::mutate(pairs, dummy = 1L),
        by = "dummy",
        suffix = c("", "2"),
        relationship = "many-to-many"
      ) |>
      dplyr::filter(cue != cue2 | target != target2)
    
    # 1) cue linked to any *other* target (forward)
    cross_ct <- cross |>
      dplyr::select(cue, target_other = target2) |>
      dplyr::left_join(all_assoc, by = c("cue", "target_other" = "target")) |>
      dplyr::filter(!is.na(fsg) & fsg > fas_cut)
    
    # 2) target linked *backward* to any other cue
    cross_tc <- cross |>
      dplyr::select(cue_other = cue2, target) |>
      dplyr::left_join(all_assoc, by = c("cue_other" = "cue", "target")) |>
      dplyr::filter(!is.na(bsg) & bsg > bsg_cut)
    
    nrow(cross_ct) == 0 && nrow(cross_tc) == 0
  }
  
  # ---- load & tidy norms (quiet) ----
  df <- suppressMessages(
    readxl::read_excel(path, na = na_strings, col_types = "text", .name_repair = "minimal")
  ) |>
    janitor::clean_names() |>
    dplyr::transmute(
      cue,
      target,
      fsg = suppressWarnings(readr::parse_number(fsg, na = na_strings)),
      bsg = suppressWarnings(readr::parse_number(bsg, na = na_strings))
    ) |>
    dplyr::filter(!is.na(cue), !is.na(target)) |>
    dplyr::filter(
      nchar(cue)    >= min_char, nchar(cue)    <= max_char,
      nchar(target) >= min_char, nchar(target) <= max_char
    )
  
  # treat missing associations as zero for checks
  assoc_tbl <- df |>
    dplyr::mutate(
      fsg = tidyr::replace_na(fsg, 0),
      bsg = tidyr::replace_na(bsg, 0)
    ) |>
    dplyr::select(cue, target, fsg, bsg)
  
  # ---- candidate pool(s) ----
  if (pair_type == "related") {
    pool <- assoc_tbl |>
      dplyr::filter(fsg >= fas_range[1], fsg <= fas_range[2]) |>
      dplyr::select(cue, target, fsg, bsg)
    if (nrow(pool) < n_pairs)
      stop("Not enough candidate pairs in the requested FAS range.")
  } else {
    vocab <- union(unique(assoc_tbl$cue), unique(assoc_tbl$target))
    if (length(vocab) < 2 * n_pairs)
      stop("Not enough unique words to form ", n_pairs, " disjoint unrelated pairs.")
    
    # lookup sets for any association above cutoff
    tmp_f <- assoc_tbl |>
      dplyr::filter(fsg > unrelated_cutoff) |>
      dplyr::transmute(key = paste(cue, target, sep = "||"))
    has_forward <- stats::setNames(rep(TRUE, nrow(tmp_f)), tmp_f$key)
    
    tmp_b <- assoc_tbl |>
      dplyr::filter(bsg > unrelated_cutoff) |>
      dplyr::transmute(key = paste(target, cue, sep = "||"))  # reversed for backward
    has_backward <- stats::setNames(rep(TRUE, nrow(tmp_b)), tmp_b$key)
  }
  
  # ---- stochastic search ----
  for (i in seq_len(max_iter)) {
    
    if (pair_type == "related") {
      cand <- dplyr::slice_sample(pool, n = n_pairs)
      
      if (anyDuplicated(c(cand$cue, cand$target))) next
      if (!is_isolated(cand, assoc_tbl, fas_unique_cutoff, bsg_unique_cutoff)) next
      
      m  <- mean(cand$fsg)
      sd <- stats::sd(cand$fsg)
      if (abs(m - desired_mean) > tol_mean) next
      if (abs(sd - desired_sd) > tol_sd)   next
      
    } else {
      # unrelated: sample 2n distinct words and pair randomly
      words <- sample(vocab, size = 2 * n_pairs, replace = FALSE)
      words <- matrix(sample(words, length(words), replace = FALSE), ncol = 2, byrow = TRUE)
      cand  <- tibble::tibble(cue = words[, 1], target = words[, 2])
      
      # within-pair zero links in BOTH directions
      keys_fwd <- paste(cand$cue,    cand$target, sep = "||")
      keys_bwd <- paste(cand$target, cand$cue,    sep = "||")
      if (any(has_forward[keys_fwd] %in% TRUE,  na.rm = TRUE)) next
      if (any(has_backward[keys_bwd] %in% TRUE, na.rm = TRUE)) next
      
      # cross-talk isolation across pairs (use strictest cutoff)
      if (!is_isolated(
        cand, assoc_tbl,
        fas_cut = max(unrelated_cutoff, fas_unique_cutoff),
        bsg_cut = max(unrelated_cutoff, bsg_unique_cutoff)
      )) next
      
      cand <- dplyr::mutate(cand, fsg = 0, bsg = 0)
      m <- 0; sd <- 0
    }
    
    # success
    cand <- dplyr::arrange(cand, dplyr::desc(fsg))
    attr(cand, "mean_fas")  <- m
    attr(cand, "sd_fas")    <- sd
    attr(cand, "iter")      <- i
    attr(cand, "pair_type") <- pair_type
    attr(cand, "min_char")  <- min_char
    attr(cand, "max_char")  <- max_char
    return(cand)
  }
  
  stop("No set found in ", max_iter, " iterations. Consider relaxing constraints or increasing max_iter.")
}