#' Compute per-participant d′, meta-d′ and M-ratio (metaSDT)
#'
#' A tidy, pipe-friendly wrapper around **metaSDT::fit_meta_d_MLE()** that
#' reproduces the counting logic you prototyped (s1_hi_to_lo / s2_lo_to_hi)
#' but works with arbitrary column names and coding conventions.
#'
#' @param data           Data frame; one row = one trial.
#' @param id             Name of the participant-ID column.
#' @param stim           Name of the stimulus column.
#' @param resp           Name of the response column.
#' @param conf           Name of the confidence column (integer 1…K, low→high).
#' @param stim_present   Value of *stim* that means “signal present” (S2).
#'                       All other values are treated as S1.
#' @param resp_present   Value of *resp* that means “responded signal present”.
#'                       All other values are treated as S1.
#' @param pad_zero       Constant to add to empty cells (log–linear correction).
#'                       Set 0 to skip padding.
#' @param add_constant   Passed through to **metaSDT::fit_meta_d_MLE()**.
#'
#' @return Tibble with one row per participant:
#'   \itemize{
#'     \item \code{id}          – participant identifier
#'     \item \code{d_prime}     – type-1 d′ (aka da)
#'     \item \code{meta_d_prime}– meta-d′
#'     \item \code{m_ratio}     – meta-d′ / d′
#'   }
#' @export
#'
#' @examples
#' out <- compute_meta_sdt(
#'   dots,
#'   id            = "participant_id",
#'   stim          = "target_left",
#'   resp          = "response",
#'   conf          = "confidence",
#'   stim_present  = "true",
#'   resp_present  = "w"
#' )
compute_meta_sdt <- function(data,
                             id            = "participant_id",
                             stim          = "stim",
                             resp          = "resp",
                             conf          = "conf",
                             stim_present  = "S2",
                             resp_present  = "S2",
                             pad_zero      = 0.5,
                             add_constant  = TRUE) {
  
  ## ----------------------------------------------------------------------
  ## 1.  Re-code to canonical S1 / S2 and ensure confidence is integer
  ## ----------------------------------------------------------------------
  df <- data %>%
    dplyr::mutate(
      .stim = ifelse(.data[[stim]] == stim_present, "S2", "S1"),
      .resp = ifelse(.data[[resp]] == resp_present,  "S2", "S1"),
      .conf = as.integer(.data[[conf]])
    )
  
  ## ----------------------------------------------------------------------
  ## 2.  Internal helpers (kept in your original style)
  ## ----------------------------------------------------------------------
  collapse_counts <- function(dsub, stim_label, K) {
    
    ## S1-responses, high → low confidence
    s1_hi_to_lo <- sapply(rev(seq_len(K)), function(c)
      sum(dsub$.stim == stim_label & dsub$.resp == "S1" & dsub$.conf == c))
    
    ## S2-responses, low → high confidence
    s2_lo_to_hi <- sapply(seq_len(K), function(c)
      sum(dsub$.stim == stim_label & dsub$.resp == "S2" & dsub$.conf == c))
    
    c(s1_hi_to_lo, s2_lo_to_hi)          # exactly as in your prototype
  }
  
  safe_counts <- function(x) {
    if (pad_zero > 0 && any(x == 0)) x <- x + pad_zero
    x
  }
  
  ## ----------------------------------------------------------------------
  ## 3.  Per-participant loop
  ## ----------------------------------------------------------------------
  out <- df %>%
    dplyr::group_by(.data[[id]]) %>%
    dplyr::group_map(~{
      K  <- max(.x$.conf, na.rm = TRUE)
      
      n1 <- safe_counts(collapse_counts(.x, "S1", K))
      n2 <- safe_counts(collapse_counts(.x, "S2", K))
      
      fit <- metaSDT::fit_meta_d_MLE(n1, n2, add_constant = add_constant)
      
      dplyr::tibble(
        !!id := .y[[1]],
        d_prime        = fit$da[1],
        meta_d_prime   = fit$meta_da[1],
        m_ratio        = fit$M_ratio[1]
      )
    }, .keep = TRUE) %>%
    dplyr::bind_rows()
  
  out
}
