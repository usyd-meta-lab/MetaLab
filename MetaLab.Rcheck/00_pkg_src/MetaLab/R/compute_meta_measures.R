#' Compute meta-d' and related metacognitive measures (Meta Measures)
#'
#' Computes the same per-participant results table produced by the **Meta
#' Measures** Shiny app included in this repository (`Meta Measures/app.R`).
#' This includes a native R implementation of the Maniscalco & Lau (2012/2014)
#' meta-d' maximum-likelihood fit and several additional trial-level metrics.
#'
#' @param data A data.frame with one row per trial.
#' @param id_col Name of the participant ID column.
#' @param stim_col Name of the stimulus column (S1 vs S2).
#' @param resp_col Name of the response column (S1 vs S2).
#' @param conf_col Name of the confidence column (discrete, ordered; low->high).
#' @param s1_stim Value in `stim_col` that indicates an S1 trial.
#' @param s2_stim Value in `stim_col` that indicates an S2 trial.
#' @param s1_resp Value in `resp_col` that indicates an S1 response.
#' @param s2_resp Value in `resp_col` that indicates an S2 response.
#' @param s_param SD ratio parameter `s` (default 1; equal-variance SDT).
#' @param n_restarts Number of random restarts for the MLE optimiser.
#' @param min_trials Minimum trials per participant required to fit (default 10).
#'
#' @return A data.frame with one row per participant and columns:
#' \itemize{
#'   \item `Participant`
#'   \item `d_prime`, `meta_d_prime`, `M_ratio`, `M_diff`
#'   \item `delta_conf`, `phi`, `gamma`, `auc2`
#'   \item `bias`, `abs_accuracy`, `discrimination`, `scatter`
#'   \item `logL`, `note`
#' }
#' Values are rounded to match the Shiny app output.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' res <- compute_meta_measures(
#'   data = df,
#'   id_col = "participant_id",
#'   stim_col = "stimulus",
#'   resp_col = "response",
#'   conf_col = "confidence",
#'   s1_stim = "S1",
#'   s2_stim = "S2",
#'   s1_resp = "S1",
#'   s2_resp = "S2"
#' )
#' }
compute_meta_measures <- function(data,
                                  id_col,
                                  stim_col,
                                  resp_col,
                                  conf_col,
                                  s1_stim,
                                  s2_stim,
                                  s1_resp,
                                  s2_resp,
                                  s_param = 1,
                                  n_restarts = 3,
                                  min_trials = 10) {
  stopifnot(is.data.frame(data))

  required <- c(id_col, stim_col, resp_col, conf_col)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in `data`: ", paste(missing_cols, collapse = ", "))
  }

  if (identical(as.character(s1_stim), as.character(s2_stim))) {
    stop("`s1_stim` and `s2_stim` must differ.")
  }
  if (identical(as.character(s1_resp), as.character(s2_resp))) {
    stop("`s1_resp` and `s2_resp` must differ.")
  }

  .run_analysis(
    data = data,
    id_col = id_col,
    stim_col = stim_col,
    resp_col = resp_col,
    conf_col = conf_col,
    s1_stim = s1_stim,
    s2_stim = s2_stim,
    s1_resp = s1_resp,
    s2_resp = s2_resp,
    s_param = s_param,
    n_restarts = n_restarts,
    min_trials = min_trials
  )
}

.EMPTY_ROW <- function(pid, note = "") {
  data.frame(
    Participant = pid,
    d_prime = NA_real_,
    meta_d_prime = NA_real_,
    M_ratio = NA_real_,
    M_diff = NA_real_,
    delta_conf = NA_real_,
    phi = NA_real_,
    gamma = NA_real_,
    auc2 = NA_real_,
    bias = NA_real_,
    abs_accuracy = NA_real_,
    discrimination = NA_real_,
    scatter = NA_real_,
    logL = NA_real_,
    note = note,
    stringsAsFactors = FALSE
  )
}

.compute_gamma <- function(conf, correct) {
  c_corr <- conf[correct]
  c_err <- conf[!correct]
  if (length(c_corr) == 0 || length(c_err) == 0) return(NA_real_)
  C <- sum(outer(c_corr, c_err, `>`))
  D <- sum(outer(c_corr, c_err, `<`))
  if (C + D == 0) return(NA_real_)
  (C - D) / (C + D)
}

.compute_auc2_empirical <- function(conf, correct, conf_levels) {
  n_corr <- sum(correct)
  n_err <- sum(!correct)
  if (n_corr == 0 || n_err == 0) return(NA_real_)

  thresholds <- conf_levels[-1]
  HR2 <- vapply(thresholds, function(t) sum(conf[correct] >= t) / n_corr, numeric(1))
  FAR2 <- vapply(thresholds, function(t) sum(conf[!correct] >= t) / n_err, numeric(1))

  ord <- order(FAR2)
  x <- c(0, FAR2[ord], 1)
  y <- c(0, HR2[ord], 1)
  sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
}

.compute_simple_metrics <- function(pdata,
                                    stim_col,
                                    resp_col,
                                    conf_col,
                                    s1_stim,
                                    s2_stim,
                                    s1_resp,
                                    s2_resp,
                                    conf_levels) {
  stim <- as.character(pdata[[stim_col]])
  resp <- as.character(pdata[[resp_col]])
  conf <- suppressWarnings(as.numeric(as.character(pdata[[conf_col]])))
  if (anyNA(conf)) return(NULL)

  correct <- (stim == as.character(s1_stim) & resp == as.character(s1_resp)) |
    (stim == as.character(s2_stim) & resp == as.character(s2_resp))

  n <- length(correct)
  n_corr <- sum(correct)
  n_err <- n - n_corr

  delta_conf <- if (n_corr > 0 && n_err > 0) mean(conf[correct]) - mean(conf[!correct]) else NA_real_
  phi <- if (stats::sd(conf) > 0) stats::cor(conf, as.numeric(correct)) else NA_real_
  gamma <- .compute_gamma(conf, correct)
  auc2 <- .compute_auc2_empirical(conf, correct, conf_levels)

  c_min <- min(conf_levels)
  c_max <- max(conf_levels)
  conf_norm <- (conf - c_min) / max(c_max - c_min, 1e-9)
  acc_01 <- as.numeric(correct)

  bias <- mean(conf_norm - acc_01)
  abs_accuracy <- mean((conf_norm - acc_01)^2)
  discrimination <- if (n_corr > 0 && n_err > 0) {
    (n_corr * mean(conf[correct]) - n_err * mean(conf[!correct])) / n
  } else {
    NA_real_
  }
  scatter <- if (n_corr > 1 && n_err > 1) {
    (n_corr * stats::var(conf[correct]) - n_err * stats::var(conf[!correct])) / n
  } else {
    NA_real_
  }

  list(
    delta_conf = delta_conf,
    phi = phi,
    gamma = gamma,
    auc2 = auc2,
    bias = bias,
    abs_accuracy = abs_accuracy,
    discrimination = discrimination,
    scatter = scatter
  )
}

.trials_to_counts <- function(data,
                              stim_col,
                              resp_col,
                              conf_col,
                              s1_stim,
                              s2_stim,
                              s1_resp,
                              s2_resp,
                              conf_levels) {
  nRatings <- length(conf_levels)
  nR_S1 <- numeric(2 * nRatings)
  nR_S2 <- numeric(2 * nRatings)

  stim <- as.character(data[[stim_col]])
  resp <- as.character(data[[resp_col]])
  conf <- data[[conf_col]]

  s1_mask <- stim == as.character(s1_stim)
  s2_mask <- stim == as.character(s2_stim)

  for (i in seq_len(nRatings)) {
    cl_high <- conf_levels[nRatings - i + 1]
    cl_low <- conf_levels[i]

    nR_S1[i] <- sum(s1_mask & resp == as.character(s1_resp) & conf == cl_high)
    nR_S1[nRatings + i] <- sum(s1_mask & resp == as.character(s2_resp) & conf == cl_low)

    nR_S2[i] <- sum(s2_mask & resp == as.character(s1_resp) & conf == cl_high)
    nR_S2[nRatings + i] <- sum(s2_mask & resp == as.character(s2_resp) & conf == cl_low)
  }

  list(nR_S1 = nR_S1, nR_S2 = nR_S2)
}

.meta_d_logL <- function(params, nR_S1, nR_S2, nRatings, d1, t1c1, s) {
  meta_d1 <- params[1]
  t2c1_shifted <- params[-1]
  if (abs(d1) < 1e-9) return(1e6)

  const_crit <- meta_d1 * (t1c1 / d1)
  S1mu <- -meta_d1 / 2 - const_crit
  S1sd <- 1
  S2mu <- meta_d1 / 2 - const_crit
  S2sd <- S1sd / s

  nC_rS1 <- nR_S1[seq_len(nRatings)]
  nI_rS1 <- nR_S2[seq_len(nRatings)]
  nC_rS2 <- nR_S2[nRatings + seq_len(nRatings)]
  nI_rS2 <- nR_S1[nRatings + seq_len(nRatings)]

  C_area_rS1 <- stats::pnorm(0, S1mu, S1sd)
  I_area_rS1 <- stats::pnorm(0, S2mu, S2sd)
  C_area_rS2 <- stats::pnorm(0, S2mu, S2sd, lower.tail = FALSE)
  I_area_rS2 <- stats::pnorm(0, S1mu, S1sd, lower.tail = FALSE)
  if (any(c(C_area_rS1, I_area_rS1, C_area_rS2, I_area_rS2) <= 0)) return(1e6)

  t2c1x <- c(
    -Inf,
    t2c1_shifted[seq_len(nRatings - 1)],
    0,
    t2c1_shifted[nRatings:(2 * nRatings - 2)],
    Inf
  )

  prC_rS1 <- prI_rS1 <- prC_rS2 <- prI_rS2 <- numeric(nRatings)
  for (i in seq_len(nRatings)) {
    prC_rS1[i] <- (stats::pnorm(t2c1x[i + 1], S1mu, S1sd) - stats::pnorm(t2c1x[i], S1mu, S1sd)) /
      C_area_rS1
    prI_rS1[i] <- (stats::pnorm(t2c1x[i + 1], S2mu, S2sd) - stats::pnorm(t2c1x[i], S2mu, S2sd)) /
      I_area_rS1
    prC_rS2[i] <- (stats::pnorm(t2c1x[nRatings + i], S2mu, S2sd, lower.tail = FALSE) -
      stats::pnorm(t2c1x[nRatings + i + 1], S2mu, S2sd, lower.tail = FALSE)) /
      C_area_rS2
    prI_rS2[i] <- (stats::pnorm(t2c1x[nRatings + i], S1mu, S1sd, lower.tail = FALSE) -
      stats::pnorm(t2c1x[nRatings + i + 1], S1mu, S1sd, lower.tail = FALSE)) /
      I_area_rS2
  }

  prC_rS1 <- pmax(prC_rS1, 1e-10)
  prI_rS1 <- pmax(prI_rS1, 1e-10)
  prC_rS2 <- pmax(prC_rS2, 1e-10)
  prI_rS2 <- pmax(prI_rS2, 1e-10)

  logL <- sum(
    nC_rS1 * log(prC_rS1) +
      nI_rS1 * log(prI_rS1) +
      nC_rS2 * log(prC_rS2) +
      nI_rS2 * log(prI_rS2)
  )
  if (!is.finite(logL)) return(1e6)
  -logL
}

.fit_meta_d_MLE_R <- function(nR_S1, nR_S2, s = 1, n_restarts = 3) {
  nRatings <- length(nR_S1) / 2
  nCriteria <- 2 * nRatings - 1
  nLow <- (nCriteria - 1) / 2
  if (nRatings < 2) stop("Need at least 2 rating levels.")

  adj_f <- 1 / (2 * nRatings)
  nR_S1 <- nR_S1 + adj_f
  nR_S2 <- nR_S2 + adj_f

  ratingHR <- numeric(2 * nRatings - 1)
  ratingFAR <- numeric(2 * nRatings - 1)
  for (c_i in seq(2, 2 * nRatings)) {
    ratingHR[c_i - 1] <- sum(nR_S2[c_i:(2 * nRatings)]) / sum(nR_S2)
    ratingFAR[c_i - 1] <- sum(nR_S1[c_i:(2 * nRatings)]) / sum(nR_S1)
  }

  ratingHR <- pmax(pmin(ratingHR, 1 - 1e-7), 1e-7)
  ratingFAR <- pmax(pmin(ratingFAR, 1 - 1e-7), 1e-7)

  t1_idx <- nRatings
  d1 <- (1 / s) * stats::qnorm(ratingHR[t1_idx]) - stats::qnorm(ratingFAR[t1_idx])
  if (!is.finite(d1) || abs(d1) < 1e-6) d1 <- 0.01

  c1 <- (-1 / (1 + s)) * (stats::qnorm(ratingHR) + stats::qnorm(ratingFAR))
  t1c1 <- c1[t1_idx]
  t2c1 <- c1[setdiff(seq_len(2 * nRatings - 1), t1_idx)]

  guess <- c(d1, t2c1 - t1c1)

  LB <- c(-10, rep(-20, nLow), rep(0, nLow))
  UB <- c(10, rep(0, nLow), rep(20, nLow))

  guess <- pmax(pmin(guess, UB - 1e-4), LB + 1e-4)
  for (j in seq(2, length(guess) - 1)) {
    if (guess[j + 1] <= guess[j] + 1e-4) guess[j + 1] <- guess[j] + 1e-4
  }
  guess <- pmax(pmin(guess, UB - 1e-4), LB + 1e-4)

  obj_penalised <- function(params) {
    t2c_s <- params[-1]
    penalty <- 0
    if (length(t2c_s) > 1) {
      viol <- diff(t2c_s) - 1e-5
      bad <- viol[viol < 0]
      if (length(bad) > 0) penalty <- 1e6 * sum(bad^2)
    }
    .meta_d_logL(params, nR_S1, nR_S2, nRatings, d1, t1c1, s) + penalty
  }

  run_optim <- function(start) {
    tryCatch(
      stats::optim(
        par = start,
        fn = obj_penalised,
        method = "L-BFGS-B",
        lower = LB,
        upper = UB,
        control = list(maxit = 2000, factr = 1e7)
      ),
      error = function(e) NULL
    )
  }

  candidates <- list(run_optim(guess))
  if (n_restarts > 1) {
    set.seed(42)
    for (r in seq_len(n_restarts - 1)) {
      lo <- sort(stats::runif(nLow, LB[2], UB[2] - 1e-4))
      hi <- sort(stats::runif(nLow, LB[nLow + 2], UB[nLow + 2] - 1e-4) + 1e-4)
      rnd_start <- pmax(pmin(c(stats::runif(1, -3, 3), lo, hi), UB - 1e-4), LB + 1e-4)
      candidates[[r + 1]] <- run_optim(rnd_start)
    }
  }

  valid <- Filter(Negate(is.null), candidates)
  if (length(valid) == 0) return(NULL)

  result <- valid[[which.min(vapply(valid, `[[`, numeric(1), "value"))]]
  meta_d1 <- result$par[1]
  logL <- -result$value

  da <- sqrt(2 / (1 + s^2)) * s * d1
  meta_da <- sqrt(2 / (1 + s^2)) * s * meta_d1

  list(
    da = da,
    meta_da = meta_da,
    M_diff = meta_da - da,
    M_ratio = meta_da / da,
    d1 = d1,
    meta_d1 = meta_d1,
    logL = logL
  )
}

.run_analysis <- function(data,
                          id_col,
                          stim_col,
                          resp_col,
                          conf_col,
                          s1_stim,
                          s2_stim,
                          s1_resp,
                          s2_resp,
                          s_param,
                          n_restarts = 3,
                          min_trials = 10) {
  conf_levels <- sort(unique(data[[conf_col]]))
  participants <- sort(unique(data[[id_col]]))

  results <- lapply(participants, function(pid) {
    pdata <- data[data[[id_col]] == pid, , drop = FALSE]
    if (nrow(pdata) < min_trials) return(.EMPTY_ROW(pid, sprintf("Too few trials (<%d)", min_trials)))

    simple <- tryCatch(
      .compute_simple_metrics(
        pdata = pdata,
        stim_col = stim_col,
        resp_col = resp_col,
        conf_col = conf_col,
        s1_stim = s1_stim,
        s2_stim = s2_stim,
        s1_resp = s1_resp,
        s2_resp = s2_resp,
        conf_levels = conf_levels
      ),
      error = function(e) NULL
    )

    counts <- tryCatch(
      .trials_to_counts(
        data = pdata,
        stim_col = stim_col,
        resp_col = resp_col,
        conf_col = conf_col,
        s1_stim = s1_stim,
        s2_stim = s2_stim,
        s1_resp = s1_resp,
        s2_resp = s2_resp,
        conf_levels = conf_levels
      ),
      error = function(e) NULL
    )
    if (is.null(counts)) return(.EMPTY_ROW(pid, "Count error"))

    fit <- tryCatch(
      .fit_meta_d_MLE_R(counts$nR_S1, counts$nR_S2, s = s_param, n_restarts = n_restarts),
      error = function(e) NULL
    )

    row <- .EMPTY_ROW(pid, if (is.null(fit)) "SDT fit failed" else "")

    if (!is.null(simple)) {
      row$delta_conf <- round(simple$delta_conf, 4)
      row$phi <- round(simple$phi, 4)
      row$gamma <- round(simple$gamma, 4)
      row$auc2 <- round(simple$auc2, 4)
      row$bias <- round(simple$bias, 4)
      row$abs_accuracy <- round(simple$abs_accuracy, 4)
      row$discrimination <- round(simple$discrimination, 4)
      row$scatter <- round(simple$scatter, 4)
    }
    if (!is.null(fit)) {
      row$d_prime <- round(fit$da, 4)
      row$meta_d_prime <- round(fit$meta_da, 4)
      row$M_ratio <- round(fit$M_ratio, 4)
      row$M_diff <- round(fit$M_diff, 4)
      row$logL <- round(fit$logL, 2)
    }

    row
  })

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out
}

