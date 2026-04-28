# ============================================================
# Meta-d' Shiny App
# Implements fit_meta_d_MLE (Maniscalco & Lau) natively in R
# ============================================================
# Required: install.packages(c("shiny","bslib","bsicons","DT","ggplot2","dplyr"))

library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(ggplot2)
library(dplyr)
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# ============================================================
# Core: Log-likelihood for meta-d' MLE
# ============================================================

meta_d_logL <- function(params, nR_S1, nR_S2, nRatings, d1, t1c1, s) {
  meta_d1      <- params[1]
  t2c1_shifted <- params[-1]
  if (abs(d1) < 1e-9) return(1e6)
  const_crit <- meta_d1 * (t1c1 / d1)
  S1mu <- -meta_d1 / 2 - const_crit;  S1sd <- 1
  S2mu <-  meta_d1 / 2 - const_crit;  S2sd <- S1sd / s
  nC_rS1 <- nR_S1[seq_len(nRatings)];  nI_rS1 <- nR_S2[seq_len(nRatings)]
  nC_rS2 <- nR_S2[nRatings + seq_len(nRatings)];  nI_rS2 <- nR_S1[nRatings + seq_len(nRatings)]
  C_area_rS1 <- pnorm(0, S1mu, S1sd);  I_area_rS1 <- pnorm(0, S2mu, S2sd)
  C_area_rS2 <- pnorm(0, S2mu, S2sd, lower.tail = FALSE)
  I_area_rS2 <- pnorm(0, S1mu, S1sd, lower.tail = FALSE)
  if (any(c(C_area_rS1, I_area_rS1, C_area_rS2, I_area_rS2) <= 0)) return(1e6)
  t2c1x <- c(-Inf, t2c1_shifted[seq_len(nRatings - 1)], 0,
             t2c1_shifted[nRatings:(2 * nRatings - 2)], Inf)
  prC_rS1 <- prI_rS1 <- prC_rS2 <- prI_rS2 <- numeric(nRatings)
  for (i in seq_len(nRatings)) {
    prC_rS1[i] <- (pnorm(t2c1x[i+1], S1mu, S1sd) - pnorm(t2c1x[i], S1mu, S1sd)) / C_area_rS1
    prI_rS1[i] <- (pnorm(t2c1x[i+1], S2mu, S2sd) - pnorm(t2c1x[i], S2mu, S2sd)) / I_area_rS1
    prC_rS2[i] <- (pnorm(t2c1x[nRatings+i],   S2mu, S2sd, lower.tail = FALSE) -
                   pnorm(t2c1x[nRatings+i+1], S2mu, S2sd, lower.tail = FALSE)) / C_area_rS2
    prI_rS2[i] <- (pnorm(t2c1x[nRatings+i],   S1mu, S1sd, lower.tail = FALSE) -
                   pnorm(t2c1x[nRatings+i+1], S1mu, S1sd, lower.tail = FALSE)) / I_area_rS2
  }
  prC_rS1 <- pmax(prC_rS1, 1e-10);  prI_rS1 <- pmax(prI_rS1, 1e-10)
  prC_rS2 <- pmax(prC_rS2, 1e-10);  prI_rS2 <- pmax(prI_rS2, 1e-10)
  logL <- sum(nC_rS1*log(prC_rS1) + nI_rS1*log(prI_rS1) +
              nC_rS2*log(prC_rS2) + nI_rS2*log(prI_rS2))
  if (!is.finite(logL)) return(1e6)
  return(-logL)
}

# ============================================================
# Core: fit_meta_d_MLE (Maniscalco & Lau, translated to R)
# ============================================================

fit_meta_d_MLE_R <- function(nR_S1, nR_S2, s = 1, n_restarts = 3) {
  nRatings  <- length(nR_S1) / 2
  nCriteria <- 2 * nRatings - 1
  nLow      <- (nCriteria - 1) / 2
  if (nRatings < 2) stop("Need at least 2 rating levels.")
  # Always pad all cells (per M&L recommendation to avoid bias; Snodgrass & Corwin, 1988)
  adj_f <- 1 / (2 * nRatings)
  nR_S1 <- nR_S1 + adj_f;  nR_S2 <- nR_S2 + adj_f
  ratingHR  <- numeric(2 * nRatings - 1);  ratingFAR <- numeric(2 * nRatings - 1)
  for (c_i in seq(2, 2 * nRatings)) {
    ratingHR[c_i - 1]  <- sum(nR_S2[c_i:(2 * nRatings)]) / sum(nR_S2)
    ratingFAR[c_i - 1] <- sum(nR_S1[c_i:(2 * nRatings)]) / sum(nR_S1)
  }
  ratingHR  <- pmax(pmin(ratingHR,  1 - 1e-7), 1e-7)
  ratingFAR <- pmax(pmin(ratingFAR, 1 - 1e-7), 1e-7)
  t1_idx <- nRatings
  d1     <- (1 / s) * qnorm(ratingHR[t1_idx]) - qnorm(ratingFAR[t1_idx])
  if (!is.finite(d1) || abs(d1) < 1e-6) d1 <- 0.01
  c1   <- (-1 / (1 + s)) * (qnorm(ratingHR) + qnorm(ratingFAR))
  t1c1 <- c1[t1_idx]
  t2c1 <- c1[setdiff(seq_len(2 * nRatings - 1), t1_idx)]
  guess <- c(d1, t2c1 - t1c1)
  LB <- c(-10, rep(-20, nLow), rep(0,  nLow))
  UB <- c( 10, rep(  0, nLow), rep(20, nLow))
  guess <- pmax(pmin(guess, UB - 1e-4), LB + 1e-4)
  for (j in seq(2, length(guess) - 1)) {
    if (guess[j + 1] <= guess[j] + 1e-4) guess[j + 1] <- guess[j] + 1e-4
  }
  guess <- pmax(pmin(guess, UB - 1e-4), LB + 1e-4)
  obj_penalised <- function(params) {
    t2c_s <- params[-1];  penalty <- 0
    if (length(t2c_s) > 1) {
      viol <- diff(t2c_s) - 1e-5;  bad <- viol[viol < 0]
      if (length(bad) > 0) penalty <- 1e6 * sum(bad^2)
    }
    meta_d_logL(params, nR_S1, nR_S2, nRatings, d1, t1c1, s) + penalty
  }
  run_optim <- function(start) {
    tryCatch(
      optim(par = start, fn = obj_penalised, method = "L-BFGS-B",
            lower = LB, upper = UB, control = list(maxit = 2000, factr = 1e7)),
      error = function(e) NULL
    )
  }
  candidates <- list(run_optim(guess))
  if (n_restarts > 1) {
    set.seed(42)
    for (r in seq_len(n_restarts - 1)) {
      lo        <- sort(runif(nLow, LB[2],        UB[2]         - 1e-4))
      hi        <- sort(runif(nLow, LB[nLow + 2], UB[nLow + 2] - 1e-4) + 1e-4)
      rnd_start <- pmax(pmin(c(runif(1, -3, 3), lo, hi), UB - 1e-4), LB + 1e-4)
      candidates[[r + 1]] <- run_optim(rnd_start)
    }
  }
  valid  <- Filter(Negate(is.null), candidates)
  if (length(valid) == 0) return(NULL)
  result  <- valid[[which.min(sapply(valid, `[[`, "value"))]]
  meta_d1 <- result$par[1]
  logL    <- -result$value
  da      <- sqrt(2 / (1 + s^2)) * s * d1
  meta_da <- sqrt(2 / (1 + s^2)) * s * meta_d1
  list(da = da, meta_da = meta_da, M_diff = meta_da - da, M_ratio = meta_da / da,
       d1 = d1, meta_d1 = meta_d1, logL = logL)
}

# ============================================================
# Helper: Goodman-Kruskal Gamma
# ============================================================

compute_gamma <- function(conf, correct) {
  c_corr <- conf[correct];  c_err <- conf[!correct]
  if (length(c_corr) == 0 || length(c_err) == 0) return(NA_real_)
  C <- sum(outer(c_corr, c_err, `>`))
  D <- sum(outer(c_corr, c_err, `<`))
  if (C + D == 0) return(NA_real_)
  (C - D) / (C + D)
}

# ============================================================
# Helper: Empirical AUC2 (Type 2 ROC area)
# ============================================================

compute_auc2_empirical <- function(conf, correct, conf_levels) {
  n_corr <- sum(correct);  n_err <- sum(!correct)
  if (n_corr == 0 || n_err == 0) return(NA_real_)
  thresholds <- conf_levels[-1]
  HR2  <- sapply(thresholds, function(t) sum(conf[correct]  >= t) / n_corr)
  FAR2 <- sapply(thresholds, function(t) sum(conf[!correct] >= t) / n_err)
  ord <- order(FAR2)
  x   <- c(0, FAR2[ord], 1)
  y   <- c(0,  HR2[ord], 1)
  sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
}

# ============================================================
# Helper: Simple trial-level metrics (no SDT fitting)
# ============================================================

compute_simple_metrics <- function(pdata, stim_col, resp_col, conf_col,
                                    s1_stim, s2_stim, s1_resp, s2_resp,
                                    conf_levels) {
  stim    <- as.character(pdata[[stim_col]])
  resp    <- as.character(pdata[[resp_col]])
  conf    <- as.numeric(pdata[[conf_col]])
  correct <- (stim == s1_stim & resp == s1_resp) |
             (stim == s2_stim & resp == s2_resp)
  n       <- length(correct)
  n_corr  <- sum(correct)
  n_err   <- n - n_corr

  delta_conf     <- if (n_corr > 0 && n_err > 0)
                      mean(conf[correct]) - mean(conf[!correct]) else NA_real_
  phi            <- if (sd(conf) > 0) cor(conf, as.numeric(correct)) else NA_real_
  gamma          <- compute_gamma(conf, correct)
  auc2           <- compute_auc2_empirical(conf, correct, conf_levels)

  # Schraw (2009) metrics — normalize confidence to [0, 1]
  c_min     <- min(conf_levels);  c_max <- max(conf_levels)
  conf_norm <- (conf - c_min) / max(c_max - c_min, 1e-9)
  acc_01    <- as.numeric(correct)

  bias           <- mean(conf_norm - acc_01)
  abs_accuracy   <- mean((conf_norm - acc_01)^2)
  discrimination <- if (n_corr > 0 && n_err > 0)
                      (n_corr * mean(conf[correct]) - n_err * mean(conf[!correct])) / n
                    else NA_real_
  scatter        <- if (n_corr > 1 && n_err > 1)
                      (n_corr * var(conf[correct]) - n_err * var(conf[!correct])) / n
                    else NA_real_

  list(delta_conf = delta_conf, phi = phi, gamma = gamma, auc2 = auc2,
       bias = bias, abs_accuracy = abs_accuracy,
       discrimination = discrimination, scatter = scatter)
}

# ============================================================
# Data helper: trial rows → nR_S1 / nR_S2 vectors
# ============================================================

trials_to_counts <- function(data, stim_col, resp_col, conf_col,
                              s1_stim, s2_stim, s1_resp, s2_resp, conf_levels) {
  nRatings <- length(conf_levels)
  nR_S1    <- numeric(2 * nRatings);  nR_S2 <- numeric(2 * nRatings)
  stim <- as.character(data[[stim_col]]);  resp <- as.character(data[[resp_col]])
  conf <- data[[conf_col]]
  s1_mask <- stim == as.character(s1_stim);  s2_mask <- stim == as.character(s2_stim)
  for (i in seq_len(nRatings)) {
    cl_high <- conf_levels[nRatings - i + 1];  cl_low <- conf_levels[i]
    nR_S1[i]            <- sum(s1_mask & resp == s1_resp & conf == cl_high)
    nR_S1[nRatings + i] <- sum(s1_mask & resp == s2_resp & conf == cl_low)
    nR_S2[i]            <- sum(s2_mask & resp == s1_resp & conf == cl_high)
    nR_S2[nRatings + i] <- sum(s2_mask & resp == s2_resp & conf == cl_low)
  }
  list(nR_S1 = nR_S1, nR_S2 = nR_S2)
}

# ============================================================
# Run analysis across all participants
# ============================================================

EMPTY_ROW <- function(pid, note = "") {
  data.frame(Participant = pid, d_prime = NA_real_, meta_d_prime = NA_real_,
             M_ratio = NA_real_, M_diff = NA_real_, delta_conf = NA_real_,
             phi = NA_real_, gamma = NA_real_, auc2 = NA_real_,
             bias = NA_real_, abs_accuracy = NA_real_,
             discrimination = NA_real_, scatter = NA_real_,
             logL = NA_real_, note = note, stringsAsFactors = FALSE)
}

run_analysis <- function(data, id_col, stim_col, resp_col, conf_col,
                         s1_stim, s2_stim, s1_resp, s2_resp, s_param,
                         n_restarts = 3, progress_cb = NULL) {
  conf_levels  <- sort(unique(data[[conf_col]]))
  participants <- sort(unique(data[[id_col]]))
  n_part       <- length(participants)

  results <- lapply(seq_along(participants), function(idx) {
    pid   <- participants[idx]
    pdata <- data[data[[id_col]] == pid, ]
    if (!is.null(progress_cb)) progress_cb(idx, n_part, pid)

    if (nrow(pdata) < 10) return(EMPTY_ROW(pid, "Too few trials (<10)"))

    simple <- tryCatch(
      compute_simple_metrics(pdata, stim_col, resp_col, conf_col,
                              s1_stim, s2_stim, s1_resp, s2_resp, conf_levels),
      error = function(e) NULL
    )

    counts <- tryCatch(
      trials_to_counts(pdata, stim_col, resp_col, conf_col,
                       s1_stim, s2_stim, s1_resp, s2_resp, conf_levels),
      error = function(e) NULL
    )
    if (is.null(counts)) return(EMPTY_ROW(pid, "Count error"))

    fit <- tryCatch(
      fit_meta_d_MLE_R(counts$nR_S1, counts$nR_S2, s = s_param,
                       n_restarts = n_restarts),
      error = function(e) NULL
    )

    row <- EMPTY_ROW(pid, if (is.null(fit)) "SDT fit failed" else "")

    if (!is.null(simple)) {
      row$delta_conf     <- round(simple$delta_conf,     4)
      row$phi            <- round(simple$phi,            4)
      row$gamma          <- round(simple$gamma,          4)
      row$auc2           <- round(simple$auc2,           4)
      row$bias           <- round(simple$bias,           4)
      row$abs_accuracy   <- round(simple$abs_accuracy,   4)
      row$discrimination <- round(simple$discrimination, 4)
      row$scatter        <- round(simple$scatter,        4)
    }
    if (!is.null(fit)) {
      row$d_prime      <- round(fit$da,      4)
      row$meta_d_prime <- round(fit$meta_da, 4)
      row$M_ratio      <- round(fit$M_ratio, 4)
      row$M_diff       <- round(fit$M_diff,  4)
      row$logL         <- round(fit$logL,    2)
    }
    row
  })

  do.call(rbind, results)
}

# ============================================================
# Summary helper
# ============================================================

make_summary_row <- function(res, col, label, mu0 = NULL) {
  v <- res[[col]];  v <- v[!is.na(v) & is.finite(v)];  n <- length(v)
  t_str <- p_str <- ""
  if (!is.null(mu0) && n >= 2) {
    tt    <- suppressWarnings(t.test(v, mu = mu0))
    t_str <- sprintf("%.3f", tt$statistic)
    p_str <- sprintf("%.3f", tt$p.value)
  }
  data.frame(Metric = label, N = n,
             Mean   = if (n > 0) round(mean(v),   3) else NA,
             SD     = if (n > 1) round(sd(v),     3) else NA,
             Median = if (n > 0) round(median(v), 3) else NA,
             Min    = if (n > 0) round(min(v),    3) else NA,
             Max    = if (n > 0) round(max(v),    3) else NA,
             `t vs ref` = t_str, `p vs ref` = p_str,
             check.names = FALSE, stringsAsFactors = FALSE)
}

# ============================================================
# UI
# ============================================================

ui <- page_navbar(
  title      = "Meta-d\u2019 Analysis",
  theme      = bs_theme(bootswatch = "flatly", primary = "#2c7bb6"),
  window_title = "Meta-d' App",
  header     = tags$head(tags$style(HTML(
    ".metric-group { font-size:0.78rem; color:#666; }
     .formula-box  { background:#f8f9fa; border-left:4px solid #2c7bb6;
                     padding:8px 14px; border-radius:4px; margin:6px 0; }
     .ref-box      { font-size:0.82rem; color:#555; margin-top:4px; }
     .badge-group  { display:inline-block; background:#e8f0fe; color:#1a56db;
                     border-radius:4px; padding:1px 7px; font-size:0.75rem; }"
  ))),

  # ---- Setup ----
  nav_panel(
    title = "Setup", icon = icon("upload"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        fileInput("file", "Upload CSV", accept = c(".csv","text/csv"),
                  buttonLabel = "Browse", placeholder = "No file selected"),
        hr(),
        h6("Column mapping", class = "fw-bold text-secondary"),
        selectInput("col_id",   "Participant ID", NULL),
        selectInput("col_stim", "Stimulus",       NULL),
        selectInput("col_resp", "Response",       NULL),
        selectInput("col_conf", "Confidence",     NULL),
        hr(),
        h6("Stimulus values", class = "fw-bold text-secondary"),
        div(class = "d-flex gap-2",
          div(class = "flex-fill", selectInput("s1_stim_val", "S1", NULL)),
          div(class = "flex-fill", selectInput("s2_stim_val", "S2", NULL))
        ),
        h6("Response values", class = "fw-bold text-secondary"),
        div(class = "d-flex gap-2",
          div(class = "flex-fill", selectInput("s1_resp_val", "S1", NULL)),
          div(class = "flex-fill", selectInput("s2_resp_val", "S2", NULL))
        ),
        hr(),
        h6("Model parameters", class = "fw-bold text-secondary"),
        numericInput("s_param",
          HTML("s &nbsp;<small class='text-muted'>(SD ratio, default=1)</small>"),
          value = 1, min = 0.1, max = 10, step = 0.1),
        numericInput("n_restarts",
          HTML("Restarts &nbsp;<small class='text-muted'>(random starts per participant)</small>"),
          value = 3, min = 1, max = 20, step = 1),
        actionButton("run_btn", "Run Analysis",
                     icon = icon("play"), class = "btn-primary w-100 mt-3"),
        hr(),
        uiOutput("status_ui")
      ),
      card(
        card_header("Data preview",
          tooltip(bs_icon("info-circle"),
                  "Showing up to 200 rows. All rows are used for analysis.")),
        DTOutput("data_preview"), style = "overflow-x:auto;"
      )
    )
  ),

  # ---- Results ----
  nav_panel(title = "Results", icon = icon("table"), uiOutput("results_ui")),

  # ---- Plots ----
  nav_panel(title = "Plots", icon = icon("chart-bar"), uiOutput("plots_ui")),

  # ---- Guide ----
  nav_panel(
    title = "Guide", icon = icon("book-open"),
    withMathJax(),
    layout_columns(col_widths = 12,
      card(card_header(HTML("<b>Measures of Metacognition \u2014 Conceptual & Mathematical Overview</b>")),
        card_body(

          # ----- SDT block -----
          tags$h5(class="mt-1 mb-2",
            tags$span(class="badge-group","SDT-based"),
            " Signal Detection Theory Measures"),
          tags$p(class="text-muted small",
            "SDT measures assume internal evidence follows normal distributions.
             They disentangle metacognitive sensitivity from bias."),

          accordion(open = FALSE, id = "acc_sdt",

            accordion_panel("d\u2019 \u2014 Type 1 sensitivity",
              tags$p("The standard measure of perceptual discrimination ability. Quantifies how well the observer separates the two stimulus distributions."),
              div(class="formula-box", helpText("$$d' = \\Phi^{-1}(H) - \\Phi^{-1}(F)$$")),
              tags$p(class="small", "where H = hit rate, F = false-alarm rate, \\(\\Phi^{-1}\\) = probit function."),
              div(class="ref-box", HTML("&#x1F4DA; Green, D. M., & Swets, J. A. (1966). <em>Signal detection theory and psychophysics.</em> Wiley."))
            ),

            accordion_panel("meta-d\u2019 \u2014 Type 2 sensitivity",
              tags$p("The value of d\u2019 that a hypothetical ideal observer would need to produce the observed pattern of Type 2 (confidence) responses. Fit via maximum likelihood to the Type 2 ROC curve."),
              div(class="formula-box", helpText("$$\\text{meta-}d' = \\underset{m}{\\arg\\max}\\; \\ell(\\text{Type 2 data} \\mid m,\\, d',\\, c)$$")),
              tags$p(class="small", "where \\(\\ell\\) is the log-likelihood under the SDT model with metacognitive sensitivity \\(m\\), type 1 sensitivity \\(d'\\), and criteria \\(c\\). Implemented via L-BFGS-B MLE."),
              div(class="ref-box", HTML("&#x1F4DA; Maniscalco, B., & Lau, H. (2012). A signal detection theoretic approach for estimating metacognitive sensitivity from confidence ratings. <em>Consciousness & Cognition, 21</em>, 422\u2013430."))
            ),

            accordion_panel("M-Ratio = meta-d\u2019 / d\u2019",
              tags$p("Normalises metacognitive sensitivity by task performance. Values near 1 indicate ideal metacognition; values < 1 suggest information loss in the metacognitive system."),
              div(class="formula-box", helpText("$$M_{\\text{ratio}} = \\frac{\\text{meta-}d'}{d'}$$")),
              tags$p(class="small", "Range: any positive real number. Unstable when d\u2019 \u2248 0."),
              div(class="ref-box", HTML("&#x1F4DA; Maniscalco & Lau (2012) <em>ibid.</em>; Rahnev, D. (2025). <em>Nature Communications, 16</em>, 701."))
            ),

            accordion_panel("M-Diff = meta-d\u2019 \u2212 d\u2019",
              tags$p("Difference-based alternative to M-Ratio. Negative values indicate a metacognitive deficit; positive values indicate a surplus. Tends to over-correct at extreme d\u2019 values."),
              div(class="formula-box", helpText("$$M_{\\text{diff}} = \\text{meta-}d' - d'$$")),
              div(class="ref-box", HTML("&#x1F4DA; Maniscalco & Lau (2012) <em>ibid.</em>; Rahnev (2025) <em>ibid.</em>"))
            )
          ),

          tags$hr(),

          # ----- Association block -----
          tags$h5(class="mt-2 mb-2",
            tags$span(class="badge-group","Association"),
            " Trial-Level Association Measures"),
          tags$p(class="text-muted small",
            "Measure how strongly confidence tracks accuracy on a trial-by-trial basis.
             No SDT model assumptions required; computable from raw trial data."),

          accordion(open = FALSE, id = "acc_assoc",

            accordion_panel("\u0394Conf \u2014 Delta Confidence",
              tags$p("The simplest association measure: the difference in mean confidence between correct and incorrect trials. Positive values indicate higher confidence on correct trials."),
              div(class="formula-box", helpText(
                "$$\\Delta\\text{Conf} = \\overline{c}_{\\text{correct}} - \\overline{c}_{\\text{error}}$$")),
              tags$p(class="small", "Units: raw confidence scale. Strongly depends on task d\u2019."),
              div(class="ref-box", HTML("&#x1F4DA; Rahnev, D. (2025). A comprehensive assessment of current methods for measuring metacognition. <em>Nature Communications, 16</em>, 701."))
            ),

            accordion_panel("AUC\u2082 \u2014 Area Under the Type 2 ROC",
              tags$p("The area under the curve that plots Type 2 hit rate against Type 2 false-alarm rate across all confidence thresholds. The oldest metacognitive measure (proposed 1950s). Computed here from empirical response frequencies."),
              div(class="formula-box", helpText(
                "$$\\text{AUC}_2 = \\int_0^1 \\text{HR}_2\\,d(\\text{FAR}_2) \\approx \\sum_k \\Delta\\text{FAR}_{2,k}\\cdot\\overline{\\text{HR}}_{2,k}$$")),
              tags$p(class="small", "Range: 0.5 (chance) \u2013 1.0 (perfect). Strongly depends on task d\u2019."),
              div(class="ref-box", HTML("&#x1F4DA; Galvin, S. J. et al. (2003). Type 2 tasks in the theory of signal detectability. <em>Perception & Psychophysics, 65</em>, 354\u2013370. | Rahnev (2025) <em>ibid.</em>"))
            ),

            accordion_panel("\u03b3 \u2014 Goodman\u2013Kruskal Gamma",
              tags$p("The rank correlation between trial-by-trial confidence and accuracy. The most common metacognitive measure in the memory literature."),
              div(class="formula-box", helpText(
                "$$\\gamma = \\frac{C - D}{C + D}$$")),
              tags$p(class="small", "where C = concordant pairs (high conf & correct > low conf & incorrect), D = discordant pairs. Range: \\(-1\\) to \\(+1\\)."),
              div(class="ref-box", HTML("&#x1F4DA; Nelson, T. O. (1984). A comparison of current measures of the accuracy of feeling-of-knowing predictions. <em>Psychological Bulletin, 95</em>, 109\u2013133. | Rahnev (2025) <em>ibid.</em>"))
            ),

            accordion_panel("\u03d5 \u2014 Phi (Pearson correlation)",
              tags$p("The Pearson product-moment correlation between trial-by-trial confidence rating and binary accuracy. Assumes a linear relationship between the two."),
              div(class="formula-box", helpText(
                "$$\\phi = r(c_i,\\, a_i) = \\frac{\\sum_i(c_i-\\bar{c})(a_i-\\bar{a})}{\\sqrt{\\sum_i(c_i-\\bar{c})^2 \\sum_i(a_i-\\bar{a})^2}}$$")),
              tags$p(class="small", "where \\(c_i\\) = confidence on trial \\(i\\), \\(a_i \\in \\{0,1\\}\\). Range: \\(-1\\) to \\(+1\\)."),
              div(class="ref-box", HTML("&#x1F4DA; Rahnev (2025) <em>ibid.</em>"))
            )
          ),

          tags$hr(),

          # ----- Calibration block -----
          tags$h5(class="mt-2 mb-2",
            tags$span(class="badge-group","Calibration"),
            " Calibration Measures (Schraw 2009)"),
          tags$p(class="text-muted small",
            "Item-level measures comparing normalised confidence to binary accuracy.
             Confidence is rescaled to [0,\u20091] before computation.
             Originally described for free-recall and knowledge-monitoring tasks."),

          accordion(open = FALSE, id = "acc_cal",

            accordion_panel("Bias \u2014 over- / under-confidence",
              tags$p("The signed mean discrepancy between normalised confidence and accuracy. Positive = overconfident; negative = underconfident."),
              div(class="formula-box", helpText(
                "$$\\text{Bias} = \\frac{1}{N}\\sum_{i=1}^{N}(\\hat{c}_i - p_i)$$")),
              tags$p(class="small", "\\(\\hat{c}_i\\) = confidence normalised to [0,1]; \\(p_i \\in \\{0,1\\}\\). Range: \\(-1\\) to \\(+1\\)."),
              div(class="ref-box", HTML("&#x1F4DA; Schraw, G. (2009). A conceptual analysis of five measures of metacognitive monitoring. <em>Metacognition and Learning, 4</em>, 33\u201345."))
            ),

            accordion_panel("Calibration \u2014 Absolute Accuracy (Brier score)",
              tags$p("The mean squared error between normalised confidence and accuracy. Lower values are better. Sometimes called the Brier score."),
              div(class="formula-box", helpText(
                "$$\\text{Calibration} = \\frac{1}{N}\\sum_{i=1}^{N}(\\hat{c}_i - p_i)^2$$")),
              tags$p(class="small", "Range: 0 (perfect) \u2013 1 (worst). Sensitive to both direction and magnitude of error."),
              div(class="ref-box", HTML("&#x1F4DA; Schraw (2009) <em>ibid.</em>"))
            ),

            accordion_panel("Discrimination \u2014 weighted confidence difference",
              tags$p("Measures how well the observer assigns higher confidence to correct versus incorrect items, weighted by the proportion of each response type. Related to \u0394Conf but with count-weighting."),
              div(class="formula-box", helpText(
                "$$D = \\frac{N_c\\,\\overline{c}_{\\text{correct}} - N_e\\,\\overline{c}_{\\text{error}}}{N}$$")),
              tags$p(class="small", "\\(N_c, N_e\\) = number of correct / error trials; \\(N = N_c + N_e\\). Range: \\(-c_{\\max}\\) to \\(+c_{\\max}\\)."),
              div(class="ref-box", HTML("&#x1F4DA; Schraw (2009) <em>ibid.</em>"))
            ),

            accordion_panel("Scatter \u2014 confidence variability difference",
              tags$p("Assesses whether confidence judgments are more variable for correct trials than error trials. Positive scatter means confidence is more spread out on correct trials."),
              div(class="formula-box", helpText(
                "$$S = \\frac{N_c\\,\\text{var}(c_{\\text{correct}}) - N_e\\,\\text{var}(c_{\\text{error}})}{N}$$")),
              tags$p(class="small", "Range: \\(-\\infty\\) to \\(+\\infty\\). Near zero = equal variability."),
              div(class="ref-box", HTML("&#x1F4DA; Schraw (2009) <em>ibid.</em>"))
            )
          ),

          tags$hr(),
          tags$p(class="text-muted small mt-2",
            HTML("<b>Recommended reading:</b><br>
            Maniscalco, B., & Lau, H. (2014). Signal detection theory analysis of type 1 and type 2 data: meta-d\u2019, response-specific meta-d\u2019, and the unequal variance SDT model. In S. M. Fleming & C. D. Frith (Eds.), <em>The Cognitive Neuroscience of Metacognition</em> (pp.\u00a025\u201366). Springer.<br><br>
            Rahnev, D. (2025). A comprehensive assessment of current methods for measuring metacognition. <em>Nature Communications, 16</em>, 701. https://doi.org/10.1038/s41467-025-56117-0<br><br>
            Schraw, G. (2009). A conceptual analysis of five measures of metacognitive monitoring. <em>Metacognition and Learning, 4</em>, 33\u201345. https://doi.org/10.1007/s11409-008-9031-3"))
        )
      )
    )
  )
)

# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {

  raw_data <- reactive({
    req(input$file)
    tryCatch({
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      showNotification(sprintf("Loaded %d rows \u00d7 %d columns.", nrow(df), ncol(df)),
                       type = "message", duration = 4)
      df
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error"); NULL
    })
  })

  observe({
    df <- raw_data(); req(df); cols <- names(df)
    updateSelectInput(session, "col_id",   choices = cols)
    updateSelectInput(session, "col_stim", choices = cols)
    updateSelectInput(session, "col_resp", choices = cols)
    updateSelectInput(session, "col_conf", choices = cols)
  })

  observe({
    df <- raw_data(); req(df, input$col_stim)
    vals <- sort(unique(as.character(df[[input$col_stim]])))
    updateSelectInput(session, "s1_stim_val", choices = vals, selected = vals[1])
    updateSelectInput(session, "s2_stim_val", choices = vals, selected = vals[min(2, length(vals))])
  })

  observe({
    df <- raw_data(); req(df, input$col_resp)
    vals <- sort(unique(as.character(df[[input$col_resp]])))
    updateSelectInput(session, "s1_resp_val", choices = vals, selected = vals[1])
    updateSelectInput(session, "s2_resp_val", choices = vals, selected = vals[min(2, length(vals))])
  })

  output$data_preview <- renderDT({
    df <- raw_data(); req(df)
    datatable(head(df, 200), options = list(scrollX = TRUE, pageLength = 10, dom = "tp"),
              rownames = FALSE)
  }, server = FALSE)

  output$status_ui <- renderUI({
    df <- raw_data(); if (is.null(df)) return(NULL)
    n_part <- length(unique(df[[req(input$col_id)]]))
    n_conf <- length(unique(df[[req(input$col_conf)]]))
    tags$div(class = "small text-muted",
      tags$b(n_part), " participants detected.", tags$br(),
      tags$b(n_conf), " confidence levels detected.")
  })

  results <- eventReactive(input$run_btn, {
    df <- raw_data()
    req(df, input$col_id, input$col_stim, input$col_resp, input$col_conf,
        input$s1_stim_val, input$s2_stim_val, input$s1_resp_val, input$s2_resp_val)
    shiny::validate(
      shiny::need(input$s1_stim_val != input$s2_stim_val, "S1 and S2 stimulus values must differ."),
      shiny::need(input$s1_resp_val != input$s2_resp_val, "S1 and S2 response values must differ.")
    )
    withProgress(message = "Fitting meta-d\u2019...", value = 0, {
      tryCatch({
        pb  <- function(idx, n, pid) setProgress(idx/n, detail = sprintf("Participant %s (%d/%d)", pid, idx, n))
        res <- run_analysis(df, input$col_id, input$col_stim, input$col_resp, input$col_conf,
                            input$s1_stim_val, input$s2_stim_val,
                            input$s1_resp_val, input$s2_resp_val,
                            input$s_param, input$n_restarts, pb)
        res
      }, error = function(e) {
        showNotification(paste("Analysis error:", e$message), type = "error"); NULL
      })
    })
  })

  # ---- Results tab ----
  output$results_ui <- renderUI({
    res <- results()
    if (is.null(res)) return(card(card_body(
      p(class = "text-muted text-center mt-4",
        "Run the analysis on the Setup tab to see results."))))

    tagList(
      layout_columns(
        value_box("Participants",   nrow(res),
                  showcase = bs_icon("people-fill"),  theme = "primary"),
        value_box("Mean meta-d\u2019", round(mean(res$meta_d_prime, na.rm=TRUE), 3),
                  showcase = bs_icon("graph-up"),      theme = "success"),
        value_box("Mean M\u2009ratio", round(mean(res$M_ratio, na.rm=TRUE), 3),
                  showcase = bs_icon("bar-chart-fill"), theme = "info"),
        value_box("Mean d\u2019",     round(mean(res$d_prime, na.rm=TRUE), 3),
                  showcase = bs_icon("activity"),      theme = "secondary"),
        col_widths = c(3, 3, 3, 3)
      ),
      card(card_header("Group summary statistics"), tableOutput("summary_table"),
           full_screen = TRUE),
      card(
        card_header("Per-participant results",
          downloadButton("download_btn", "Download CSV",
                         class = "btn-sm btn-outline-primary float-end")),
        DTOutput("results_table"), full_screen = TRUE
      )
    )
  })

  output$summary_table <- renderTable({
    res <- results(); req(res)
    specs <- list(
      list("d_prime",        "d\u2019",            NULL),
      list("meta_d_prime",   "meta-d\u2019",       NULL),
      list("M_ratio",        "M ratio",            1),
      list("M_diff",         "M diff",             0),
      list("delta_conf",     "\u0394Conf",          NULL),
      list("auc2",           "AUC\u2082",          NULL),
      list("gamma",          "\u03b3 (Gamma)",     NULL),
      list("phi",            "\u03d5 (Phi)",        NULL),
      list("bias",           "Bias",               NULL),
      list("abs_accuracy",   "Calibration",        NULL),
      list("discrimination", "Discrimination",     NULL),
      list("scatter",        "Scatter",            NULL)
    )
    do.call(rbind, lapply(specs, function(s)
      make_summary_row(res, s[[1]], s[[2]], s[[3]])))
  }, digits = 3, spacing = "s", striped = TRUE, hover = TRUE)

  output$results_table <- renderDT({
    res <- results(); req(res)
    d <- res
    names(d)[names(d)=="d_prime"]       <- "d\u2019"
    names(d)[names(d)=="meta_d_prime"]  <- "meta-d\u2019"
    names(d)[names(d)=="M_ratio"]       <- "M ratio"
    names(d)[names(d)=="M_diff"]        <- "M diff"
    names(d)[names(d)=="delta_conf"]    <- "\u0394Conf"
    names(d)[names(d)=="phi"]           <- "\u03d5"
    names(d)[names(d)=="gamma"]         <- "\u03b3"
    names(d)[names(d)=="auc2"]          <- "AUC\u2082"
    names(d)[names(d)=="bias"]          <- "Bias"
    names(d)[names(d)=="abs_accuracy"]  <- "Calibration"
    names(d)[names(d)=="discrimination"]<- "Discrim."
    names(d)[names(d)=="scatter"]       <- "Scatter"
    datatable(d, options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
              rownames = FALSE) |>
      formatRound(columns = c("d\u2019","meta-d\u2019","M ratio","M diff",
                               "\u0394Conf","\u03d5","\u03b3","AUC\u2082",
                               "Bias","Calibration","Discrim.","Scatter"),
                  digits = 3)
  }, server = FALSE)

  output$download_btn <- downloadHandler(
    filename = function() paste0("meta_d_results_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(results(), file, row.names = FALSE)
  )

  # ---- Plots tab ----
  output$plots_ui <- renderUI({
    res <- results()
    if (is.null(res)) return(card(card_body(
      p(class = "text-muted text-center mt-4",
        "Run the analysis on the Setup tab to see plots."))))
    tagList(
      layout_columns(
        card(card_header("d\u2019 vs meta-d\u2019"),
             plotOutput("plot_scatter", height = "380px"), full_screen = TRUE),
        card(card_header("M\u2009ratio distribution"),
             plotOutput("plot_hist",    height = "380px"), full_screen = TRUE),
        col_widths = c(6, 6)
      ),
      card(card_header("meta-d\u2019 per participant (sorted, coloured by M\u2009ratio)"),
           plotOutput("plot_bar", height = "380px"), full_screen = TRUE)
    )
  })

  output$plot_scatter <- renderPlot({
    res <- results(); req(res)
    r <- res[!is.na(res$d_prime) & !is.na(res$meta_d_prime) &
               is.finite(res$d_prime) & is.finite(res$meta_d_prime), ]
    req(nrow(r) > 0)
    lim <- range(c(r$d_prime, r$meta_d_prime)); pad <- diff(lim) * 0.08; lim <- lim + c(-pad, pad)
    ggplot(r, aes(x = d_prime, y = meta_d_prime)) +
      geom_abline(slope=1, intercept=0, linetype="dashed", colour="grey60", linewidth=0.7) +
      geom_point(aes(colour = M_ratio), size = 3.5, alpha = 0.85) +
      scale_colour_gradient2(low="#d73027", mid="#ffffbf", high="#1a9850",
                             midpoint=1, name="M ratio") +
      coord_equal(xlim=lim, ylim=lim) +
      labs(x="d\u2019", y="meta-d\u2019") +
      theme_minimal(base_size=14)
  })

  output$plot_hist <- renderPlot({
    res <- results(); req(res)
    v <- res$M_ratio[!is.na(res$M_ratio) & is.finite(res$M_ratio)]; req(length(v) > 0)
    nbins <- max(5, min(30, ceiling(length(v)/2)))
    ggplot(data.frame(M_ratio=v), aes(x=M_ratio)) +
      geom_histogram(bins=nbins, fill="#2c7bb6", colour="white", alpha=0.85) +
      geom_vline(xintercept=1,        linetype="dashed", colour="#d73027", linewidth=0.9) +
      geom_vline(xintercept=mean(v),  linetype="solid",  colour="navy",    linewidth=1) +
      labs(x="M ratio", y="Count",
           caption=sprintf("Red = 1 (perfect metacognition); navy = group mean (%.2f)", mean(v))) +
      theme_minimal(base_size=14)
  })

  output$plot_bar <- renderPlot({
    res <- results(); req(res)
    r <- res[!is.na(res$meta_d_prime) & is.finite(res$meta_d_prime), ]; req(nrow(r) > 0)
    r$Participant <- factor(as.character(r$Participant),
                            levels=as.character(r$Participant[order(r$meta_d_prime)]))
    gm <- mean(r$meta_d_prime, na.rm=TRUE)
    ggplot(r, aes(x=Participant, y=meta_d_prime, fill=M_ratio)) +
      geom_col(alpha=0.88, colour="white", linewidth=0.3) +
      geom_hline(yintercept=gm, linetype="dashed", colour="black", linewidth=0.8) +
      annotate("text", x=nrow(r), y=gm, label=sprintf(" mean=%.2f",gm),
               hjust=1, vjust=-0.4, size=3.5) +
      scale_fill_gradient2(low="#d73027", mid="#ffffbf", high="#1a9850",
                           midpoint=1, name="M ratio") +
      labs(x="Participant", y="meta-d\u2019") +
      theme_minimal(base_size=13) +
      theme(axis.text.x=element_text(angle=45, hjust=1, size=10))
  })
}

shinyApp(ui, server)
