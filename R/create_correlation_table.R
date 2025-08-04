#' Create APA‐style correlation table with optional embedded thumbnail plots
#'
#' Computes Pearson correlations (pairwise deletion) with significance stars,
#' prints an APA-style table, and (optionally) writes a .docx with:
#'   • scatterplot thumbnails on the chosen triangle (upper/lower),
#'   • correlation text on the opposite triangle (or hidden), and
#'   • histograms on the diagonal.
#'
#' @param data               Data frame or matrix; non-numeric variables are dropped.
#' @param file               Optional .docx path; if provided, writes the Word file.
#' @param embed_scatter      Logical. If TRUE, embed scatter thumbnails in `scatter_triangle`. Default TRUE.
#' @param scatter_triangle   "lower" or "upper" for where the scatter thumbnails should go. Default "lower".
#' @param show_correlations  Logical. If TRUE, correlation text is shown on the
#'                           opposite triangle (or on upper if no scatter). Default TRUE.
#' @param diag_hist          Logical. If TRUE, embed histogram thumbnails on the diagonal. Default TRUE.
#' @param show_scatter_labels Logical. If TRUE (default), overlay “r=.XX” + sig stars on scatter thumbnails.
#' @param fit_line_col       NULL or an R colour. If provided, draw a least-squares line in each scatter.
#' @param point_col          Colour for scatterplot points. Default "black".
#' @param label_cex          cex for the “r=.XX[***]” label in scatter thumbnails. Default 1.15.
#' @param img_size           Image size (inches) for thumbnails (square). Default 0.8.
#' @param img_dpi            DPI for thumbnails. Default 300.
#' @param embed_lower        (Deprecated) If provided, TRUE behaves like `scatter_triangle="lower"` and `embed_scatter=TRUE`.
#' @param ...                Passed to png() (e.g., bg = "transparent").
#'
#' @return Invisibly returns the printed correlation table (data.frame).
#' @import Hmisc knitr kableExtra dplyr officer flextable
#' @export
create_correlation_table <- function(data,
                                     file                = NULL,
                                     embed_scatter       = FALSE,
                                     scatter_triangle    = c("lower", "upper"),
                                     show_correlations   = TRUE,
                                     diag_hist           = FALSE,
                                     show_scatter_labels = TRUE,
                                     fit_line_col        = NULL,
                                     point_col           = "grey",
                                     label_cex           = 1.15,
                                     img_size            = 0.8,
                                     img_dpi             = 300,
                                     ...) {

  ## ---------- 0) Back-compat & args ----------
  scatter_triangle <- match.arg(scatter_triangle)


  # Data prep
  numeric_data <- dplyr::select(data, where(is.numeric))
  n_var <- ncol(numeric_data)
  if (is.null(n_var) || n_var == 0) stop("No numeric variables found in the provided data.")
  var_nm <- colnames(numeric_data)

  # Correlations & p-values (pairwise)
  cor_obj <- Hmisc::rcorr(as.matrix(numeric_data), type = "pearson")

  # Descriptives
  means <- round(colMeans(numeric_data, na.rm = TRUE), 2)
  sds   <- round(apply(numeric_data, 2, sd, na.rm = TRUE), 2)
  desc  <- paste0(means, " (", sds, ")")

  # Helpers
  stars_from_p <- function(p) {
    if (is.na(p)) "" else if (p < .001) "***" else if (p < .01) "**" else if (p < .05) "*" else ""
  }

  ## ---------- 1) Build correlation text matrix per requested triangle ----------
  mat <- matrix("",
                nrow = n_var, ncol = n_var,
                dimnames = list(var_nm, var_nm))

  # Which triangle shows correlation text?
  cor_triangle <- "none"
  if (isTRUE(show_correlations)) {
    if (isTRUE(embed_scatter)) {
      cor_triangle <- if (scatter_triangle == "lower") "upper" else "lower"
    } else {
      cor_triangle <- "upper"
    }
  }

  for (i in seq_len(n_var)) {
    for (j in seq_len(n_var)) {
      if (i == j) {
        mat[i, j] <- "—"
      } else {
        is_upper <- (i < j)
        is_lower <- (i > j)
        show_here <- (cor_triangle == "upper" && is_upper) ||
          (cor_triangle == "lower" && is_lower)
        if (show_here) {
          r <- round(cor_obj$r[i, j], 2)
          p <- cor_obj$P[i, j]
          mat[i, j] <- paste0(r, stars_from_p(p))
        } else {
          mat[i, j] <- ""
        }
      }
    }
  }

  df_mat <- as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)

  # Keep strictly 2-D: add a "Variable" column; no rownames
  tbl <- data.frame(
    Variable = var_nm,
    `M (SD)` = desc,
    df_mat,
    check.names = FALSE
  )
  rownames(tbl) <- NULL  # <-- ensure row names are dropped for the console viewer

  ## ---------- 2) Console print ----------
  kbl <- knitr::kable(
    tbl,
    format     = "markdown",
    caption    = "Means, Standard Deviations, and Correlations",
    row.names  = FALSE
  ) %>% kableExtra::kable_styling(full_width = FALSE)
  print(kbl)
  cat("\nNote: * p < .05, ** p < .01, *** p < .001. Missing values handled with pairwise deletion.\n")

  ## ---------- 3) Exit if not writing a file ----------
  if (is.null(file)) return(invisible(tbl))
  if (!grepl("\\.docx$", file, ignore.case = TRUE)) stop("The file name must end with '.docx'.")

  ## ---------- 4) Build flextable for Word ----------
  ft <- flextable::flextable(tbl)

  ## ---------- 5) Thumbnails: scatters (upper/lower) + optional diagonal hist ----------
  tmp_files <- character(0)

  need_thumbs <- (isTRUE(embed_scatter) || isTRUE(diag_hist))
  if (need_thumbs) {
    if (!("as_image" %in% getNamespaceExports("flextable"))) {
      warning("flextable::as_image() not available – update flextable to embed images in cells. Skipping thumbnails.")
    } else {
      # Validate colours
      valid_col <- function(z) isTRUE(tryCatch({ grDevices::col2rgb(z); TRUE }, error = function(e) FALSE))
      draw_lm <- !is.null(fit_line_col) && valid_col(fit_line_col)
      if (!is.null(fit_line_col) && !draw_lm) warning("`fit_line_col` is not a valid colour; regression lines will be skipped.")
      if (!valid_col(point_col)) { warning("`point_col` is not a valid colour; using 'black'."); point_col <- "black" }

      fmt_r <- function(r) {
        if (is.na(r)) return("r=NA")
        s <- sprintf("%.2f", r)
        s <- sub("^0\\.", ".", s); s <- sub("^-0\\.", "-.", s)
        paste0("r=", s)
      }

      # tiny scatter PNG with top-centered r+stars (optional) and optional fit line
      make_scatter <- function(x, y, path) {
        grDevices::png(filename = path,
                       width  = img_size * img_dpi,
                       height = img_size * img_dpi,
                       res    = img_dpi,
                       ...)
        old_mar <- par("mar"); on.exit(par(mar = old_mar), add = TRUE)
        par(mar = c(0, 0, 0, 0))

        ok <- is.finite(x) & is.finite(y)
        if (!any(ok)) {
          plot.new()
        } else {
          x <- x[ok]; y <- y[ok]
          plot(x, y,
               pch = 16, cex = 0.4,
               col = grDevices::adjustcolor(point_col, alpha.f = 0.5),
               axes = FALSE, xlab = "", ylab = "")
          if (draw_lm && length(x) >= 3) {
            fit <- tryCatch(lm(y ~ x), error = function(e) NULL)
            if (!is.null(fit)) abline(fit, col = fit_line_col, lwd = 2)
          }
          if (isTRUE(show_scatter_labels)) {
            ct <- tryCatch(stats::cor.test(x, y), error = function(e) NULL)
            r  <- if (is.null(ct)) suppressWarnings(cor(x, y)) else unname(ct$estimate)
            p  <- if (is.null(ct)) NA_real_ else ct$p.value
            lbl <- paste0(fmt_r(r), stars_from_p(p))
            rx <- range(x, finite = TRUE); ry <- range(y, finite = TRUE)
            xmid <- mean(rx)
            ytop <- ry[2] - 0.06 * diff(ry)         # centered at top
            font_weight <- if (!is.na(p) && p < .05) 2 else 1  # bold if significant
            text(xmid, ytop, labels = lbl, cex = label_cex, font = font_weight)
          }
        }
        grDevices::dev.off()
      }

      # tiny histogram PNG for diagonal
      make_hist <- function(x, path) {
        grDevices::png(filename = path,
                       width  = img_size * img_dpi,
                       height = img_size * img_dpi,
                       res    = img_dpi,
                       ...)
        old_mar <- par("mar"); on.exit(par(mar = old_mar), add = TRUE)
        par(mar = c(0, 0, 0, 0))
        ok <- is.finite(x)
        if (!any(ok)) {
          plot.new()
        } else {
          hist(x[ok], main = "", xlab = "", ylab = "", axes = FALSE)
        }
        grDevices::dev.off()
      }

      col_keys <- colnames(tbl)  # "Variable", "M (SD)", then var names…

      # Scatter thumbnails on requested triangle
      if (isTRUE(embed_scatter) && n_var >= 2) {
        for (row in seq_len(n_var)) {
          for (col in seq_len(n_var)) {
            is_upper <- (row < col)
            is_lower <- (row > col)
            want_scatter <- (scatter_triangle == "upper" && is_upper) ||
              (scatter_triangle == "lower" && is_lower)
            if (want_scatter) {
              tmp <- tempfile(fileext = ".png")
              tmp_files <- c(tmp_files, tmp)
              # x = column var, y = row var
              make_scatter(numeric_data[[col]], numeric_data[[row]], tmp)
              target_col_name <- col_keys[col + 2]  # +2 for "Variable" & "M (SD)"
              ft <- flextable::compose(
                ft,
                i = row,
                j = target_col_name,
                value = flextable::as_paragraph(
                  flextable::as_image(src = tmp, width = img_size, height = img_size)
                ),
                part = "body"
              )
            }
          }
        }
      }

      # Diagonal histograms
      if (isTRUE(diag_hist)) {
        for (i in seq_len(n_var)) {
          tmp <- tempfile(fileext = ".png")
          tmp_files <- c(tmp_files, tmp)
          make_hist(numeric_data[[i]], tmp)
          target_col_name <- col_keys[i + 2]
          ft <- flextable::compose(
            ft,
            i = i,
            j = target_col_name,
            value = flextable::as_paragraph(
              flextable::as_image(src = tmp, width = img_size, height = img_size)
            ),
            part = "body"
          )
        }
      }

      ft <- flextable::align(ft, align = "center", part = "body")
      ft <- flextable::valign(ft, valign = "center", part = "body")
      ft <- flextable::height_all(ft, height = max(img_size, 0.25), part = "body")
    }
  }

  # Autofit after embedding so layout adjusts
  ft <- flextable::autofit(ft)

  ## ---------- 6) Write Word document ----------
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "Means, Standard Deviations, and Correlations", style = "heading 1")
  doc <- flextable::body_add_flextable(doc, ft)
  doc <- officer::body_add_par(
    doc,
    "Note: * p < .05, ** p < .01, *** p < .001. Missing values handled with pairwise deletion."
  )

  print(doc, target = file)
  if (length(tmp_files)) try(unlink(tmp_files), silent = TRUE)

  message("Document written to ", file)
  invisible(tbl)
}
