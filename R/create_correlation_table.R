#' Create APA-style correlation table with descriptive statistics
#'
#' This function computes a correlation matrix with significance stars, along with means and standard deviations,
#' and presents the results in an APA-style formatted table. Automatically excludes non-numeric variables.
#'
#' @param data A data frame or matrix containing variables. Non-numeric variables are automatically excluded.
#' Missing values will be handled using pairwise deletion.
#' @param file Optional. If provided, saves the table to a Word document at the specified file path (must end with .docx).
#'
#' @return Invisibly returns the formatted correlation table as a data frame. Also prints a styled table and significance note.
#'
#' @details Correlations are computed using Pearson's r. Significance levels are denoted as follows:
#' * p < .05, ** p < .01, *** p < .001. Means and standard deviations are shown in the first column.
#'
#' @import Hmisc
#' @import knitr
#' @import kableExtra
#' @import dplyr
#' @import officer
#' @import flextable
#'
#' @examples
#' \dontrun{
#'   my_data <- data.frame(
#'     var1 = c(1, 2, NA, 4, 5),
#'     var2 = c(2, 3, 4, NA, 6),
#'     var3 = c(3, 4, 5, 6, NA),
#'     group = c("A", "B", "A", "B", "A")
#'   )
#'   create_correlation_table(my_data, file = "correlation_table.docx")
#' }
#'
#' @export
create_correlation_table <- function(data, file = NULL) {

  # Select only numeric variables
  numeric_data <- data %>% dplyr::select(where(is.numeric))

  if (ncol(numeric_data) == 0) {
    stop("No numeric variables found in the provided data.")
  }

  # Calculate correlation matrix with p-values
  correlation_matrix <- rcorr(as.matrix(numeric_data), type = "pearson")

  # Calculate means and SDs
  means <- round(colMeans(numeric_data, na.rm = TRUE), 2)
  sds <- round(apply(numeric_data, 2, sd, na.rm = TRUE), 2)
  desc_stats <- paste0(means, " (", sds, ")")

  # Round correlations
  rounded_correlations <- round(correlation_matrix$r, 2)

  # Prepare formatted matrix
  n <- ncol(numeric_data)
  formatted_matrix <- matrix("", nrow = n, ncol = n)

  for(i in 1:n) {
    for(j in i:n) {
      if(i == j) {
        formatted_matrix[i,j] <- "—"
      } else {
        correlation <- rounded_correlations[i,j]
        p_value <- correlation_matrix$P[i,j]
        if(is.na(correlation)) {
          formatted_matrix[i,j] <- "NA"
        } else {
          stars <- dplyr::case_when(
            p_value < .001 ~ "***",
            p_value < .01 ~ "**",
            p_value < .05 ~ "*",
            TRUE ~ ""
          )
          formatted_matrix[i,j] <- paste0(correlation, stars)
        }
      }
    }
  }

  for(i in 1:n) {
    for(j in 1:(i-1)) {
      formatted_matrix[i,j] <- ""
    }
  }

  formatted_table <- as.data.frame(formatted_matrix)
  formatted_table <- cbind('M (SD)' = desc_stats, formatted_table)

  colnames(formatted_table)[-1] <- colnames(numeric_data)
  rownames(formatted_table) <- colnames(numeric_data)

  ## --- Print nice table to console ----
  kable_table <- knitr::kable(formatted_table, format = "markdown",
                              caption = "Means, Standard Deviations, and Correlations") %>%
    kableExtra::kable_styling(full_width = FALSE)

  print(kable_table)

  cat("\nNote: * p < .05, ** p < .01, *** p < .001")
  cat("\nMissing values were handled using pairwise deletion.")

  ## --- Export to Word if file path provided ----
  if (!is.null(file)) {

    if (!grepl("\\.docx$", file, ignore.case = TRUE)) {
      stop("The file name must end with '.docx'")
    }

    ft <- flextable::flextable(formatted_table)
    ft <- flextable::autofit(ft)

    doc <- officer::read_docx()
    doc <- officer::body_add_par(doc, "Means, Standard Deviations, and Correlations", style = "heading 1")
    doc <- flextable::body_add_flextable(doc, ft)  # <- FIXED LINE
    doc <- officer::body_add_par(doc, "Note: * p < .05, ** p < .01, *** p < .001. Missing values were handled using pairwise deletion.")

    print(doc, target = file)
    message(paste("Correlation table exported to", file))
  }

  invisible(formatted_table)
}
