#' Create APA-style correlation table with descriptive statistics
#'
#' This function computes a correlation matrix with significance stars, along with means and standard deviations,
#' and presents the results in an APA-style formatted table.
#'
#' @param data A data frame or matrix containing numeric variables. Missing values will be handled using pairwise deletion.
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
#'
#' @examples
#' \dontrun{
#'   my_data <- data.frame(
#'     var1 = c(1, 2, NA, 4, 5),
#'     var2 = c(2, 3, 4, NA, 6),
#'     var3 = c(3, 4, 5, 6, NA)
#'   )
#'   create_correlation_table(my_data)
#' }
#'
#' @export
create_correlation_table <- function(data) {
  # Calculate correlation matrix with p-values
  correlation_matrix <- rcorr(as.matrix(data), type = "pearson")

  # Calculate means and SDs
  means <- round(colMeans(data, na.rm = TRUE), 2)
  sds <- round(apply(data, 2, sd, na.rm = TRUE), 2)
  desc_stats <- paste0(means, " (", sds, ")")

  # Round correlations
  rounded_correlations <- round(correlation_matrix$r, 2)

  # Prepare formatted matrix
  n <- ncol(data)
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

  colnames(formatted_table)[-1] <- colnames(data)
  rownames(formatted_table) <- colnames(data)

  kable_table <- knitr::kable(formatted_table, format = "markdown",
                              caption = "Means, Standard Deviations, and Correlations") %>%
    kableExtra::kable_styling(full_width = FALSE)

  print(kable_table)

  cat("\nNote: * p < .05, ** p < .01, *** p < .001")
  cat("\nMissing values were handled using pairwise deletion.")

  invisible(formatted_table)
}
