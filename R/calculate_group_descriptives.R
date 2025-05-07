#' Calculate Group Descriptives and t-tests
#'
#' Computes group-wise means, standard deviations, and sample sizes for specified variables.
#' Performs independent samples t-tests comparing the groups, and returns a summary table.
#'
#' @param data A data frame containing the variables to analyse.
#' @param vars A character vector of variable names to summarise (e.g., \code{c("age", "height")}).
#' @param group_var The name of the grouping variable (a factor or character variable).
#'
#' @return A data frame with means (SDs) for each group, the p-value, and the t-statistic for each variable.
#'
#' @details
#' \itemize{
#'   \item Group descriptives are reported as \code{mean (SD)}.
#'   \item t-tests are Welch's t-tests (default in \code{t.test()}), which do not assume equal variances.
#'   \item Missing data are handled using pairwise deletion (listwise within each variable).
#' }
#'
#' @examples
#' \dontrun{
#' my_data <- data.frame(
#'   group = rep(c("Control", "Treatment"), each = 5),
#'   age = c(25, 30, 35, 40, 45, 28, 32, 37, 42, 47),
#'   height = c(170, 175, 168, 172, 169, 171, 176, 169, 173, 170),
#'   weight = c(70, 75, 68, 73, 71, 72, 77, 70, 75, 73)
#' )
#' vars_to_analyze <- c("age", "height", "weight")
#' results_table <- calculate_group_descriptives(my_data, vars_to_analyze, "group")
#' print(results_table)
#' }
#'
#' @export
calculate_group_descriptives <- function(data, vars, group_var) {

  # Initialize results list
  results <- list()

  # Calculate descriptives for each variable
  for (var in vars) {

    # Split data by group and compute mean, sd, n
    group_stats <- tapply(data[[var]], data[[group_var]], function(x) {
      c(mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        n = sum(!is.na(x)))
    })

    # Format results: mean (SD) for each group
    formatted_stats <- sapply(group_stats, function(x) {
      paste0(round(x["mean"], 2),
             " (",
             round(x["sd"], 2),
             ")")
    })

    # Run t-test
    t_test <- t.test(data[[var]] ~ data[[group_var]])

    # Combine results into a row
    row_result <- c(
      formatted_stats,
      `p-value` = format.pval(t_test$p.value, digits = 3),
      `t-statistic` = round(t_test$statistic, 2)
    )

    results[[var]] <- row_result
  }

  # Convert list to data frame
  result_df <- do.call(rbind, results)
  result_df <- as.data.frame(result_df)

  # Add variable names as row names
  rownames(result_df) <- vars

  return(result_df)
}
