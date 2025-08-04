#' Independent Samples t-test with APA Reporting
#'
#' Conducts an independent-samples t-test and returns an APA-formatted result string, including group means,
#' standard deviations, t-statistic, p-value, and Cohen's d.
#'
#' @param formula A formula specifying the outcome and group variables, e.g., \code{score ~ group}.
#' @param data A data frame containing the variables in the formula.
#' @param var.equal Logical. If \code{TRUE} (default), assumes equal variances (Student's t-test).
#' If \code{FALSE}, Welch's t-test is used.
#'
#' @return A character string reporting the t-test results in APA format.
#'
#' @details
#' \itemize{
#'   \item Checks that the grouping variable has exactly two levels.
#'   \item Calculates group means and standard deviations.
#'   \item Computes the t-statistic, degrees of freedom, p-value, and Cohen's d.
#'   \item Outputs a complete APA-style sentence describing the results.
#' }
#'
#' @examples
#' \dontrun{
#' # Example dataset
#' mydata <- data.frame(
#'   score = c(85, 88, 90, 78, 82, 91, 84, 77, 86, 80),
#'   group = rep(c("Control", "Treatment"), each = 5)
#' )
#'
#' # Run the function
#' result <- independent_t_test_apa(score ~ group, data = mydata, var.equal = TRUE)
#' cat(result)
#' }
#'
#' @export
independent_t_test_apa <- function(formula, data, var.equal = TRUE) {

  # Create a model frame from the formula and data
  mf <- model.frame(formula, data = data)

  # Check for exactly 2 variables (outcome and group)
  if (ncol(mf) != 2) {
    stop("The formula should be of the form 'outcome ~ group'.")
  }

  # Extract outcome and group
  outcome <- mf[[1]]
  group <- mf[[2]]

  # Ensure group is a factor with 2 levels
  if (!is.factor(group)) {
    group <- factor(group)
  }
  if (length(levels(group)) != 2) {
    stop("The grouping variable must have exactly two levels.")
  }

  # Split the outcome variable by group
  split_data <- split(outcome, group)
  group1 <- split_data[[1]]
  group2 <- split_data[[2]]

  # Calculate group means and SDs
  mean1 <- mean(group1, na.rm = TRUE)
  mean2 <- mean(group2, na.rm = TRUE)
  sd1 <- sd(group1, na.rm = TRUE)
  sd2 <- sd(group2, na.rm = TRUE)

  # Perform t-test
  ttest <- t.test(formula, data = data, var.equal = var.equal)
  t_stat <- ttest$statistic
  p_value <- ttest$p.value
  df_val <- ttest$parameter

  # Compute Cohen's d
  if (var.equal) {
    n1 <- length(group1)
    n2 <- length(group2)
    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
    cohen_d <- (mean1 - mean2) / pooled_sd
  } else {
    cohen_d <- (mean1 - mean2) / sqrt((sd1^2 + sd2^2) / 2)
  }

  # Format p-value for APA reporting
  p_str <- if (p_value < .001) {
    "< .001"
  } else {
    paste0("= ", format(round(p_value, 3), nsmall = 3))
  }

  # Degrees of freedom formatting
  df_str <- if (var.equal) {
    as.character(df_val)
  } else {
    as.character(round(df_val, 2))
  }

  # Significance wording
  significance <- ifelse(p_value < 0.05, "significantly", "non-significantly")

  # Create APA-formatted result string
  result_str <- sprintf(
    "An independent-samples t-test was conducted to compare the groups. %s (M = %.2f, SD = %.2f) and %s (M = %.2f, SD = %.2f) differed %s, t(%s) = %.2f, p %s, Cohen's d = %.2f.",
    levels(group)[1], mean1, sd1,
    levels(group)[2], mean2, sd2,
    significance,
    df_str, t_stat, p_str, cohen_d
  )

  return(result_str)
}
