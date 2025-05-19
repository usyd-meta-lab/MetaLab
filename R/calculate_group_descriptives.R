#' Calculate Group Descriptives and t-tests or ANOVA
#'
#' Computes group-wise means, standard deviations, and sample sizes for specified variables.
#' Performs t-tests or one-way ANOVAs depending on the number of groups.
#'
#' @param data A data frame containing the variables to analyse.
#' @param vars A character vector of variable names to summarise.
#' @param group_var The name of the grouping variable.
#'
#' @return A data frame with group descriptives and statistical test results.
#' @export
calculate_group_descriptives <- function(data, vars, group_var) {
  
  results <- list()
  group_levels <- na.omit(unique(data[[group_var]]))
  n_groups <- length(group_levels)
  
  for (var in vars) {
    
    # Descriptive stats
    group_stats <- tapply(data[[var]], data[[group_var]], function(x) {
      c(mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        n = sum(!is.na(x)))
    })
    
    # Format descriptives: mean (SD)
    formatted_stats <- sapply(group_stats, function(x) {
      paste0(round(x["mean"], 2), " (", round(x["sd"], 2), ")")
    })
    
    # Run appropriate test
    formula <- as.formula(paste(var, "~", group_var))
    
    if (n_groups == 2) {
      test <- t.test(formula, data = data)
      stat_name <- "t-statistic"
      stat_value <- round(test$statistic, 2)
      p_val <- format.pval(test$p.value, digits = 3)
    } else if (n_groups > 2) {
      aov_result <- aov(formula, data = data)
      summary_aov <- summary(aov_result)[[1]]
      stat_name <- "F-statistic"
      stat_value <- round(summary_aov["F value"][1], 2)
      p_val <- format.pval(summary_aov["Pr(>F)"][1], digits = 3)
    }
    
    # Combine all results safely
    row_result <- c(
      formatted_stats,
      `p-value` = p_val,
      setNames(stat_value, stat_name)
    )
    results[[var]] <- row_result
  }
  
  # Convert list to data frame
  result_df <- do.call(rbind, results)
  result_df <- as.data.frame(result_df)
  rownames(result_df) <- vars
  
  return(result_df)
}