#' Plot Histograms for All Numeric Variables
#'
#' Creates histograms for all numeric variables in a data frame and displays them
#' together in a multi-panel plot using base R graphics.
#'
#' @param data A data frame containing the variables to plot.
#' @param col Fill color for the histograms (default is "lightblue").
#' @param border Border color for the histograms (default is "black").
#'
#' @return Invisibly returns \code{NULL}. Generates a multi-panel plot as a side effect.
#'
#' @examples
#' df <- data.frame(
#'   age = c(23, 35, 45, 29, 60),
#'   score = c(88.5, 92.3, 79.4, 85.0, 90.1),
#'   income = c(40000, 52000, 61000, 48000, 57000)
#' )
#' plot_numeric_histograms(df)
#'
#' @export
plot_numeric_histograms <- function(data, col = "lightblue", border = "black") {
  
  # Check if input is a data frame
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  
  # Identify numeric variables
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  num_vars <- length(numeric_vars)
  
  if (num_vars == 0) {
    warning("No numeric variables found in the data frame.")
    return(invisible(NULL))
  }
  
  # Set up plot layout
  rows <- ceiling(sqrt(num_vars))
  cols <- ceiling(num_vars / rows)
  old_par <- par(mfrow = c(rows, cols))
  
  # Plot histograms
  for (var in numeric_vars) {
    hist(data[[var]],
         main = paste("Histogram of", var),
         xlab = var,
         col = col,
         border = border)
  }
  
  # Reset plotting parameters to original
  par(old_par)
  
  invisible(NULL)
}