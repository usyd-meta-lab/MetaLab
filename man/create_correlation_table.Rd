% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_correlation_table.R
\name{create_correlation_table}
\alias{create_correlation_table}
\title{Create APA-style correlation table with descriptive statistics}
\usage{
create_correlation_table(data, file = NULL)
}
\arguments{
\item{data}{A data frame or matrix containing variables. Non-numeric variables are automatically excluded.
Missing values will be handled using pairwise deletion.}

\item{file}{Optional. If provided, saves the table to a Word document at the specified file path (must end with .docx).}
}
\value{
Invisibly returns the formatted correlation table as a data frame. Also prints a styled table and significance note.
}
\description{
This function computes a correlation matrix with significance stars, along with means and standard deviations,
and presents the results in an APA-style formatted table. Automatically excludes non-numeric variables.
}
\details{
Correlations are computed using Pearson's r. Significance levels are denoted as follows:
\itemize{
\item p < .05, ** p < .01, *** p < .001. Means and standard deviations are shown in the first column.
}
}
\examples{
\dontrun{
  my_data <- data.frame(
    var1 = c(1, 2, NA, 4, 5),
    var2 = c(2, 3, 4, NA, 6),
    var3 = c(3, 4, 5, 6, NA),
    group = c("A", "B", "A", "B", "A")
  )
  create_correlation_table(my_data, file = "correlation_table.docx")
}

}
