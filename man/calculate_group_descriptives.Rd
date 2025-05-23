% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_group_descriptives.R
\name{calculate_group_descriptives}
\alias{calculate_group_descriptives}
\title{Calculate Group Descriptives and t-tests or ANOVA}
\usage{
calculate_group_descriptives(data, vars, group_var)
}
\arguments{
\item{data}{A data frame containing the variables to analyse.}

\item{vars}{A character vector of variable names to summarise.}

\item{group_var}{The name of the grouping variable.}
}
\value{
A data frame with group descriptives and statistical test results.
}
\description{
Computes group-wise means, standard deviations, and sample sizes for specified variables.
Performs t-tests or one-way ANOVAs depending on the number of groups.
}
