% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_variable.R
\name{check_variable}
\alias{check_variable}
\title{check_variable
Check whether a specific categorical variable only contains expected values}
\usage{
check_variable(df, var, expect)
}
\arguments{
\item{df}{dataframe:}

\item{var}{character: Name of the variable to be checked}

\item{expect}{character vector: Vector of expected values for \code{var}}
}
\value{
dataframe
}
\description{
check_variable
Check whether a specific categorical variable only contains expected values
}
\examples{
check_variable(df = my_df, var = 'my_variable', expect = c('value1', 'value2'))
}

