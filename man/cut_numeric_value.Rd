% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_patient_data_fix.R
\name{cut_numeric_value}
\alias{cut_numeric_value}
\title{If x is outside its allowed range, return error value.}
\usage{
cut_numeric_value(x, min, max, col_name = "")
}
\arguments{
\item{x}{numeric value to check.}

\item{min}{lower bound.}

\item{max}{upper bound.}

\item{col_name}{column name if used with mutate/across.}
}
\value{
either x or error value.
}
\description{
If x is outside its allowed range, return error value.
}
