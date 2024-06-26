% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wide_format_2_long_format.R
\name{find_string_cols}
\alias{find_string_cols}
\title{Find Columns Containing Specific String}
\usage{
find_string_cols(df, string)
}
\arguments{
\item{df}{A dataframe in which to search for the string.}

\item{string}{The string to search for in the column names.}
}
\value{
A character vector of matching column names.
}
\description{
This function returns the column names of a dataframe that contain a specific string.
}
\examples{
data(mtcars)
find_string_cols(mtcars, "mpg")
}
