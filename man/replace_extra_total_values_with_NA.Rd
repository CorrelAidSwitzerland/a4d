% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wide_format_2_long_format.R
\name{replace_extra_total_values_with_NA}
\alias{replace_extra_total_values_with_NA}
\title{Replace Extra "Total" Values with NA}
\usage{
replace_extra_total_values_with_NA(df, column_name)
}
\arguments{
\item{df}{A data frame containing the column to be processed.}

\item{column_name}{The name of the column where extra "Total" values will be replaced with NA.}
}
\value{
The modified data frame where any value in \code{column_name} preceded by "Total" in one of the two preceding columns is replaced with NA.
}
\description{
This function replaces values in a specified column (\code{column_name}) with NA if one of the two preceding columns contains the word "Total".
This is intended to handle cases where there might be multiple "Total" values in close proximity, keeping only the first.
}
\examples{
# Create a sample data frame
df <- data.frame(col1 = c("A", "B", "Total"), col2 = c(1, 2, 3), col3 = c(4, 5, 6), col4 = c(7, "Total", 9))

# Replace extra "Total" values with NA
df_modified <- replace_extra_total_values_with_NA(df, column_name = "col4")

# Print the modified data frame
print(df_modified)
}
