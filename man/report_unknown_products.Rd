% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_product_data.R
\name{report_unknown_products}
\alias{report_unknown_products}
\title{Report unknown products}
\usage{
report_unknown_products(df, Sheet, stock_list_df)
}
\arguments{
\item{df}{A dataframe containing product data.}

\item{Sheet}{The name of the sheet where the product data is located.}

\item{stock_list_df}{A dataframe containing the stock list of products.}
}
\value{
This function does not return a value. It logs a warning message if there are any unknown products, and logs an info message if there are no unknown products.
}
\description{
This function reports unknown products. It compares the product list in a given dataframe with a stock list, and logs any products that are not found in the stock list.
}
