#' @title Sanitize column name
#'
#' @description
#' This function takes in a column name,
#' sanitizes it by converting all letters to lower case,
#' removing all spaces and special characters except alphanumeric characters,
#' and then returns the sanitized column name.
#'
#' @param column_name A string representing the name of the column to be sanitized.
#'
#' @return The sanitized column name.
#'
#' @examples
#' sanitize_column_name("John Doe's Column") # should return "johndoescolumn"
#' sanitize_column_name("Date 2022") # should return "date2022"
#' sanitize_column_name("My Awesome 1st Column!!") # should return "myawesome1stcolumn"
#'
#' @export
sanitize_column_name <- function(column_name) {
    column_name <- column_name %>%
        str_to_lower() %>%
        str_replace_all(fixed(" "), "") %>%
        str_replace_all("[^[:alnum:]]", "")
}