#' @title Sanitize column name
#'
#' @description
#' This function takes in a string,
#' sanitizes it by converting all letters to lower case,
#' removing all spaces and special characters except alphanumeric characters,
#' and then returns the sanitized string.
#'
#' @param text A text to be sanitized.
#'
#' @return The sanitized text.
#'
#' @examples
#' sanitize_str("John Doe's Column") # should return "johndoescolumn"
#' sanitize_str("Date 2022") # should return "date2022"
#' sanitize_str("My Awesome 1st Column!!") # should return "myawesome1stcolumn"
#'
#' @export
sanitize_str <- function(text) {
    text <- text %>%
        str_to_lower() %>%
        str_replace_all(fixed(" "), "") %>%
        str_replace_all("[^[:alnum:]]", "")
}
