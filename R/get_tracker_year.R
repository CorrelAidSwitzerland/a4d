#' @title Get the tracker year
#'
#' @description
#' This function parses the tracker year from either the monthly sheets
#' or the tracker file name if the parsing is not successful
#'
#' @param tracker_data_file The path to the tracker file
#' @param month_list The list of all the monthly sheets within a tracker
#'
#' @return The year of the tracker
#' @export
get_tracker_year <- function(tracker_data_file, month_list) {
    year <- suppressWarnings(2000 + unique(readr::parse_number(month_list)))
    if (is.na(year)) {
        year <- as.integer(stringr::str_match(tracker_data_file, "[:digit:]{4}"))
    }
    return(year)
}
