#' @title Try to handle invalid dates.
#'
#' @description
#' For each value of the character vector, this function uses the
#' lubridate parse functions to find common formats
#'
#'
#' @param df data frame with patient data.
#' @param colname column that has invalid dates.
parse_invalid_dates <- function(df, colname) {
    orders <- c("dmy", "dmY", "by", "bY")
    df <- df %>% mutate(across(!!colname, function(x) {
        lubridate::parse_date_time(x, orders)
    }))
}


numbers_only <- function(x) !grepl("\\D", x)


parse_date_string <- function(text) {
    parsed_date <- as.Date(lubridate::parse_date_time(text, orders = c("ymd", "dmy", "my"), quiet = T))

    if (is.na(parsed_date) && numbers_only(text)) {
        parsed_date <-
            try(as.Date(as.numeric(text), origin = "1899-12-30"), silent = TRUE)
    }

    if (class(parsed_date) == "try-error" || is.na(parsed_date)) parsed_date <- as.Date("9999-01-01")

    if (lubridate::year(parsed_date) < 17) {
        lubridate::year(parsed_date) <- lubridate::year(parsed_date) + 2000
    }

    return(parsed_date)
}


fix_date_cols <- function(d) {
    if (!is.na(d)) {
        d <- gsub(".*[(]|[)].*", "", d)

        d <- parse_date_string(d)

        if (is.na(d)) d <- as.Date("9999-01-01")
    } else {
        # without this, so with NA instead, dplyr::mutate is throwing an error:
        # "must return compatible vectors across groups".
        d <- NA_Date_
    }

    return(d)
}
