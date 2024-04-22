#' @title Check if dataframe contains required columns
#'
#' @description
#' This function checks if a dataframe contains the columns 'Total Units Released' and 'Units Released per person'.
#' (for detection the Mandalay Children's Hospital 2020-2021 wide-format)
#' It returns TRUE if both columns are present, and FALSE otherwise.
#'
#' @param df A dataframe to check for the presence of required columns.
#' @return A logical value indicating whether the dataframe contains the required columns.
#' @examples
#' df <- data.frame("Total Units Released" = c(1, 2, 3), "Units Released per person" = c(4, 5, 6), "Other Column" = c(7, 8, 9))
#' check_wide_format_columns(df) # Returns: TRUE
check_wide_format_columns <- function(df) {
    required_columns <- c("Total Units Released", "Units Released per person")
    all(required_columns %in% names(df))
}


#' @title Create New Rows in a DataFrame
#'
#' @description
#' This function creates new rows in a given dataframe based on specific conditions.
#' To convert wide-format area in the 2017 - 2020 data to long-format area.
#'
#' @param df A dataframe containing the original data.
#' @return A modified dataframe with additional rows.
#' @details The function checks if 'Total Units Released' and 'Units Released per person'
#' are not empty and not equal. If this condition is met, it adds new rows to the dataframe
#' based on values in columns between 'Released To' and 'Units Released per person'.
#' If an error occurs during processing, the original dataframe is returned.
#'
#' @examples
#' df <- data.frame(
#'     "Date" = c("2024-03-01", "2024-03-02"),
#'     "Total Units Released" = c(100, 200),
#'     "Units Released per person" = c(10, 20),
#'     "Released To (select from drop down list)" = c("A", "B")
#' )
#' new_df <- create_new_rows(df)
create_new_rows <- function(df) {
    tryCatch(
        {
            # Create an empty dataframe to store the new rows
            new_df <- df[FALSE, ]
            # Loop over rows of the dataframe
            for (i in 1:nrow(df)) {
                # Add the current row to the new dataframe
                new_df <- rbind(new_df, df[i, ])
                # Check if 'Total Units Released' and 'Units Released per person' are not empty and not equal
                if (!is.na(df$"Total Units Released"[i]) & !is.na(df$"Units Released per person"[i]) & df$"Total Units Released"[i] != df$"Units Released per person"[i]) {
                    # Loop over all columns between 'Released To' and 'Units Released per person'
                    start_col <- which(names(df) == "Released To (select from drop down list)") + 1
                    end_col <- which(names(df) == "Units Released per person") - 1
                    for (j in start_col:end_col) {
                        # If the cell is not empty, copy its value to a new row in 'Released To (select from drop down list)'
                        if (!is.na(df[i, j])) {
                            new_row <- df[i, ]
                            new_row[] <- NA
                            new_row$"Date" <- df$"Date"[i]
                            new_row$"Released To (select from drop down list)" <- df[i, j]
                            new_row$"Units Released per person" <- df$"Units Released per person"[i]
                            # Add the new row to the new dataframe
                            new_df <- rbind(new_df, new_row)
                        }
                    }
                }
            }
            logDebug("The wide-format area has been converted to long-format!")
            return(new_df)
        },
        error = function(e) {
            # Handle errors
            logError(
                log_to_json(
                    "Error occurred while converting a wide-format area to a long-format, dataframe was returned without changes. Error: {values['e']}",
                    values = list(e = e$message),
                    script = "script1",
                    file = "wide_format_2_long_format.R",
                    errorCode = "tryCatch",
                    functionName = "create_new_rows"
                )
            )
            # Return original df
            return(df)
        },
        warning = function(w) {
            # Handle warnings
            logWarn(
                log_to_json(
                    "Warning occurred while converting a wide-format area to a long-format, dataframe was returned without changes. Warning: {values['w']}",
                    values = list(w = w$message),
                    script = "script1",
                    file = "wide_format_2_long_format.R",
                    warningCode = "tryCatch",
                    functionName = "create_new_rows"
                )
            )
            # Return original df
            return(df)
        }
    )
}
