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

#' @title Find Columns Containing Specific String
#'
#' @description
#' This function returns the column names of a dataframe that contain a specific string.
#'
#' @param df A dataframe in which to search for the string.
#' @param string The string to search for in the column names.
#'
#' @return A character vector of matching column names.
#'
#' @examples
#' data(mtcars)
#' find_string_cols(mtcars, "mpg")
find_string_cols <- function(df, string) {
    matching_cols <- colnames(df)[grepl(string, names(df))]
    return(matching_cols)
}

#' @title Check for Specific Patterns in a Dataframe Column
#'
#' @description
#' This function checks if any cell in a specific column of a dataframe contains a pattern with a hyphen followed by a digit, or a comma (which could be leading with a space). It returns TRUE if any cell in the column contains the pattern, and FALSE otherwise.
#'
#' @param df A dataframe to check for the specific patterns.
#' @param column_name The name of the column to check for the patterns.
#'
#' @return A logical value indicating whether any cell in the specified column contains the patterns.
#'
#' @examples
#' data(mtcars)
#' check_patterns_in_column(mtcars, "mpg")
#'
#' @export
check_patterns_in_column <- function(df, column_name) {
    # pattern <- "(-\\d)|(,\\s*)"
    pattern <- "(-\\s*\\d)|(,\\s*)"
    return(any(grepl(pattern, df[[column_name]])))
}

#' @title Remove Original Cells based on Relationship
#'
#' @description
#' This function removes rows from the column `col_units_rel` where the corresponding value in `col_rel_to` contains a comma (",").
#' It also removes those comma-separated values from the `col_rel_to` column itself.
#'
#' @param df A data frame containing the columns `col_units_rel` and `col_rel_to`.
#' @param col_units_rel The name of the column containing units (cells) potentially related to multiple other units.
#' @param col_rel_to The name of the column containing relationships between units, where comma (",") indicates a relationship with multiple units.
#'
#' @return
#' The modified data frame where rows with comma-separated relationships in `col_rel_to` have corresponding values in `col_units_rel` set to NA and the comma-separated values removed from `col_rel_to`.
#' @examples
#' # Create a sample data frame
#' df <- data.frame(col_units_rel = c(1, 2, 3, 4), col_rel_to = c("A", "B,C", "D", "A,B"))
#'
#' # Remove original cells based on relationship
#' df_modified <- remove_original_cells(df, col_units_rel = "col_units_rel", col_rel_to = "col_rel_to")
#'
#' # Print the modified data frame
#' print(df_modified)
remove_original_cells <- function(df, col_units_rel, col_rel_to) {
    df[[col_units_rel]][df[[col_rel_to]] %in% grep(",", df[[col_rel_to]], value = TRUE)] <- NA
    df[[col_rel_to]][df[[col_rel_to]] %in% grep(",", df[[col_rel_to]], value = TRUE)] <- NA
    return(df)
}

#' @title Replace Extra "Total" Values with NA
#'
#' @description
#' This function replaces values in a specified column (`column_name`) with NA if one of the two preceding columns contains the word "Total".
#' This is intended to handle cases where there might be multiple "Total" values in close proximity, keeping only the first.
#'
#' @param df A data frame containing the column to be processed.
#' @param column_name The name of the column where extra "Total" values will be replaced with NA.
#'
#' @return
#' The modified data frame where any value in `column_name` preceded by "Total" in one of the two preceding columns is replaced with NA.
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(col1 = c("A", "B", "Total"), col2 = c(1, 2, 3), col3 = c(4, 5, 6), col4 = c(7, "Total", 9))
#'
#' # Replace extra "Total" values with NA
#' df_modified <- replace_extra_total_values_with_NA(df, column_name = "col4")
#'
#' # Print the modified data frame
#' print(df_modified)
replace_extra_total_values_with_NA <- function(df, column_name) {
    # Get the column index
    col_index <- which(names(df) == column_name)

    # Check if the column index is greater than 2 (to avoid negative indexing)
    if (col_index > 2) {
        # Loop over rows of the dataframe
        for (i in 1:nrow(df)) {
            # If one of the two previous columns contains 'Total' (case-insensitive), replace the cell with NA
            if ("total" %in% tolower(df[i, (col_index - 2):(col_index - 1)])) {
                df[i, column_name] <- NA
            }
        }
    }

    return(df)
}

#' @title Convert Wide-Format Cells to Long-Format Rows
#'
#' @description
#' This function converts a data frame from wide-format to long-format. It assumes specific column naming conventions:
#' - Columns containing "Released To" are expected to have comma-separated values representing units released to.
#' - Columns containing "Units Released" are expected to store the corresponding unit quantity for each "Released To" value.
#' - A column containing "Date" (or similar) should exist to hold the date information.
#'
#' @param df A data frame in wide-format with columns named as described.
#'
#' @return
#' A new data frame in long-format where each row represents a single release event with columns:
#' - The original columns from the input data frame.
#' - "Released To": The recipient unit for a specific release.
#' - "Units Released": The quantity of units released to the corresponding recipient.
#' - "Date": The date of the release event (unchanged from the input).
#' - "Received From": unchanged from the input.
#'
#' @examples
#' # Create a sample data frame in wide-format
#' df <- data.frame(
#'     ID = c(1, 2, 3),
#'     "Units Available" = c(10, 15, 20),
#'     "Released To A,B" = c(NA, "5-2", "8-1"),
#'     Date = c("2024-01-01", "2024-02-01", "2024-03-01")
#' )
#'
#' # Convert wide-format cells to long-format rows
#' df_long <- wide_cells_2_rows(df)
#'
#' # Print the long-format data frame
#' print(df_long)
wide_cells_2_rows <- function(df) {
    tryCatch(
        {
            col_released_to <- find_string_cols(df, "Released To")
            col_units_released <- find_string_cols(df, "Units Released")
            col_date <- find_string_cols(df, "Date")
            col_recived_from <- find_string_cols(df, "Received From")

            # Create an empty dataframe to store the new rows
            new_df <- df[FALSE, ]
            # Loop over rows of the dataframe
            for (i in 1:nrow(df)) {
                # Add the current row to the new dataframe
                new_df <- rbind(new_df, df[i, ])
                # If the cell in 'Released To ...' is not empty, split its value by comma
                if (!is.na(df[[col_released_to]][i]) & grepl(",", df[[col_released_to]][i])) {
                    # Split the cell content by comma
                    cell_content <- strsplit(as.character(df[[col_released_to]][i]), ",")[[1]]
                    for (content in cell_content) {
                        # Split the content by '-'
                        split_content <- strsplit(content, "-")[[1]]
                        new_row <- df[i, ]
                        new_row[] <- NA
                        new_row[[col_date]] <- df[[col_date]][i]
                        new_row[[col_recived_from]] <- df[[col_recived_from]][i]
                        # Assign the part before '-' to 'Released To ...'
                        new_row[[col_released_to]] <- split_content[1]
                        # Assign the part after '-' to 'Units Released'
                        new_row[[col_units_released]] <- split_content[2]
                        # Add the new row to the new dataframe
                        new_df <- rbind(new_df, new_row)
                    }
                }
            }

            new_df <- remove_original_cells(new_df, col_units_released, col_released_to)
            new_df <- replace_extra_total_values_with_NA(new_df, col_units_released)

            logDebug(
                log_to_json(
                    "The wide-format cells has been converted to long-format!",
                    script = "script1",
                    file = "wide_format_2_long_format.R",
                    functionName = "wide_cells_2_rows"
                )
            )
            return(new_df)
        },
        error = function(e) {
            # Handle errors
            logError(
                log_to_json(
                    "Error occurred while converting a wide-format cells to a long-format, dataframe was returned without changes. Error: {values['e']}",
                    values = list(e = e$message),
                    script = "script1",
                    file = "wide_format_2_long_format.R",
                    errorCode = "tryCatch",
                    functionName = "wide_cells_2_rows"
                )
            )
            # Return original df
            return(df)
        },
        warning = function(w) {
            # Handle warnings
            logWarn(
                log_to_json(
                    "Warning occurred while converting a wide-format cells to a long-format, dataframe was returned without changes. Warning: {values['w']}",
                    values = list(w = w$message),
                    script = "script1",
                    file = "wide_format_2_long_format.R",
                    warningCode = "tryCatch",
                    functionName = "wide_cells_2_rows"
                )
            )
            # Return original df
            return(df)
        }
    )
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
            logDebug(
                log_to_json(
                    "The wide-format area has been converted to long-format!",
                    script = "script1",
                    file = "wide_format_2_long_format.R",
                    functionName = "create_new_rows"
                )
            )

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
