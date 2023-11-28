# function to process product data in step 1
# extract product data with existing functions
# rename columns and remove empty rows/columns
# function based on parts from run_a4d_product_data.R and helper functions
reading_product_data_step1 <-
    function(tracker_data_file, columns_synonyms) {
        logDebug("Start reading_product_data_step1.")
        # rename column names to match
        colnames(columns_synonyms) <- c("name_clean", "name_to_be_matched")

        # get all month sheets with product data, get year
        sheet_list <- readxl::excel_sheets(tracker_data_file)
        month_list <- sheet_list[na.omit(pmatch(month.abb, sheet_list))]
        year <- get_tracker_year(tracker_data_file, month_list)

        logDebug("Found ", length(sheet_list), " sheets: ", paste(sheet_list, collapse = ", "), ".")
        logDebug("Found ", length(month_list), " month sheets: ", paste(month_list, collapse = ", "), ".")


        # loop through all months
        for (curr_sheet in month_list) {
            logDebug("Start processing the following sheet: ", curr_sheet)

            # open tracker data
            tracker_data <- data.frame(
                readxl::read_xlsx(
                    path = tracker_data_file,
                    sheet = curr_sheet,
                    col_types = c("text"),
                    .name_repair = "unique_quiet"
                )
            )

            # Jump to next tab sheet if there are no product data
            if (
                !any((grepl("Product", tracker_data[, ]) |
                    grepl("Description of Support", tracker_data[, ])))
            ) {
                # go to next month
                logInfo("Could not find product data in tracker data. Skipping ", curr_sheet, ".")
                next
            }

            # Extract relevant data and renamn columns
            product_df <- extract_product_data(tracker_data)

            # If after extraction, dataframe is empty, this iteration is also skipped.
            if (all(is.na(product_df))) {
                logInfo("Product data is empty. Skipping ", curr_sheet, ".")
                next
            }

            # harmonize to names
            product_df <- product_df %>% harmonize_input_data_columns(columns_synonyms)

            # Remove meaningless rows with no information
            del_rows <- apply(product_df, MARGIN = 1, function(x) sum(!is.na(x))) # count per row how many cols are not NA. If only 0, there is not enough information and row is dropped.
            del_rows <- as.numeric(which(del_rows < 1))

            product_df <- product_df %>%
                dplyr::slice(., -del_rows) %>%
                dplyr::filter_all(dplyr::any_vars(complete.cases(.))) %>% # Remove empty rows
                dplyr::filter((product != "Product" | is.na(product)) & (product != "PATIENT DATA SUMMARY" | is.na(product))) # remove new headers from data 2022 onwards

            # Checking if the patient's name is missing next to the released units
            col_released <- "product_units_released"
            col_released_to <- "product_released_to"
            num_na_rows <- count_na_rows(product_df, col_released, col_released_to)
            tryCatch(
                {
                    if (num_na_rows > 0) {
                        logInfo(curr_sheet, " the number of rows where the patient's name is missing: ", col_released, " is not NA and ", col_released_to, " (patient's name) is NA = ", num_na_rows)
                    }
                },
                error = function(e) {
                    logError(curr_sheet, " trying with num_na_rows for products. Error: ", e$message)
                }
            )

            # Checking non-processed dates in product_entry_date
            non_processed_dates <- product_df %>%
                dplyr::filter(!is.na(product_entry_date) & !grepl("^[0-9]+$", product_entry_date)) # filter if: no NA and the string is not only numeric.
            tryCatch(
                {
                    if (nrow(non_processed_dates) > 0) {
                        logWarn(
                            curr_sheet,
                            " the number of rows with non-processed dates in product_entry_date is ",
                            nrow(non_processed_dates), ": ",
                            paste(non_processed_dates$product_entry_date, collapse = ", ")
                        )
                    }
                },
                error = function(e) {
                    logError(curr_sheet, " trying with non_processed_dates in product_entry_date. Error: ", e$message)
                }
            )

            # Add country, hospital, month, year, tabname
            product_df <- product_df %>%
                dplyr::mutate(
                    product_table_month = extract_month(curr_sheet),
                    product_table_year = year,
                    product_sheet_name = curr_sheet
                )

            # Check if the entry dates for the balance match the month/year on the sheet
            check_entry_dates(product_df, curr_sheet)

            logDebug("Finish processing sheet: ", curr_sheet)

            # combine all months
            if (!exists("df_final")) {
                df_final <- product_df
            } else {
                df_final <- dplyr::bind_rows(df_final, product_df)
            }
        }
        if (exists("df_final")) {
            return(df_final)
        } else {
            return(NULL)
        }
        logDebug("Finish reading_product_data_step1.")
    }


#' @title Count rows with missing patient's name next to the released units
#'
#' @description
#' This function checks if the patient's name is missing next to the released units in a given dataframe. It counts the number of such rows.
#'
#' @param df A dataframe containing patient data.
#' @param units_released_col The name of the column that contains information about the units released.
#' @param released_to_col The name of the column that contains information about who the units were released to (patient's name).
#'
#' @return The number of rows where the patient's name is missing next to the released units.
count_na_rows <- function(df, units_released_col, released_to_col) {
    na_rows <- df[is.na(df[[released_to_col]]) & !is.na(df[[units_released_col]]), ]
    nrow(na_rows)
}


#' @title Check entry dates in product data
#'
#' @description
#' This function checks if the entry dates for the balance match the month/year on the sheet. If any discrepancies are found, it logs a warning message with the number of mismatched dates and their corresponding 'product_entry_date' values.
#'
#' @param df A dataframe containing product data.
#' @param Sheet The name of the sheet where the product data is located.
#'
#' @return This function does not return a value. It logs a warning message if there are any dates in 'product_entry_date' that don't match the month/year on the sheet.
check_entry_dates <- function(df, Sheet) {
    logDebug("Start check_entry_dates.")
    # Check if the entry dates for the balance match the month/year on the sheet
    entry_dates_df <- df %>% dplyr::filter(grepl("^[0-9]+$", product_entry_date))

    entry_dates_df$product_entry_date <- as.numeric(entry_dates_df$product_entry_date)
    entry_dates_df$product_table_month <- as.numeric(entry_dates_df$product_table_month)

    # Extract month and year from product_entry_date column
    entry_dates_df$ed_date <- as.Date(entry_dates_df$product_entry_date, origin = "1899-12-30")
    entry_dates_df$ed_month <- as.numeric(format(entry_dates_df$ed_date, "%m"))
    entry_dates_df$ed_year <- as.numeric(format(entry_dates_df$ed_date, "%Y"))

    # Compare month and year with product_table_month and product_table_year
    not_same <- entry_dates_df[entry_dates_df$ed_month != entry_dates_df$product_table_month |
        entry_dates_df$ed_year != entry_dates_df$product_table_year, ]
    if (nrow(not_same) > 0) {
        logWarn(
            Sheet,
            " the number of dates in product_entry_date that don't match the month/year on the sheet is ",
            nrow(not_same), ": ",
            paste(not_same$ed_date, collapse = ", ")
        )
    }
    logDebug("Finish check_entry_dates.")
}

#' @title Remove Rows with NA Values in Specified Columns.
#'
#' @description
#' This function takes a data frame and a vector of column names as input. It removes rows from the data frame where all the specified columns contain only NA values. If any of the specified columns have at least one non-NA value, the corresponding row will be retained in the output data frame.
#'
#' @param df A data frame that contains the data from which rows need to be removed.
#' @param column_names A character vector specifying the column names to be checked for NA values.
#'
#' @return A new data frame with rows removed if all the specified columns in the row contain NA values. The returned data frame will have the same structure as the input data frame, but with rows that satisfy the condition removed.
remove_rows_with_na_columns <-
    function(df, column_names) {
        # Get the row indices where all specified columns are NA
        na_rows <- apply(df[column_names], 1, function(x) all(is.na(x)))

        # log message
        logInfo(paste(length(na_rows[na_rows == T]), " rows deleted out of ", nrow(df), " rows (reason: rows not containing additional info).", sep = ""))

        # Return the data frame without the NA rows
        return(df[!na_rows, ])
    }


#' @title Check negative values in 'product_balance' column
#'
#' @description
#' This function checks for negative values in the 'product_balance' column of a given dataframe. If any negative values are found, it logs a warning message with the number of negative values and their corresponding 'product_balance' values.
#'
#' @param df A dataframe containing product data.
#' @param Sheet The name of the sheet where the product data is located.
#'
#' @return This function does not return a value. It logs a warning message if there are any negative values in the 'product_balance' column.
check_negative_balance <- function(df, Sheet) {
    # Create a new data frame containing only rows with negative values in product_balance column
    negative_df <- df[df$product_balance < 0, ]

    # Check if there are any rows in the new data frame
    if (nrow(negative_df) > 0) {
        # Log a warning message with the number of negative values and their corresponding product_balance values
        logWarn(
            Sheet,
            " number of negative values in product_balance on the sheet is ",
            nrow(negative_df), ": ",
            paste(negative_df$product_balance, collapse = ", ")
        )
    }
}


#' @title Switch product_received_from and product_units_received column.
#'
#' @description
#' Renaming columns when string Remaining stock in wrong column. E.g. 2018_PNG for Nov and Dec.
#'
#'
#' @param df Dataframe. Output of tracker file from script 1 for product data.
#'
#' @return Dataframe with columns switched (renamed)
switch_columns_stock <-
    function(df) {
        if (sum(stringr::str_detect(df$product_units_received[!is.na(df$product_units_received)], "Remaining Stock")) > 0) {
            df <- df %>%
                dplyr::rename(
                    "product_units_received" = "product_received_from",
                    "product_received_from" = "product_units_received"
                )
            logDebug("Columns product_units_received and product_received_from were switched")
            return(df)
        } else {
            return(df)
        }
    }

#' @title Compare two lists and return unmatched strings
#'
#' @description
#' This function compares two lists and returns the strings that are present in the first list but not in the second list.
#'
#' @param list1 The first list of strings to be compared.
#' @param list2 The second list of strings to be compared.
#'
#' @return A vector of strings that are present in 'list1' but not in 'list2'.
compare_lists <- function(list1, list2) {
    # Use the setdiff function to find strings in list1 that are not in list2
    unmatched_strings <- setdiff(list1, list2)
    return(unmatched_strings)
}


#' @title Report unknown products
#'
#' @description
#' This function reports unknown products. It compares the product list in a given dataframe with a stock list, and logs any products that are not found in the stock list.
#'
#' @param df A dataframe containing product data.
#' @param Sheet The name of the sheet where the product data is located.
#' @param stock_list_df A dataframe containing the stock list of products.
#'
#' @return This function does not return a value. It logs a warning message if there are any unknown products, and logs an info message if there are no unknown products.
report_unknown_products <- function(df, Sheet, stock_list_df) {
    # Create lists containing only products names
    products_list <- df$product
    stock_products_list <- stock_list_df$product

    products_list <- tolower(products_list)
    products_list <- products_list[!is.na(products_list)]
    stock_products_list <- tolower(stock_products_list)

    # Create a new list containing only unknown products names
    unmatched_products <- compare_lists(products_list, stock_products_list)

    # Check if there are any unknown products names
    if (length(unmatched_products) > 0) {
        # Log a warning message with the number of unknown products names
        logWarn(
            Sheet,
            " the number of unknown product names on the sheet is ",
            length(unmatched_products), ": ",
            paste(unmatched_products, collapse = ", ")
        )
    } else {
        logInfo(Sheet, " no unknown product names on the sheet")
    }
}


#' @title Load Product List from Stock Summary
#'
#' @description
#' This function loads the product list from 'Stock_Summary' sheet in an Excel file.
#'
#' @param stock_summary_xlsx A string that represents the path to the Excel file. Defaults to "reference_data/master_tracker_variables.xlsx".
#'
#' @return A data frame that contains the product names. If there is an error during the process, it logs the error message.
#'
#' @examples
#' \dontrun{
#' product_list <- load_product_reference_data()
#' product_list <- load_product_reference_data("your_file.xlsx")
#' }
load_product_reference_data <- function(stock_summary_xlsx = "reference_data/master_tracker_variables.xlsx") {
    logDebug("Trying to load the product list from the Stock Summary, ", stock_summary_xlsx, "...")
    tryCatch(
        {
            product_names_df <- readxl::read_excel(stock_summary_xlsx, "Stock_Summary")
            colnames(product_names_df) <- tolower(colnames(product_names_df))
            logDebug(nrow(product_names_df), " product names were loaded from the Stock Summary.")
            return(product_names_df)
        },
        error = function(e) {
            logError("Error in loading stock product list: ", e)
        }
    )
}

#' Inserts product categories if contained in a provided mapping
#'
#' @description Product categories are added in the sense of a left join,
#' meaning missing categories will result in NA values while products are never
#' dropped.
#'
#' @param inventory_data A product inventory dataframe containing a "product" column
#' @param product_category_mapping A dataframe mapping "product" to "category"
#'
#' @return A dataframe containing the inventory data with an additional product
#' category column.
add_product_categories <- function(inventory_data, product_category_mapping) {
    inventory_data %>%
        dplyr::left_join(
            dplyr::rename(product_category_mapping, product_category = "category"),
            c("product")
        )
}

#' @title Extract Unit Capacity from a Specified Column
#'
#' @description
#' This function extracts the unit capacity from a specified column in a dataframe.
#' It assumes that the unit capacity is represented by numbers immediately before 's or s within parentheses.
#' Non-numeric characters are removed and the extracted values are converted to numeric.
#' NA values are replaced with 1.
#'
#' @param df A dataframe that contains the column to extract unit capacity from.
#' @param column_name The name of the column to extract unit capacity from.
#'
#' @return A dataframe with an additional column 'product_unit_capacity' that contains the extracted unit capacity.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(product = c("Product A (2s)", "Product B (3's)", "Product C"))
#' df <- extract_unit_capacity(df, "product")
#' }
extract_unit_capacity <- function(df, column_name) {
    logDebug("Trying to extract Unit Capacity from ", column_name, " column")

    # Extract all symbols between parentheses
    df$product_unit_capacity <- stringr::str_extract(df[[column_name]], "\\(([^)]+)\\)")

    # Extract numbers that are immediately before 's or s
    df$product_unit_capacity <- stringr::str_extract(df$product_unit_capacity, "\\d+(?=s|'s)")

    # Remove non-numeric characters
    df$product_unit_capacity <- gsub("[^0-9]", "", df$product_unit_capacity)

    # Convert the new field to numeric
    df$product_unit_capacity <- as.numeric(df$product_unit_capacity)

    # Add 1 to NA values
    df$product_unit_capacity[is.na(df$product_unit_capacity)] <- 1

    logDebug("Finished extracting Unit Capacity from ", column_name, " column")

    return(df)
}

#' @title Process product data in script 2.
#'
#' @description
#' Processes data output from script 1. Data will be further cleaned and processed to fix data quality issues and to create a report.
#'
#'
#' @param df Dataframe. Output of tracker file from script 1 for product data.
#' @param columns_synonyms Tibble with synonyms for product data (columns: name_clean and name_to_be_matched)
#'
#' @return Cleaned product data for one specified tracker.
reading_product_data_step2 <-
    function(df, columns_synonyms) {
        logDebug("Start reading_product_data_step2.")

        # rename column names to match
        colnames(columns_synonyms) <- c("name_clean", "name_to_be_matched")

        # save all results
        df_final <- c()

        # get product list from Stock_Summary
        product_reference_data <- load_product_reference_data()
        known_products <- product_reference_data %>% dplyr::select(product)
        product_category_mapping <- product_reference_data %>% dplyr::select(product, category)

        # loop through all months
        for (sheet_month in unique(df$product_sheet_name)) {
            logDebug(paste("Start processing the following sheet:", sheet_month))

            # filter on month sheet
            product_df <- df %>%
                dplyr::filter(product_sheet_name == sheet_month)

            # Split product cells with various products and/or unit information
            product_df <- extract_product_multiple(product_df)

            # Add columns that should be in final dataframe but are still missing
            columns_missing <- columns_synonyms %>%
                dplyr::group_by(name_clean) %>%
                dplyr::distinct(., name_clean) %>%
                unlist() %>%
                as.character()

            missing_cols <- which(columns_missing %notin% colnames(product_df))
            missing_cols_names <- unique(columns_missing[missing_cols])
            product_df[missing_cols_names] <- NA

            # switch column names if applicable
            product_df <- switch_columns_stock(product_df)

            # Remove rows which do not contain any new information
            column_names_check <- c("product_entry_date", "product_units_received", "product_received_from", "product_units_released", "product_released_to", "product_units_returned", "product_returned_by")
            product_df <- remove_rows_with_na_columns(product_df, column_names_check)
            # jump to next sheet if dataframe empty from here
            if (nrow(product_df) == 0) {
                logDebug(paste(sheet_month, " sheet is empty after filtering and skipped", sep = ""))
                next
            }

            # Add row index
            product_df$index <- seq(1, nrow(product_df), 1)

            # Recode date
            product_df <- format_date(product_df)

            # Extend product name and sort by product
            # Keep first row and last row and order the rest by date
            product_df <- product_df %>%
                dplyr::ungroup() %>%
                tidyr::fill(c(product), .direction = "down") %>%
                dplyr::group_by(product) %>%
                dplyr::mutate(rank = ifelse(dplyr::row_number() == 1, 1,
                    dplyr::if_else(dplyr::row_number() == dplyr::n(), dplyr::n() + 2,
                        dplyr::if_else(is.na(product_entry_date), dplyr::row_number(), dplyr::dense_rank(product_entry_date) + 1)
                    )
                )) %>%
                dplyr::arrange(product, rank) %>%
                dplyr::ungroup() %>%
                dplyr::select(-rank)

            # extract start/end balance when they are in the sheet (e.g. 2019_PKH and 2020_STH examples)
            product_df <- update_receivedfrom(product_df)

            # Recode all NAs in unit columns to 0
            product_df <- recode_unitcolumnstozero(product_df)

            # Clean columns "units received" and "received from" from unexpected character (units) vs. numeric (received from) values.
            # In first function, received_from numeric information (Balance status at START BALANCE) is transferred to variable product_balance for later computation of this variable.
            product_df <- clean_receivedfrom(product_df)
            product_df <- clean_unitsreceived(product_df)

            # Recode all NAs in unit columns to 0, if NA coercion was applied earlier
            product_df <- recode_unitcolumnstozero(product_df)

            ## Compute balance
            # Remove rows without changes in units release or without information on balance
            product_df <- compute_balance_cleanrows(product_df)

            # Compute balance_status (start vs. end vs. change)
            product_df <- compute_balance_status(product_df)

            # Compute balance
            product_df <- compute_balance(product_df, product_df$product_table_year[1])

            # Adjust classes of variables
            product_df <- adjust_column_classes(product_df)

            # remove index column
            product_df <- subset(product_df, select = -index)

            # check negative values in product_balance column
            check_negative_balance(product_df, sheet_month)

            # Report unknown product names
            if (exists("known_products") && !is.null(known_products) && nrow(known_products) > 0) {
                report_unknown_products(product_df, sheet_month, known_products)
            }

            product_df <- product_df %>% add_product_categories(product_category_mapping)

            # extract Unit Capacity from product column
            product_df <- extract_unit_capacity(product_df, "product")

            #### hospital and country information missing here!!

            # finish and combine
            df_final <- df_final %>%
                rbind(product_df)

            logDebug(paste("Finished processing the following sheet:", sheet_month))
        }

        if (nrow(df_final) > 0) {
            return(df_final)
        } else {
            logDebug(paste("No product data extracted for the following tracker:", df$file_name[1]))
        }
    }
