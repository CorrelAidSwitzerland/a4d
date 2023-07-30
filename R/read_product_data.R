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

        # loop through all months
        for (CurrSheet in month_list) {
            print(CurrSheet)

            # open tracker data
            tracker_data <- data.frame(readxl::read_xlsx(tracker_data_file, CurrSheet,
                .name_repair = "unique_quiet"
            ))

            # Jump to next tab sheet if there are no product data
            if (!
            any((grepl("Product", tracker_data[, ]) | grepl("Description of Support", tracker_data[, ])))
            ) {
                # go to next month
                print(paste0(CurrSheet, " is skipped!"))
                next
            }

            # Extract relevant data and renamn columns
            product_df <- extract_product_data(tracker_data)

            # If after extraction, dataframe is empty, this iteration is also skipped.
            if (all(is.na(product_df))) {
                print(paste0(CurrSheet, " is skipped!"))
                next
            }

            # harmonize to names
            product_df <- product_df %>% harmonize_input_data_columns(columns_synonyms)

            # Remove meaningless rows with no information
            del_rows <- apply(product_df, MARGIN = 1, function(x) sum(!is.na(x))) # count per row how many cols are not NA. If only 0, there is not enough information and row is dropped.
            del_rows <- as.numeric(which(del_rows < 1))

            product_df <- product_df %>%
                dplyr::slice(., -del_rows) %>%
                filter_all(any_vars(complete.cases(.))) %>% # Remove empty rows
                dplyr::filter((product != "Product" | is.na(product)) & (product != "PATIENT DATA SUMMARY" | is.na(product))) # remove new headers from data 2022 onwards

            # Checking if the patient's name is missing next to the released units
            col_released <- "product_units_released"
            col_released_to <- "product_released_to"
            num_na_rows <- count_na_rows(product_df, col_released, col_released_to)
            tryCatch(
                {
                    if (num_na_rows > 0) {
                        logInfo(CurrSheet, " the number of rows where the patient's name is missing: ", col_released, " is not NA and ", col_released_to, " (patient's name) is NA = ", num_na_rows)
                    }
                },
                error = function(e) {
                    logError(CurrSheet, " trying with num_na_rows for products. Error: ", e$message)
                }
            )

            # Checking non-processed dates in product_entry_date
            non_processed_dates <- product_df %>%
                filter(!is.na(product_entry_date) & !grepl("^[0-9]+$", product_entry_date)) # filter if: no NA and the string is not only numeric.
            tryCatch(
                {
                    if (nrow(non_processed_dates) > 0) {
                        logInfo(
                            CurrSheet,
                            " the number of rows with non-processed dates in product_entry_date is ",
                            nrow(non_processed_dates), ": ",
                            paste(non_processed_dates$product_entry_date, collapse = ",")
                        )
                    }
                },
                error = function(e) {
                    logError(CurrSheet, " trying with non_processed_dates in product_entry_date. Error: ", e$message)
                }
            )

            # Add country, hospital, month, year, tabname
            product_df <- product_df %>%
                mutate(
                    product_table_month = extract_month(CurrSheet),
                    product_table_year = year,
                    product_sheet_name = CurrSheet
                )

            # combine all months
            if (!exists("df_final")) {
                df_final <- product_df
            } else {
                df_final <- bind_rows(df_final, product_df)
            }
        }
        if (exists("df_final")) {
            return(df_final)
        } else {
            return(NULL)
        }
        logDebug("Finish reading_product_data_step1.")
    }

# function for checking if the patient's name is missing next to the released units
count_na_rows <- function(df, units_released_col, released_to_col) {
    na_rows <- df[is.na(df[[released_to_col]]) & !is.na(df[[units_released_col]]), ]
    nrow(na_rows)
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
        logInfo(paste(length(na_rows[na_rows == T]), ' rows deleted out of ', nrow(df), ' rows (reason: rows not containing additional info).', sep=''))

        # Return the data frame without the NA rows
        return(df[!na_rows, ])
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

        # loop through all months
        for (sheet_month in unique(df$product_sheet_name)) {
            logDebug(paste("Start processing the following sheet:", sheet_month))

            # remove former
            rm(product_df)

            # filter on month sheet
            product_df <- df %>%
                dplyr::filter(product_sheet_name == sheet_month)

            # Split product cells with various products and/or unit information
            product_df <- extract_product_multiple(product_df)

            # Add columns that should be in final dataframe but are still missing
            columns_missing <- columns_synonyms %>%
                group_by(name_clean) %>%
                distinct(., name_clean) %>%
                unlist() %>%
                as.character()

            missing_cols <- which(columns_missing %notin% colnames(product_df))
            missing_cols_names <- unique(columns_missing[missing_cols])
            product_df[missing_cols_names] <- NA

            # Remove rows which do not contain any new information
            column_names_check <- c('product_entry_date','product_units_received','product_received_from','product_units_released','product_released_to','product_units_returned','product_returned_by')
            product_df <- remove_rows_with_na_columns(product_df, column_names_check)
            # jump to next sheet if dataframe empty from here
            if(nrow(product_df) == 0){
                logDebug(paste(sheet_month, ' sheet is empty after filtering and skipped', sep=''))
                next
            }

            # Add row index
            product_df$index <- seq(1, nrow(product_df), 1)

            # Recode date
            product_df <- format_date(product_df)

            # Extend product name and sort by product
            # Keep first row and last row and order the rest by date
            product_df <- product_df %>%
                ungroup() %>%
                tidyr::fill(c(product, product_entry_date), .direction = "down") %>%
                group_by(product) %>%
                mutate(rank = ifelse(row_number() == 1, 1,
                    if_else(row_number() == n(), n() + 2, dense_rank(product_entry_date) + 1)
                )) %>%
                arrange(product, rank) %>%
                ungroup() %>%
                select(-rank)

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

            #### hospital and country information missing here!!

            # finish and combine
            df_final <- df_final %>%
                rbind(product_df)

            logInfo(paste("Finished processing the following sheet:", sheet_month))
        }

        if (nrow(df_final) > 0) {
            return(df_final)
        } else {
            logDebug(paste("No product data extracted for the following tracker:", df$file_name[1]))
        }
    }
