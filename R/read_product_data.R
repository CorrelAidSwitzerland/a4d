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



# function to process product data in step 3
# further process and re-calculate product balance
# function based on parts from run_a4d_product_data.R and helper functions
reading_product_data_step3 <-
    function(tracker_data_file, columns_synonyms) {

        logDebug("Start reading_product_data_step3.")

        # rename column names to match
        colnames(columns_synonyms) <- c("name_clean", "name_to_be_matched")

        # open tracker
        tracker_df <- read.csv(file=tracker_data_file, sep=',', head = TRUE, stringsAsFactors=FALSE)

        # save all results
        df_final <- c()

        # loop through all months
        for(sheet_month in unique(tracker_df$product_sheet_name)){

            # remove former
            rm(product_df)

            # filter on month sheet
            product_df <- tracker_df %>%
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
        }

    }
