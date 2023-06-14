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
                dplyr::filter(product != "Product" | is.na(product)) # remove new headers from data 2022 onwards

            # Checking if the patient's name is missing next to the released units
            col_released <- "product_units_released"
            col_released_to <- "product_released_to"
            num_na_rows <- count_na_rows(product_df, col_released, col_released_to)
            if (num_na_rows > 0) {
                logInfo(CurrSheet, " the number of rows where the patient's name is missing: ", col_released, " is not NA and ", col_released_to, " (patient's name) is NA = ", num_na_rows)
            }

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
