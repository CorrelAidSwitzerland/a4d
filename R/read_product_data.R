# function to process product data in step 1
# extract product data with existing functions
# rename columns and remove empty rows/columns
# function based on parts from run_a4d_product_data.R and helper functions
reading_product_data_step1 <-
    function(tracker_data_file, columns_synonyms) {
        rm(product_df)
        rm(df_final)

        # rename column names to match
        colnames(columns_synonyms) <- c("name_clean", "name_to_be_matched")

        # get all month sheets with product data, get year
        sheet_list <- readxl::excel_sheets(tracker_data_file)
        month_list <- sheet_list[na.omit(pmatch(month.abb, sheet_list))]
        year <- 2000 + unique(parse_number(month_list))

        # loop through all months
        for (CurrSheet in month_list) {
            print(CurrSheet)
            rm(tracker_data)

            # open tracker data
            tracker_data <- data.frame(readxl::read_xlsx(tracker_data_file, CurrSheet))

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
            if (all(is.na(product_df) == TRUE)) {
                print(paste0(CurrSheet, " is skipped!"))
                next
            }

            # harmonize to names
            product_df <- product_df %>% harmonize_input_data_columns(columns_synonyms)

            # Remove meaningless rows with only information in one column or no information
            del_rows <- apply(product_df, MARGIN = 1, function(x) sum(!is.na(x))) # count per row how many cols are not NA. If only 0 or 1, there is not enough information and row is dropped.
            del_rows <- as.numeric(which(del_rows <= 1))

            product_df <- product_df %>%
                dplyr::slice(., -del_rows) %>%
                filter_all(any_vars(complete.cases(.))) %>% # Remove empty rows
                dplyr::filter(product != "Product" | is.na(product)) # remove new headers from data 2022 onwards


            # Add columns that should be in final dataframe but are still missing
            columns_missing <- columns_synonyms %>%
                group_by(name_clean) %>%
                distinct(., name_clean) %>%
                unlist() %>%
                as.character()

            missing_cols <- which(columns_missing %notin% colnames(product_df))
            missing_cols_names <- unique(columns_missing[missing_cols])
            product_df[missing_cols_names] <- NA


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
                df_final <- rbind(df_final, product_df)
            }
        }
        return(df_final)
    }