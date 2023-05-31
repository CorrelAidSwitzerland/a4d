#---------------- # Datacross - A4D Project: Phase 1 ------------- #
#  ---------------- # Preprocessing: Medical Supply Data (MSD) ----- #
#  ---------------- # Script version: 1.3 -------------------------- #


# TODOs:
  # 1. DONE. "Units Received" sometimes means "Product_Units_Received" and sometimes
      # "Product_Balance". See Codebook. Make sure the difference is figured
      # out correctly by the extraction functions. Codebook adjusted, for raw columns "Units Received"
      # and "Received from".
  # 2. DONE (Codebook. up to date). Similar issues: 'Extra Unis Kept in Clinic' sometimes 'product_units_returned'
  #    and sometimes 'product_units_extra_keptinclinic'. Make sure to differ. Codebook up to date?
  #    See especially years 2017+2018 in old version '00_Preprocessing_MSD_V1.3.R"
  # 2.5 DONE. Test whether both helper functions in 00_helper_product_data.R work for all years
  #       and if word searches need to be done for years separately
  # 3. In the real data sometimes there are product rows which are concatenated
  #    multiple products in one cell. Make sure to treat this as an exception and
  #    try to extract the products correctly anyway. #!see May17 and Jun17. Select by "); " or by ") and " .
  # 4. DONE. Make sure final dataframe contains all columns as specified in the codebook, even if not present
  #    in this specific version (due to accidental deletion or because not present in this year.)
  # 5. Test if box units can be extracted and put into one separate columns "product_name_units" (e.g. "2" or "2 boxes" or "50's").
  #    For this, use everything in brackets after the product name.



#### Input ####
tracker_root_path <- select_A4D_directory()
tracker_file <- rstudioapi::selectFile(path = tracker_root_path, filter = "Excel Workbook (*.xlsx)")
codebook_data_file <- "master_tracker_variables.xlsx" # Define path of codebook

#### Define product data function ####

# @Description: Function to completely read and process product data from tracker
# @tracker_data_file: Path to tracker
# @codebook: Codebook with synonyms for product data columns
reading_a4d_products_from_tracker <- function(tracker_data_file, codebook_data_file) {

  # Initialization
  # columns_synonyms <- codebook
  #codebook_product <-
  #    read_column_synonyms(synonym_file = "synonyms_product.yaml")
  #colnames(codebook_product) <- c('name_clean','name_to_be_matched')
  columns_synonyms <- read_column_synonyms_product(codebook_data_file)

  # Set parameters
  sheet_list <- readxl::excel_sheets(tracker_data_file)
  month_list <-sheet_list[na.omit(pmatch(month.abb, sheet_list))]
  year <- 2000 + unique(parse_number(month_list))

  # Extract year
  print(year)
  tidy_tracker_list <- NULL

  # Remove CurrSheet
  rm(CurrSheet) ; rm(df_final) ; rm(df_out)


  ## Loop ####

  for(CurrSheet in month_list){

    print(CurrSheet)
    rm(product_df)

    tracker_data <- data.frame(readxl::read_xlsx(tracker_data_file, CurrSheet))
    print("tracker read in")

    cc_codes <- extract_country_clinic_code(tracker_data)
    country_code <- cc_codes$country_code
    clinic_code <- cc_codes$clinic_code

  #### Extract raw data ####

    # Jump to next tab sheet if there are no product data
    if(!
       any((grepl("Product", tracker_data[, ]) | grepl("Description of Support", tracker_data[, ])))
       ){
      # go to next month
      print(paste0(CurrSheet, " is skipped!"))
      next
    }

    # Extract relevant data and renamn columns. If after extraction, dataframe is empty, this iteration is also skipped.
    product_df <- extract_product_data(tracker_data)
    if(all(is.na(product_df) == TRUE)){
      print(paste0(CurrSheet, " is skipped!"))
      next
    }
    product_df <- product_df %>% harmonize_input_data_columns(columns_synonyms)

    # Remove meaningless rows with only information in one column or no information
    del_rows <- apply(product_df, MARGIN = 1, function(x) sum(!is.na(x))) # count per row how many cols are not NA. If only 0 or 1, there is not enough information and row is dropped.
    del_rows <- as.numeric(which(del_rows <= 1))

    product_df <- product_df %>%
      dplyr::slice(., -del_rows) %>%
      filter_all(any_vars(complete.cases(.))) %>% # Remove empty rows
      dplyr::filter(product != 'Product' | is.na(product)) # remove new headers from data 2022 onwards

    # Split product cells with various products and/or unit information
    product_df <- extract_product_multiple(product_df)

  #### Add final dataframe columns ####
    # Add columns that should be in final dataframe but are still missing

    columns_missing <- codebook_data_file %>%
      readxl::read_xlsx(sheet = "synonyms_ProductData") %>%
      as_tibble() %>%
      pivot_longer(cols = everything(),
                   names_to = "name_clean",
                   values_to = "name_to_be_matched") %>%
      as_tibble() %>%
      group_by(name_clean) %>% distinct(., name_clean) %>% unlist() %>% as.character()



    missing_cols <- which(columns_missing %notin% colnames(product_df))
    missing_cols_names <- unique(columns_missing[missing_cols])
    product_df[missing_cols_names] <- NA

  #### Recode columns where necessary ####

    # Add row index
    product_df$index <- seq(1,nrow(product_df),1)

    # Recode date
    product_df <- format_date(product_df)

    # Extend product name and sort by product
    # Keep first row and last row and order the rest by date
    product_df <- product_df %>% ungroup() %>% tidyr::fill(c(product, product_entry_date), .direction = "down") %>%
        group_by(product) %>%
        mutate(rank = ifelse(row_number() == 1, 1,
                             if_else(row_number() == n(), n()+2, dense_rank(product_entry_date)+1))) %>%
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

    # Compute balance
      # Remove rows without changes in units release or without information on balance
    product_df <- compute_balance_cleanrows(product_df)

      # Compute balance_status (start vs. end vs. change)
    product_df <- compute_balance_status(product_df)

      # Compute balance
    product_df <- compute_balance(product_df, year)

    # Add country, hospital, month, year, tabname
    product_df <- product_df %>%
      mutate(product_country = toupper(country_code),
             product_hospital = toupper(clinic_code),
             product_table_month = extract_month(CurrSheet),
             product_table_year = year,
             product_sheet_name = CurrSheet)

    # Adjust classes of variables
    product_df <- adjust_column_classes(product_df)
    product_df <- subset(product_df, select=-index)


  #### Combine all month sheets and return final data ####

    cat("\014")
    print(paste0('Preprocessing for month ', CurrSheet, ' was successful!'))
    testit(3.7)
    if(!exists("df_final")){
      df_final <- product_df
    } else{
      df_final <- rbind(df_final, product_df)
    }

  }

  return(df_final)
}

a4d_out_2017 <- reading_a4d_products_from_tracker(tracker_file, codebook_data_file)

#### End ####


