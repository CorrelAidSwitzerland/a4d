---------------- # Datacross - A4D Project: Phase 1 ------------- #
  ---------------- # Preprocessing: Medical Supply Data (MSD) ----- #
  ---------------- # Script version: 1.3 -------------------------- #   
  
  
# TODOs:
  # 1. "Units Received" sometimes means "Product_Units_Received" and sometimes
      # "Product_Balance". See Codebook. Make sure the difference is figured
      # out correctly by the extraction functions.
  # 2. Similar issues: 'Extra Unis Kept in Clinic' sometimes 'product_units_returned'
  #    and sometimes 'product_units_extra_keptinclinic'. Make sure to differ. Codebook up to date?
  #    See espeically years 2017+2018 in old version '00_Preprocessing_MSD_V1.3.R"
  # 3. In the real data sometimes there are product rows which are concatenated
  #    multiple products in one cell. Make sure to treat this as an exception and
  #    try to extract the products correctly anyway.
  
  
# Think about:
  # 1. Why do we extract patient data here? If we need to, then use alexandras
  #    functions to make it easier here.
  
  
  
  
# Packages
lib_list <- c("tidyverse", "tidyxl", "readxl", "readxl", "stringr", "openxlsx")
lapply(lib_list, library, character.only = TRUE)

# Source functions
source("3_Code/00_helper_product_data.R")
source("3_Code/01_a4d_tracker_extract.R")



#### Input ####  

path_output <- "/Volumes/Encrypted_SK/Datacross/A4D/Data/02_preprocdata/" # Define path where preprocessed data file shall be stored

#### Initialize data ####

# DELETE: list_excel_tabs <- str_subset(sheets[!grepl(" ", sheets)], "([1:100])")

# Initalize empty feedback dataframe and empty final dataframe #
setwd(path_output)
wb = createWorkbook() # For feedback sheets


# @Description: Function to completely read and process product data from tracker
# @tracker_data_file: Path to tracker
# @codebook: Codebook with synonyms for product data columns
reading_a4d_products_from_tracker <- function(tracker_data_file, codebook) {
  
  # Initialization
  columns_synonyms <- codebook
  
  # Set parameters
  sheet_list <- excel_sheets(tracker_data_file)
  month_list <-sheet_list[na.omit(pmatch(month.abb, sheet_list))]
  year <- 2000 + unique(parse_number(month_list))
  
  # DELETE? Only important for patient data extraction?
  # Identify all patients for the year based on anonymous data tab
  # patient_sheet <- sheet_list[na.omit(grepl("AN Data", sheet_list))]
  # an_patient_data <- data.frame(read_xlsx(tracker_data_file, patient_sheet))
  # all_patient_ids <- an_patient_data$Patient.ID
  # print("patient AN Data extracted")
  
  # Extract year
  print(year)
  tidy_tracker_list <- NULL
  
  
  ## Loop ####
  sheet_num <- 1
  for (CurrSheet in month_list) {
    
    print(CurrSheet)
    
    tracker_data <- data.frame(read_xlsx(tracker_data_file, CurrSheet))
    print("tracker read in")
    
    cc_codes <- extract_country_clinic_code(tracker_data)
    country_code <- cc_codes$country_code
    clinic_code <- cc_codes$clinic_code
    print("country and clinic code extracted")

    ### Extract raw data ####
    
    # Jump to next tab sheet if there are no product data
    if(!
       any((grepl("Product", tracker_data[, ]) | grepl("Description of Support", tracker_data[, ])))
       ){
      # go to next month
      print(paste0(CurrSheet, " is skipped!"))
      next
    }
    
    # Old msd_df = product_df
    product_df <- extract_product_data(tracker_data) %>%
      harmonize_input_data_columns(columns_synonyms) # TODO: Add logic which correctly sets 
                                               # units_received for different years (see first todo on top)
    patient_df <- extract_patient_data_in_products(tracker_data)
    
  #### Create splits and quality check #####
    
    #### 2021 ####
    if(year == "2021"){

      ##### For medical supplies distribution (MSD) #####
      # Remove empty rows
      product_df <- product_df %>% filter_all(any_vars(complete.cases(.)))  
      
      
      # Change column "Entry Date" to datetime object
      product_df$Entry_Date <- as.numeric(product_df$Entry_Date)  
      product_df[["Entry_Date"]] <- 
        as.POSIXct( product_df[["Entry_Date"]] * (60*60*24)
                    , origin="1899-12-30"
                    , tz="GMT")
      
      
      #### Quality check ####
      
      # Define final feedback file which should contain all necessary input columns of this version
      expected_colnames <- c("Product","Entry_Date","Balance","Units_Received","Received_From","Units_Released","Released_To","Units_Returned","Returned_By")
      expected_classes_char <- c("Product", "Received_From", "Released_To", "Returned_By")
      expected_classes_num <- c("Balance", "Units_Received", "Units_Released", "Units_Returned")
      num_rows <- nrow(product_df)
      df_feedback <- data.frame(matrix(ncol = length(expected_colnames), nrow = num_rows, "CORRECT"))
      colnames(df_feedback) <- expected_colnames
      
      
      # Check1: Is any column name not present?
      if(any(colnames(df_feedback) %notin% colnames(product_df))){
        missing_cols <- which(colnames(df_feedback) %notin% colnames(product_df))
        df_feedback[, missing_cols] <- "ISSUE"
      }
      
      #Check2: Are all column classes expected?
      # For character columns
      if(any(sapply(product_df[, expected_classes_char], class) != "character")){
        wrongclass_cols <- which(sapply(product_df[, expected_classes_char], class) != "character")
        df_feedback[,wrongclass_cols] <- "ISSUE"
      }
      if(!any(grepl("POSIX", is(product_df$Entry_Date))) == TRUE){
        df_feedback$Entry_Date <- "ISSUE"
      }
      # For actual numeric columns, if they contain a character value even after trying to trnasform to numeric
      for(j in unique(expected_classes_num)){
        if(any(is.na(as.numeric(na.omit(unlist(product_df[,j]))))==TRUE)){
          row <- which(is.na(as.numeric(na.omit(unlist(product_df[,j])))))
          df_feedback[row,j] <- "ISSUE"
        }
      }
      
      # Check3: OPEN  
      
      
    }
    
    #### 2020 ####
    if(year == "2020") {
      
      ##### For medical supplies distribution (MSD) #####
     
      # Remove empty rows
      product_df <- product_df %>% filter_all(any_vars(complete.cases(.)))
      
      
      # Change column "Entry Date" to datetime object
      product_df$Entry_Date <- as.numeric(product_df$Entry_Date)
      product_df[["Entry_Date"]] <-
        as.POSIXct(product_df[["Entry_Date"]] * (60 * 60 * 24)
                   , origin = "1899-12-30"
                   , tz = "GMT")
      

      #### Quality check ####
      
      # Define final feedback file which should contain all necessary input columns of this version
      expected_colnames <- c("Product","Entry_Date","Units_Received","Units_Released","Released_To","Units_Returned","Returned_By")
      expected_classes_char <- c("Product", "Received_From", "Released_To", "Returned_By")
      expected_classes_num <- c("Received_From",  "Units_Released", "Units_Returned")
      num_rows <- nrow(product_df)
      df_feedback <- data.frame(matrix(ncol = length(expected_colnames), nrow = num_rows, "CORRECT"))
      colnames(df_feedback) <- expected_colnames
      
      
      # Check1: Is any column name not present?
      if(any(colnames(df_feedback) %notin% colnames(product_df))){
        missing_cols <- which(colnames(df_feedback) %notin% colnames(product_df))
        df_feedback[, missing_cols] <- "ISSUE"
      }
      
      #Check2: Are all column classes expected?
      # For character columns
      if(any(sapply(product_df[, expected_classes_char], class) != "character")){
        wrongclass_cols <- which(sapply(product_df[, expected_classes_char], class) != "character")
        df_feedback[,wrongclass_cols] <- "ISSUE"
      }
      if(!any(grepl("POSIX", is(product_df$Entry_Date))) == TRUE){
        df_feedback$Entry_Date <- "ISSUE"
      }
      # For actual numeric columns, if they contain a character value even after trying to trnasform to numeric
      for(j in unique(expected_classes_num)){
        if(any(is.na(as.numeric(na.omit(unlist(product_df[,j]))))==TRUE)){
          row <- which(is.na(as.numeric(na.omit(unlist(product_df[,j])))))
          df_feedback[row,j] <- "ISSUE"
        }
      }
      
      # Check3: OPEN  
      
      
      
    }
    #### 2019 ####
    if (year == "2019") {
      
      ##### For medical supplies distribution (MSD) #####
      
      
      # Remove empty rows
      product_df <- product_df %>% filter_all(any_vars(complete.cases(.)))
      
      
      # Change column "Entry Date" to datetime object
      product_df$Entry_Date <- as.numeric(product_df$Entry_Date)
      product_df[["Entry_Date"]] <-
        as.POSIXct(product_df[["Entry_Date"]] * (60 * 60 * 24)
                   , origin = "1899-12-30"
                   , tz = "GMT")
      

      #### Quality check ####
      
      # Define final feedback file which should contain all necessary input columns of this version
      expected_colnames <- c("Product","Entry_Date","Units_Received","Received_From", "Units_Released","Released_To","Units_Returned","Returned_By")
      expected_classes_char <- c("Product", "Units_Received",  "Released_To", "Returned_By")
      expected_classes_num <- c("Received_From",  "Units_Released", "Units_Returned")
      num_rows <- nrow(product_df)
      df_feedback <- data.frame(matrix(ncol = length(expected_colnames), nrow = num_rows, "CORRECT"))
      colnames(df_feedback) <- expected_colnames
      
      
      # Check1: Is any column name not present?
      if(any(colnames(df_feedback) %notin% colnames(product_df))){
        missing_cols <- which(colnames(df_feedback) %notin% colnames(product_df))
        df_feedback[, missing_cols] <- "ISSUE"
      }
      
      #Check2: Are all column classes expected?
      # For character columns
      if(any(sapply(product_df[, expected_classes_char], class) != "character")){
        wrongclass_cols <- which(sapply(product_df[, expected_classes_char], class) != "character")
        df_feedback[,wrongclass_cols] <- "ISSUE"
      }
      if(!any(grepl("POSIX", is(product_df$Entry_Date))) == TRUE){
        df_feedback$Entry_Date <- "ISSUE"
      }
      # For actual numeric columns, if they contain a character value even after trying to trnasform to numeric
      for(j in unique(expected_classes_num)){
        if(any(is.na(as.numeric(na.omit(unlist(product_df[,j]))))==TRUE)){
          row <- which(is.na(as.numeric(na.omit(unlist(product_df[,j])))))
          df_feedback[row,j] <- "ISSUE"
        }
      }
      
      
      
    }
    #### 2018 ####
    if (year == "2018") {
      
      # TODO: Account for this specific case for 2018 before
      ##### For medical supplies distribution (MSD) #####
      if(any(grepl("Returned By", colnames(product_df))) == FALSE){
        product_df$`Returned By` <- NA
      }
      
    
      
      # Remove empty rows
      product_df <- product_df %>% filter_all(any_vars(complete.cases(.)))
      
      
      # Change column "Entry Date" to datetime object
      product_df$Entry_Date <- as.numeric(product_df$Entry_Date)
      product_df[["Entry_Date"]] <-
        as.POSIXct(product_df[["Entry_Date"]] * (60 * 60 * 24)
                   , origin = "1899-12-30"
                   , tz = "GMT")
      
 
      #### Quality check ####
      
      # Define final feedback file which should contain all necessary input columns of this version
      expected_colnames <- c("Product","Entry_Date","Units_Received","Received_From","Units_Released","Released_To","Units_Returned","Returned_By")
      expected_classes_char <- c("Product", "Units_Received", "Released_To", "Returned_By")
      expected_classes_num <- c("Received_From", "Units_Released", "Units_Returned")
      num_rows <- nrow(product_df)
      df_feedback <- data.frame(matrix(ncol = length(expected_colnames), nrow = num_rows, "CORRECT"))
      colnames(df_feedback) <- expected_colnames
      
      
      # Check1: Is any column name not present?
      if(any(colnames(df_feedback) %notin% colnames(product_df))){
        missing_cols <- which(colnames(df_feedback) %notin% colnames(product_df))
        df_feedback[, missing_cols] <- "ISSUE"
      }
      
      #Check2: Are all column classes expected?
      # For character columns
      if(any(sapply(product_df[, expected_classes_char], class) != "character")){
        wrongclass_cols <- which(sapply(product_df[, expected_classes_char], class) != "character")
        df_feedback[,wrongclass_cols] <- "ISSUE"
      }
      if(!any(grepl("POSIX", is(product_df$Entry_Date))) == TRUE){
        df_feedback$Entry_Date <- "ISSUE"
      }
      # For actual numeric columns, if they contain a character value even after trying to trnasform to numeric
      for(j in unique(expected_classes_num)){
        if(any(is.na(as.numeric(na.omit(unlist(product_df[,j]))))==TRUE)){
          row <- which(is.na(as.numeric(na.omit(unlist(product_df[,j])))))
          df_feedback[row,j] <- "ISSUE"
        }
      }
      
      # Check3: OPEN  
      
      
      
    }
    #### 2017   |not09 CHANGE MADE HERE ####
    if (year == "2017" & !grepl("Sep", CurrSheet)) {
      
      # TODO: Add this logic somewhere before to make sure to make less work?
      # If there is no section for MSD, jump to next monthly tab ; CHANGE WAS MADE HERE; THIS WAS ADDED DUE TO FEB17
      if(!any(grepl("Description of Support", df_alt1))){
        next 
      }
  
      # TODO: Account for this specific case for 2018 + 2017 before 
      ##### For medical supplies distribution (MSD) #####
    # CHANGE WAS MADE HERE. Added for V1.3
      if(any(grepl("Returned By", colnames(product_df))) == FALSE){
        product_df$`Returned By` <- NA
      }
      
      
      # Remove empty rows
      product_df <- product_df %>% filter_all(any_vars(complete.cases(.)))
      
      # If there are no data, jump to next month ; CHANGE WAS MADE HERE, THIS WAS ADDED
      if(nrow(product_df) == 0){
        next
      }
      
      
      # Change column "Entry Date" to datetime object
      product_df$Entry_Date <- as.numeric(product_df$Entry_Date)
      product_df[["Entry_Date"]] <-
        as.POSIXct(product_df[["Entry_Date"]] * (60 * 60 * 24)
                   , origin = "1899-12-30"
                   , tz = "GMT")
      

      #### Quality check ####
      
      # Define final feedback file which should contain all necessary input columns of this version
      expected_colnames <- c("Product","Entry_Date","Units_Received","Received_From", "Units_Released","Released_To","Units_Returned","Returned_By")
      expected_classes_char <- c("Product", "Received_From", "Released_To", "Returned_By")
      expected_classes_num <- c("Units_Received",  "Units_Released", "Units_Returned")
      num_rows <- nrow(product_df)
      df_feedback <- data.frame(matrix(ncol = length(expected_colnames), nrow = num_rows, "CORRECT"))
      colnames(df_feedback) <- expected_colnames
      
      
      # Check1: Is any column name not present?
      if(any(colnames(df_feedback) %notin% colnames(product_df))){
        missing_cols <- which(colnames(df_feedback) %notin% colnames(product_df))
        df_feedback[, missing_cols] <- "ISSUE"
      }
      
      #Check2: Are all column classes expected?
      # For character columns
      if(any(sapply(product_df[, expected_classes_char], class) != "character")){
        wrongclass_cols <- which(sapply(product_df[, expected_classes_char], class) != "character")
        df_feedback[,wrongclass_cols] <- "ISSUE"
      }
      if(!any(grepl("POSIX", is(product_df$Entry_Date))) == TRUE){
        df_feedback$Entry_Date <- "ISSUE"
      }
      # For actual numeric columns, if they contain a character value even after trying to trnasform to numeric
      for(j in unique(expected_classes_num)){
        if(any(is.na(as.numeric(na.omit(unlist(product_df[,j]))))==TRUE)){
          row <- which(is.na(as.numeric(na.omit(unlist(product_df[,j])))))
          df_feedback[row,j] <- "ISSUE"
        }
      }
      
      # Check3: OPEN  
      
      
      
      
    }
    #### 2017|09 ####
    
    if (year == "2017" & grepl("Sep", tab)) {
      
      # TODO: Account for this specific case for 2018 + 2017 before 
      ##### For medical supplies distribution (MSD) #####

      # Remove empty rows
      product_df <- product_df %>% filter_all(any_vars(complete.cases(.)))
      
      
      # Change column "Entry Date" to datetime object
      product_df$Entry_Date <- as.numeric(product_df$Entry_Date)
      product_df[["Entry_Date"]] <-
        as.POSIXct(product_df[["Entry_Date"]] * (60 * 60 * 24)
                   , origin = "1899-12-30"
                   , tz = "GMT")
      

      #### Quality check ####
      
      # Define final feedback file which should contain all necessary input columns of this version
      expected_colnames <- c("Product","Entry_Date","Units_Received","Received_From", "Units_Released","Released_To")
      expected_classes_char <- c("Product", "Received_From", "Released_To")
      expected_classes_num <- c("Units_Received",  "Units_Released")
      num_rows <- nrow(product_df)
      df_feedback <- data.frame(matrix(ncol = length(expected_colnames), nrow = num_rows, "CORRECT"))
      colnames(df_feedback) <- expected_colnames
      
      
      # Check1: Is any column name not present?
      if(any(colnames(df_feedback) %notin% colnames(product_df))){
        missing_cols <- which(colnames(df_feedback) %notin% colnames(product_df))
        df_feedback[, missing_cols] <- "ISSUE"
      }
      
      #Check2: Are all column classes expected?
      # For character columns
      if(any(sapply(product_df[, expected_classes_char], class) != "character")){
        wrongclass_cols <- which(sapply(product_df[, expected_classes_char], class) != "character")
        df_feedback[,wrongclass_cols] <- "ISSUE"
      }
      if(!any(grepl("POSIX", is(product_df$Entry_Date))) == TRUE){
        df_feedback$Entry_Date <- "ISSUE"
      }
      # For actual numeric columns, if they contain a character value even after trying to trnasform to numeric
      for(j in unique(expected_classes_num)){
        if(any(is.na(as.numeric(na.omit(unlist(product_df[,j]))))==TRUE)){
          row <- which(is.na(as.numeric(na.omit(unlist(product_df[,j])))))
          df_feedback[row,j] <- "ISSUE"
        }
      }
      
      # Check3: OPEN  
      
      
      
      
      
    }
    if(year %notin% c("2017", "2018", "2019", "2020", "2021")){
      # If the month and year variables are not legit values, yield an error feedback
      print(
        'Error: The script could not be run. Please check if the month and/or year you have provided are between 2017 and 2021.'
      )
    }  
    
    
  #### Start preprocessing ####  
    #### 2021 ####
    if(year == "2021"){
      ##### Define country #####
      
      ### TODO?: Delete whole patient_df$country part? Why do we need this?
      # Extract from Patient ID first part (should be country)
      for(i in 1:nrow(patient_df)){
        patient_df$country[i] <- as.character(strsplit(patient_df$`Patient ID`, "_")[[i]][[1]])
      }
      
      # Get most common value
      patient_df$country <- unlist(patient_df$country) 
      country <- names(sort(table(patient_df$country),decreasing=TRUE)[1])
      
      
      
      #### MSD: Adjust dataframe #####
      #### Add variable Date_DataEntry # CHANGE WAS MADE HERE ####
      
      # Add data that this tab was last updated
      
      for (i in 1:nrow(df_alt1)) {
        if (any(grepl("Date:", df_alt1[i,]))) {
          Date_DataEntry <- df_alt1[i, ]
          date <- as.numeric(Date_DataEntry[grep('[0-9]+',Date_DataEntry)])
          date <- as.Date(date, origin = "1899-12-30")
        }
      }
      
      product_df$Product_Entry_Spreadsheet <- date
      
      
      
      #### Add variables country and clinic_code #####
      product_df$Product_Country <- country_code    
      product_df$Product_Hospital <- clinic_code    
      
      #### Add variable table_month and table_year ####    
      latest_date <- as.character(max(product_df$Entry_Date, na.rm = TRUE))
      year <- as.numeric(strsplit(latest_date, "-")[[1]][1]) ; month <- as.character(strsplit(latest_date, "-")[[1]][2])
      
      product_df$Product_table_month <- month
      product_df$Product_table_year <- year
      
      #### Rename column "Released_To" to "Patient_Name" #####
      product_df$Patient_Name <- product_df$Released_To 
      
      #### Extract Patient Identifier and add to Released_To #####
      patient_df_id <- patient_df %>%
        dplyr::select(`Patient Name`, `Patient ID`)
      product_df <- dplyr::left_join(product_df, patient_df_id, by = c("Released_To" = "Patient Name")) 
      product_df$Released_To <- product_df$`Patient ID`
      #product_df <- product_df %>% select(., -matches("Patient ID"))
      
      #### Rename variable patient ID #####
      product_df <- dplyr::rename(product_df, "Patient_ID" = "Patient ID")
      
      #### Extend Product Name #####
      product_df <- product_df %>% fill(Product)
      
      #### Add additional column Product No #####
      product_df <- transform(product_df,Product_No=as.numeric(factor(Product)))
      
      #### Remove rows without changes in distribution ####
      delete_rows <- as.vector(0)
      for(i in 1:nrow(product_df)){
        
        if((is.na(product_df$Units_Released[i]) == TRUE) & (is.na(product_df$Units_Released[i]) == TRUE) &
           (is.na(product_df$Released_To[i]) == TRUE) & (is.na(product_df$Entry_Date[i]) == TRUE)){
          delete_rows[length(delete_rows)+1] <- i
        }
      }
      
      product_df <- product_df[-delete_rows,] 
      
      #### Add additional column Balance_Status ####
      product_df$Balance_Status <-  "change"
      for(j in unique(product_df$Product)){
        
        # Define start
        product_df[(product_df$Balance ==  max(product_df$Balance[product_df$Product == j])) & (is.na(product_df$Units_Released) == TRUE) & (product_df$Product == j), 'Balance_Status'] <- "start"
        # Define end
        product_df[(product_df$Balance ==  min(product_df$Balance[product_df$Product == j])) & (is.na(product_df$Units_Received) == FALSE) & (product_df$Product == j), 'Balance_Status'] <- "end"
      }
      
      #### Add additional column Units_Released_perproduct ####
      product_df$Product_Units_Released_perproduct <- NA
      for(j in unique(product_df$Product)){
        df_sub <- product_df[product_df$Product == j,]
        units <- df_sub[nrow(df_sub), 'Units_Released']
        product_df[product_df$Product == j, 'Product_Units_Released_perproduct'] <- units
      }
      
      #### Rename column names #####
      # Where "Product" is not in the prefix, add it
      old_col_names <- unique(colnames(product_df))
      index <- 0
      for(j in unique(colnames(product_df))){
        index <- index + 1
        if(!startsWith(j, "Product")){
          colnames(product_df)[index] <- paste0("Product_", j)
        }
      }
      
      #### Change order of columns ####
      product_df <- subset(product_df, select=c(Product, Product_Entry_Date, Product_Balance, Product_Units_Received, Product_Received_From, Product_Units_Released,
                                        Product_Released_To, Product_Patient_Name, Product_Patient_ID, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                        Product_No, Product_Balance_Status, Product_Units_Released_perproduct))
      
      
      #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
      product_df <- product_df %>% 
        mutate(
          across(c(Product_Units_Received, Product_Units_Released, Product_Units_Returned), ~replace_na(.x, 0))
        )
      
    }  
    #### 2020 ####
    if(year == "2020"){
      ##### MSD: Adjust dataframe #####
      #### Add variable Date_DataEntry # CHANGE WAS MADE HERE ####
      
      # Add data that this tab was last updated
      
      for (i in 1:nrow(df_alt1)) {
        if (any(grepl("Date:", df_alt1[i,]))) {
          Date_DataEntry <- df_alt1[i, ]
          date <- as.numeric(Date_DataEntry[grep('[0-9]+',Date_DataEntry)])
          date <- as.Date(date, origin = "1899-12-30")
        }
      }
      
      product_df$Product_Entry_Spreadsheet <- date
      
      
      
      #### Add variables country_code and clinic_code #####
      product_df$Product_Country <- country_code    
      product_df$Product_Hospital <- clinic_code    
      
      #### Add variable table_month and table_year ####    
      latest_date <- as.character(max(product_df$Entry_Date, na.rm = TRUE))
      year <- as.numeric(strsplit(latest_date, "-")[[1]][1]) ; month <- as.character(strsplit(latest_date, "-")[[1]][2])
      
      product_df$table_month <- month
      product_df$table_year <- year
      
      #### Rename column "Released_To" to "Patient_Name" #####
      
      product_df$Patient_Name <- product_df$Released_To 
      
      
      #### Add additional variable Patient Identifier and assign to Released_To #####
      
      # If there is a column with patient ID in patient table
      if(any(colnames(patient_df) %in% "ID")){
        patient_df_id <- patient_df %>%
          dplyr::select(`Patient Name`, `Patient ID`)
        product_df <- dplyr::left_join(product_df, patient_df_id, by = c("Released_To" = "Patient Name"))
        product_df <- dplyr::rename(product_df, "Patient_ID" = "Patient ID")
        product_df$Released_To <- product_df$Patient_ID
        
      } else{
        product_df$Patient_ID <- NA ; product_df$Released_To <- product_df$Patient_ID
      }
      
      
      #### Extend Product Name #####
      product_df <- product_df %>% fill(Product)
      
      #### Extend Entry_Date ####
      product_df <- product_df %>% fill(Entry_Date)
      
      #### Add additional column Product No #####
      product_df <- transform(product_df,Product_No=as.numeric(factor(Product)))
      
      #### Remove rows without changes in distribution ####
      
      delete_rows <- as.vector(NA)
      for(i in 1:nrow(product_df)){
        if((is.na(product_df$Units_Received[i]) == TRUE) & (is.na(product_df$Units_Released[i]) == TRUE)){
          delete_rows[length(delete_rows)+1] <- i
        }
      }
      if(length(delete_rows) > 0 & is.na(delete_rows) == FALSE){ 
        product_df <- product_df[-delete_rows,] 
      }
      
      #### Add additional column Balance_Status ####
      product_df$Balance_Status <- "change"
      for(j in unique(product_df$Product)){
        print(j)
        # Define start
        
        product_df[grepl("START", product_df$Units_Received) & is.na(product_df$Units_Received) == FALSE & product_df$Product == j, 'Balance_Status'] <- "start"
        # product_df[(product_df$Received_From == max(product_df$Received_From[product_df$Product == j], na.rm = TRUE)) & (is.na(product_df$Units_Released) == TRUE) & (product_df$Product == j) & (product_df$Entry_Date == min(product_df$Entry_Date[product_df$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "start"
        
        # Define end
        product_df[(is.na(product_df$Units_Released) == TRUE) & (product_df$Product == j) & (product_df$Entry_Date == max(product_df$Entry_Date[product_df$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "end"
      }
      
      #### Add additional column Units_Released_perproduct ####
      product_df$Product_Units_Released_perproduct <- NA
      for(j in unique(product_df$Product)){
        df_sub <- product_df[product_df$Product == j,]
        units <- sum(as.numeric(df_sub$Units_Released), na.rm = TRUE)
        product_df[product_df$Product == j, 'Product_Units_Released_perproduct'] <- units
      }
      
      
      
      #### Add additional column Product_Balance ####
      
      for(i in 1:nrow(product_df)){
        for(j in unique(product_df$Product)){
          
          # 
          if(product_df$Balance_Status[i] == "start"){
            product_df[i, 'Product_Balance'] <- product_df[i, 'Received_From'] 
            start_value <- product_df[i, 'Received from']
          }
          if(product_df$Balance_Status[i] == "change"){
            
            release <- NA
            reception <- NA
            if(is.na(as.numeric(product_df[i, 'Units_Released'])) == TRUE){
              release <- as.numeric(0)
            } else{
              release <- as.numeric(product_df[i, 'Units_Released'])
            }
            if(is.na(as.numeric(product_df[i, 'Units_Received'])) == TRUE){
              reception <- as.numeric(0)
            } else{
              reception <- as.numeric(product_df[i, 'Units_Received'])
            }
            product_df[i, 'Product_Balance'] <- as.numeric(product_df[i-1, 'Product_Balance']) -  release  + reception
          }
          
          if(product_df$Balance_Status[i] == "end"){
            product_df[i, 'Product_Balance'] <- product_df[i, 'Received_From'] 
          }
          
        }
      }
      
      
      #### Recode column values: Units_Received and Received_From #####
      
      # 1.  Units_Received should contain numbers, fill with 0s where characters are the moment
      # 2.  Received_From should be name, not numeric. Fill with empty as character where numbers are.   
      
      for(i in 1:nrow(product_df)){
        if(is.na(product_df$Units_Received[i]) == FALSE & is.character(product_df$Units_Received[i]) == TRUE){
          product_df$Units_Received[i] <- as.numeric(0)
        }
      }
      
      
      for(i in 1:nrow(product_df)){
        if(is.na(product_df$Received_From[i]) == FALSE & grepl("[[:digit:]]", product_df$Received_From[i]) == TRUE){
          product_df$Received_From[i] <- as.character(NA)
        }
      }
      
      
      #### Rename column values, add prefix ####
      # Where "Product" is not in the prefix, add it
      old_col_names <- unique(colnames(product_df))
      index <- 0
      for(j in unique(colnames(product_df))){
        index <- index + 1
        if(!startsWith(j, "Product")){
          colnames(product_df)[index] <- paste0("Product_", j)
        }
      }
      
      
      
      #### Change order of columns to match v2021 version ####
      product_df <- subset(product_df, select=c(Product, Product_Entry_Date, Product_Balance, Product_Units_Received, Product_Received_From, Product_Units_Released,
                                        Product_Released_To, Product_Patient_Name, Product_Patient_ID, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                        Product_No, Product_Balance_Status, Product_Units_Released_perproduct))  
      
      #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
      product_df <- product_df %>% 
        mutate(
          across(c(Product_Units_Received, Product_Units_Released, Product_Units_Returned), ~replace_na(.x, 0))
        )
      
      
    }  
    #### 2019 ####
    if(year == "2019"){
      ##### MSD: Adjust dataframe #####
      #### Add variable Date_DataEntry # CHANGE WAS MADE HERE ####
      
      # Add data that this tab was last updated
      
      for (i in 1:nrow(df_alt1)) {
        if (any(grepl("Date:", df_alt1[i,]))) {
          Date_DataEntry <- df_alt1[i, ]
          date <- as.numeric(Date_DataEntry[grep('[0-9]+',Date_DataEntry)])
          date <- as.Date(date, origin = "1899-12-30")
        }
      }
      
      product_df$Product_Entry_Spreadsheet <- date
      
      
      
      #### Add variables country_code and clinic_code #####
      product_df$Product_Country <- country_code    
      product_df$Product_Hospital <- clinic_code    
      
      #### Add variable table_month and table_year ####    
      latest_date <- as.character(max(product_df$Entry_Date, na.rm = TRUE))
      year <- as.numeric(strsplit(latest_date, "-")[[1]][1]) ; month <- as.character(strsplit(latest_date, "-")[[1]][2])
      
      product_df$table_month <- month
      product_df$table_year <- year
      
      #### Rename column "Released_To" to "Patient_Name" #####
      
      product_df$Patient_Name <- product_df$Released_To 
      
      
      #### Add additional variable Patient Identifier and assign to Released_To #####
      
      # If there is a column with patient ID in patient table
      if(any(colnames(patient_df) %in% "ID")){
        patient_df_id <- patient_df %>%
          dplyr::select(`Patient Name`, `Patient ID`)
        product_df <- dplyr::left_join(product_df, patient_df_id, by = c("Released_To" = "Patient Name"))
        product_df <- dplyr::rename(product_df, "Patient_ID" = "Patient ID")
        product_df$Released_To <- product_df$Patient_ID
        
      } else{
        product_df$Patient_ID <- NA ;  product_df$Released_To <- product_df$Patient_ID
      }
      
      
      #### Extend Product Name #####
      product_df <- product_df %>% fill(Product)
      
      #### Extend Entry_Date ####
      product_df <- product_df %>% fill(Entry_Date)
      
      #### Add additional column Product No #####
      product_df <- transform(product_df,Product_No=as.numeric(factor(Product)))
      
      #### Remove rows without changes in distribution ####
      
      delete_rows <- as.vector(NA)
      for(i in 1:nrow(product_df)){
        if((is.na(product_df$Units_Received[i]) == TRUE) & (is.na(product_df$Units_Released[i]) == TRUE)){
          delete_rows[length(delete_rows)+1] <- i
        }
      }
      if(length(delete_rows) > 0 & is.na(delete_rows) == FALSE){ 
        product_df <- product_df[-delete_rows,] 
      }
      
      #### Add additional column Balance_Status ####
      product_df$Balance_Status <- "change"
      for(j in unique(product_df$Product)){
        print(j)
        # Define start
        
        product_df[grepl("START", product_df$Units_Received) & is.na(product_df$Units_Received) == FALSE & product_df$Product == j, 'Balance_Status'] <- "start"
        # product_df[(product_df$Received_From == max(product_df$Received_From[product_df$Product == j], na.rm = TRUE)) & (is.na(product_df$Units_Released) == TRUE) & (product_df$Product == j) & (product_df$Entry_Date == min(product_df$Entry_Date[product_df$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "start"
        
        # Define end
        product_df[(is.na(product_df$Units_Released) == TRUE) & (product_df$Product == j) & (product_df$Entry_Date == max(product_df$Entry_Date[product_df$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "end"
      }
      
      #### Add additional column Units_Released_perproduct ####
      product_df$Product_Units_Released_perproduct <- NA
      for(j in unique(product_df$Product)){
        df_sub <- product_df[product_df$Product == j,]
        units <- sum(as.numeric(df_sub$Units_Released), na.rm = TRUE)
        product_df[product_df$Product == j, 'Product_Units_Released_perproduct'] <- units
      }
      
      
      
      #### Add additional column Product_Balance ####
      product_df$Product_Balance <- NA ; 
      
      for(i in 1:nrow(product_df)){
        for(j in unique(product_df$Product)){
          
          # 
          if(product_df$Balance_Status[i] == "start"){
            product_df[i, 'Product_Balance'] <- product_df[i, 'Received_From'] 
            start_value <- product_df[i, 'Received from']
          }
          if(product_df$Balance_Status[i] == "change"){
            
            release <- NA
            reception <- NA
            if(is.na(as.numeric(product_df[i, 'Units_Released'])) == TRUE){
              release <- as.numeric(0)
            } else{
              release <- as.numeric(product_df[i, 'Units_Released'])
            }
            if(is.na(as.numeric(product_df[i, 'Units_Received'])) == TRUE){
              reception <- as.numeric(0)
            } else{
              reception <- as.numeric(product_df[i, 'Units_Received'])
            }
            product_df[i, 'Product_Balance'] <- as.numeric(product_df[i-1, 'Product_Balance']) -  release  + reception
          }
          
          if(product_df$Balance_Status[i] == "end"){
            product_df[i, 'Product_Balance'] <- product_df[i, 'Received_From'] 
          }
          
        }
      }
      
      
      #### Recode column values: Units_Received and Received_From #####
      
      # 1.  Units_Received should contain numbers, fill with 0s where characters are the moment
      # 2.  Received_From should be name, not numeric. Fill with empty as character where numbers are.   
      
      for(i in 1:nrow(product_df)){
        if(is.na(product_df$Units_Received[i]) == FALSE & is.character(product_df$Units_Received[i]) == TRUE){
          product_df$Units_Received[i] <- as.numeric(0)
        }
      }
      
      
      for(i in 1:nrow(product_df)){
        if(is.na(product_df$Received_From[i]) == FALSE & grepl("[[:digit:]]", product_df$Received_From[i]) == TRUE){
          product_df$Received_From[i] <- as.character(NA)
        }
      }
      
      
      #### Rename column values, add prefix ####
      # Where "Product" is not in the prefix, add it
      old_col_names <- unique(colnames(product_df))
      index <- 0
      for(j in unique(colnames(product_df))){
        index <- index + 1
        if(!startsWith(j, "Product")){
          colnames(product_df)[index] <- paste0("Product_", j)
        }
      }
      
      
      
      #### Change order of columns to match v2021 version ####
      product_df <- subset(product_df, select=c(Product, Product_Entry_Date, Product_Balance, Product_Units_Received, Product_Received_From, Product_Units_Released,
                                        Product_Released_To, Product_Patient_Name, Product_Patient_ID, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                        Product_No, Product_Balance_Status, Product_Units_Released_perproduct))
      
      #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
      product_df <- product_df %>% 
        mutate(
          across(c(Product_Units_Received, Product_Units_Released, Product_Units_Returned), ~replace_na(.x, 0))
        )
      
      
    }  
    #### 2018 CHANGE WAS MADE HERE ####
    if(year == "2018"){
      ##### MSD: Adjust dataframe #####
      #### Add variable Date_DataEntry # CHANGE WAS MADE HERE ####
      
      # Add data that this tab was last updated
      
      for (i in 1:nrow(df_alt1)) {
        if (any(grepl("Date:", df_alt1[i,]))) {
          Date_DataEntry <- df_alt1[i, ]
          date <- as.numeric(Date_DataEntry[grep('[0-9]+',Date_DataEntry)])
          date <- as.Date(date, origin = "1899-12-30")
        }
      }
      
      product_df$Product_Entry_Spreadsheet <- date
      
      
      #### Add variables country_code and clinic_code #####
      product_df$Product_Country <- country_code    
      product_df$Product_Hospital <- clinic_code    
      
      #### Add variable table_month and table_year ####    
      latest_date <- as.character(max(product_df$Entry_Date, na.rm = TRUE))
      year <- as.numeric(strsplit(latest_date, "-")[[1]][1]) ; month <- as.character(strsplit(latest_date, "-")[[1]][2])
      
      product_df$table_month <- month
      product_df$table_year <- year
      
      #### Rename column "Released_To" to "Patient_Name" #####
      
      product_df$Patient_Name <- product_df$Released_To 
      
      
      #### Add additional variable Patient Identifier and assign to "Released_To" #####
      
      # If there is a column with patient ID in patient table
      if(any(colnames(patient_df) %in% "ID")){
        patient_df_id <- patient_df %>%
          dplyr::select(`Patient Name`, `Patient ID`)
        product_df <- dplyr::left_join(product_df, patient_df_id, by = c("Released_To" = "Patient Name"))
        product_df <- dplyr::rename(product_df, "Patient_ID" = "Patient ID")
        product_df$Released_To <- product_df$Patient_ID
      } else{
        product_df$Patient_ID <- NA ;  product_df$Released_To <- product_df$Patient_ID
      }
      
      
      #### Extend Product Name #####
      product_df <- product_df %>% fill(Product)
      
      #### Extend Entry_Date ####
      product_df <- product_df %>% fill(Entry_Date)
      
        # THIS WAS ADDED IN V1.3
      # If empty date but entry was made (e.g., due to note of stock), use date of next entry row (e.g., Apr18, row 1 needs date)
      if(any(is.na(product_df[is.na(product_df$Product) == FALSE, 'Entry_Date']) == TRUE)){
        product_df <- product_df %>% fill(Entry_Date, .direction = "up")
      }
      
      #### Add additional column Product No #####
      product_df <- transform(product_df,Product_No=as.numeric(factor(Product)))
      
      #### Remove rows without changes in distribution ####
      
      delete_rows <- as.vector(NA)
      for(i in 1:nrow(product_df)){
        if((is.na(product_df$Units_Received[i]) == TRUE) & (is.na(product_df$Units_Released[i]) == TRUE)){
          delete_rows[length(delete_rows)+1] <- i
        }
      }
      if(length(delete_rows) > 0 & any(is.na(delete_rows) == FALSE)){ 
        product_df <- product_df[-as.numeric(na.omit(delete_rows)),]  # CHANGE WAS MADE HERE; THIS DID NOT EXCLUDE NAs BEFORE
        
      }
      
      #### Add additional column Balance_Status ####
      product_df$Balance_Status <- "change"
      for(j in unique(product_df$Product)){
        print(j)
        # Define start # CHANGE WAS MADE HERE; in v1.3, the start may also be the first line
        product_df[grepl("START", product_df$Units_Received) & is.na(product_df$Units_Received) == FALSE & product_df$Product == j, 'Balance_Status'] <- "start"
        
        # CHANGE WAS MADE HERE; in v1.3, the start may also be the first line if there is no "START" argument before
        if(!any(product_df[grepl("start", product_df$Balance_Status[product_df$Product == j])])){
          product_df[(product_df$Product == j) & product_df$Entry_Date == min(product_df$Entry_Date[product_df$Product == j]), 'Balance_Status'] <- "start"
          
        }
        
        # product_df[(product_df$Received_From == max(product_df$Received_From[product_df$Product == j], na.rm = TRUE)) & (is.na(product_df$Units_Released) == TRUE) & (product_df$Product == j) & (product_df$Entry_Date == min(product_df$Entry_Date[product_df$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "start"
        
        # Define end
        product_df[(is.na(product_df$Units_Released) == TRUE) & (product_df$Product == j) & (product_df$Entry_Date == max(product_df$Entry_Date[product_df$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "end"
      }
      
      #### Add additional column Units_Released_perproduct ####
      product_df$Product_Units_Released_perproduct <- NA
      for(j in unique(product_df$Product)){
        df_sub <- product_df[product_df$Product == j,]
        units <- sum(as.numeric(df_sub$Units_Released), na.rm = TRUE)
        product_df[product_df$Product == j, 'Product_Units_Released_perproduct'] <- units
      }
      
      
      
      #### Add additional column Product_Balance ####
      
      product_df$Product_Balance <- NA ; 
      
      for(i in 1:nrow(product_df)){
        for(j in unique(product_df$Product)){
          
          # 
          if(product_df$Balance_Status[i] == "start"){
            product_df[i, 'Product_Balance'] <- product_df[i, 'Received_From'] 
            start_value <- product_df[i, 'Received from']
          }
          if(product_df$Balance_Status[i] == "change"){
            
            release <- NA
            reception <- NA
            if(is.na(as.numeric(product_df[i, 'Units_Released'])) == TRUE){
              release <- as.numeric(0)
            } else{
              release <- as.numeric(product_df[i, 'Units_Released'])
            }
            if(is.na(as.numeric(product_df[i, 'Units_Received'])) == TRUE){
              reception <- as.numeric(0)
            } else{
              reception <- as.numeric(product_df[i, 'Units_Received'])
            }
            
            # CHANGE WAS MADE HERE:
            # If start with "change" and no prior information is available, subtract reception from release
            if(i == 1){
              product_df[i, 'Product_Balance'] <- reception - release
            } else{
              product_df[i, 'Product_Balance'] <- as.numeric(product_df[i-1, 'Product_Balance']) -  release  + reception
            }
          }
          
          if(product_df$Balance_Status[i] == "end"){
            product_df[i, 'Product_Balance'] <- product_df[i, 'Received_From'] 
          }
          
        }
      }
      
      
      #### Recode column values: Units_Received and Received_From (incl. Check10) #####
      
      # 1.  Units_Received should contain numbers, fill with 0s where characters are the moment
      # 2.  Received_From should be name, not numeric. Fill with empty as character where numbers are.   
      check10 <- matrix(NA, nrow = 1, ncol = 2)  
      check10[,1] <- "Quality Check 10: Is there any unexpected number in Column Received From?"
      
      product_df$Units_Received2 <- NA
      for(i in 1:nrow(product_df)){
        if(is.na(product_df$Units_Received[i]) == FALSE & is.character(product_df$Units_Received[i]) == TRUE){
          product_df[i, 'Units_Received2'] <- as.numeric(0)
        }
      }
      
      
      for(i in 1:nrow(product_df)){
        # If a start or end balance is met
        if(is.na(product_df$Received_From[i]) == FALSE & grepl("[[:digit:]]", product_df$Received_From[i]) == TRUE & any(stringi::stri_detect_fixed(na.omit(product_df$Units_Received[i]),c("START", "END"))) == TRUE){
          product_df$Received_From[i] <- as.character(NA)
        }
        # If there is a number in "received_from" without being a start or end balance, yield error
        if(is.na(product_df$Received_From[i]) == FALSE & grepl("[[:digit:]]", product_df$Received_From[i]) == TRUE & any(stringi::stri_detect_fixed(na.omit(product_df$Units_Received[i]),c("START", "END"))) == FALSE){
          check10[,2] <- "Error! Please check in column Received From if there is number which does not represent Start or End balance and should actually be assigned to column Units Received or Units Released."
        }
      }
      if(is.na(check10[,2]) == TRUE){
        check10[,2] <- "Check. No inconsistencies."
      }
      
      product_df <- product_df %>% dplyr::select(., -ends_with("Units_Received")) %>% dplyr::rename(., Units_Received = Units_Received2)
      
      
      #### Rename column values, add prefix ####
      # Where "Product" is not in the prefix, add it
      old_col_names <- unique(colnames(product_df))
      index <- 0
      for(j in unique(colnames(product_df))){
        index <- index + 1
        if(!startsWith(j, "Product")){
          colnames(product_df)[index] <- paste0("Product_", j)
        }
      }
      
      
      
      #### Change order of columns to match v2021 version ####
      product_df <- subset(product_df, select=c(Product, Product_Entry_Spreadsheet, Product_Entry_Date, Product_Balance, Product_Units_Received, Product_Received_From, Product_Units_Released,
                                        Product_Released_To, Product_Patient_Name, Product_Patient_ID, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                        Product_No, Product_Balance_Status, Product_Units_Released_perproduct))
      
      #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
      product_df <- product_df %>% 
        tidyr::replace_na(list(Product_Units_Received = 0, Product_Units_Released = 0, Product_Units_Returned = 0))
      
      
      
      
    } 
    #### 2017|not09 CHANGE MADE HERE ####
    if(year == "2017" & !grepl("Sep", tab)){
      ##### MSD: Adjust dataframe #####
      #### Add variable Date_DataEntry # CHANGE WAS MADE HERE ####
      
      # Add data that this tab was last updated
      
      for (i in 1:nrow(df_alt1)) {
        if (any(grepl("Date:", df_alt1[i,]))) {
          Date_DataEntry <- df_alt1[i, ]
          date <- as.numeric(Date_DataEntry[grep('[0-9]+',Date_DataEntry)])
          date <- as.Date(date, origin = "1899-12-30")
        }
      }
      
      product_df$Product_Entry_Spreadsheet <- date
      
      #### Add variables country_code and clinic_code #####
      product_df$Product_Country <- country_code    
      product_df$Product_Hospital <- clinic_code    
      
      #### Add variable table_month and table_year ####    
      latest_date <- as.character(max(product_df$Entry_Date, na.rm = TRUE))
      year <- as.numeric(strsplit(latest_date, "-")[[1]][1]) ; month <- as.character(strsplit(latest_date, "-")[[1]][2])
      
      product_df$table_month <- month
      product_df$table_year <- year
      
      #### Rename column "Released_To" to "Patient_Name" #####
      
      product_df$Patient_Name <- product_df$Released_To 
      
      
      #### Add additional variable Patient Identifier and assign to "Released_To" #####
      
      # If there is a column with patient ID in patient table
      if(any(colnames(patient_df) %in% "ID")){
        patient_df_id <- patient_df %>%
          dplyr::select(`Patient Name`, `Patient ID`)
        product_df <- dplyr::left_join(product_df, patient_df_id, by = c("Released_To" = "Patient Name"))
        product_df <- dplyr::rename(product_df, "Patient_ID" = "Patient ID")
        product_df$Released_To <- product_df$Patient_ID
      } else{
        product_df$Patient_ID <- NA ;  product_df$Released_To <- product_df$Patient_ID
      }
      
      
      #### Extend Product Name #####
      product_df <- product_df %>% fill(Product)
      
      #### Extend Entry_Date ####
      product_df <- product_df %>% fill(Entry_Date)
      
      #### Add additional column Product No #####
      product_df <- transform(product_df,Product_No=as.numeric(factor(Product)))
      
      #### Remove rows without changes in distribution ####
      
      delete_rows <- as.vector(NA)
      for(i in 1:nrow(product_df)){
        if((is.na(product_df$Units_Received[i]) == TRUE) & (is.na(product_df$Units_Released[i]) == TRUE)){
          delete_rows[length(delete_rows)+1] <- i
        }
      }
      if(length(delete_rows) > 0 & is.na(delete_rows) == FALSE){ 
        product_df <- product_df[-delete_rows,] 
      }
      
      #### Add additional column Balance_Status ####
      product_df$Balance_Status <- "change"
      for(j in unique(product_df$Product)){
        print(j)
        # Define start
        #(product_df$Received_From == max(product_df$Received_From[product_df$Product == j], na.rm = TRUE)) & (is.na(product_df$Units_Released) == TRUE) & (product_df$Product == j) & 
        product_df[(product_df$Product == j) & (product_df$Entry_Date == min(product_df$Entry_Date[product_df$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "start"
        # Define end
        product_df[(is.na(product_df$Units_Released) == TRUE) & (product_df$Product == j) & (product_df$Entry_Date == max(product_df$Entry_Date[product_df$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "end"
      }
      
      #### Add additional column Units_Released_perproduct ####
      product_df$Product_Units_Released_perproduct <- NA
      for(j in unique(product_df$Product)){
        df_sub <- product_df[product_df$Product == j,]
        units <- sum(as.numeric(df_sub$Units_Released), na.rm = TRUE)
        product_df[product_df$Product == j, 'Product_Units_Released_perproduct'] <- units
      }
      
      
      
      #### Add additional column Product_Balance and replace Units_columns with 0 where NA ####
      
      # Initiate
      product_df$Product_Balance <- NA
      
      # Compute Balance based on received vs released # CHANGE WAS MADED HERE, THIS SECTION WAS ADDED
      
      # Fill empty Units_Received rows with 0 for Balance computation
      product_df <- product_df %>% mutate_at(vars(
        matches(c("Units_Received", "Units_Released", "Units_Returned"))), ~replace(., is.na(.), 0)) %>%
        mutate(
          Units_Received = as.numeric(gsub("`", "", Units_Received)),
          Units_Released = as.numeric(gsub("`", "", Units_Released)),
          Units_Returned = as.numeric(gsub("`", "", Units_Returned))
        )
      
      # Loop through each product
      for(i in 1:nrow(product_df)){
        for(j in unique(product_df$Product)){
          start_value <- as.numeric(head(product_df$Units_Received[product_df$Product == j], n = 1))
          
          if(as.numeric(product_df[i, 'Units_Received']) == start_value){ 
            product_df[i, 'Product_Balance'] <- start_value
            new_value <- start_value
          } else{
            product_df[i, 'Product_Balance'] <- new_value + na.omit(as.numeric(product_df[i, 'Units_Received'])) -  na.omit(as.numeric(product_df[i, 'Units_Released']))
          }}}
      
      
      #### Recode column values: Units_Received and Received_From (incl. Check10) #####
      
      # 1.  Units_Received should contain numbers, fill with 0s where characters are the moment
      # 2.  Received_From should be name, not numeric. Fill with empty as character where numbers are.   
      check10 <- matrix(NA, nrow = 1, ncol = 2)  
      check10[,1] <- "Quality Check 10: Is there any unexpected number in Column Received From?"
      
      for(i in 1:nrow(product_df)){
        if(is.na(product_df$Units_Received[i]) == FALSE & is.character(product_df$Units_Received[i]) == TRUE){
          product_df$Units_Received2[i] <- as.numeric(0)
        }
        if(is.na(product_df$Units_Received[i]) == TRUE){
          product_df$Units_Received2[i] <- as.numeric(0)
        }
        else{
          product_df$Units_Received2[i] <- as.numeric(product_df$Units_Received[i])
        }
      }
      
      for(i in 1:nrow(product_df)){
        if(is.na(product_df$Received_From[i]) == FALSE & grepl("[[:digit:]]", product_df$Received_From[i]) == TRUE){
          check10[,2] <- "Error! Please check in column Received From if there is number which should actually be assigned to column Units Received or Units Released."
        }
        # If there is change-indicating input for Received_From but not for Units_Received
        if((is.na(product_df$Received_From[i]) == FALSE & product_df$Received_From[i] != "NA") & ((as.numeric(product_df[i, 'Units_Received']) == 0 ) | is.na(product_df[i, 'Units_Received']) == TRUE)){ # CHANGES WERE MADE WITH strsplit to account for wrongfully entered numbers
          check10[,2] <- "Error! Please check columns Units Received and Received From for inconsistencies."
        }
        # If there is no input for Received_From but change-indicating for Units_Received
        if((is.na(product_df$Received_From[i]) == TRUE | product_df$Received_From[i] == "NA") & ((as.numeric(product_df[i, 'Units_Received']) > 0) & is.na(product_df[i, 'Units_Received']) == FALSE)){ # CHANGES WERE MADE WITH strsplit to account for wrongfully entered numbers
          check10[,2] <- "Error! Please check columns Units Received and Received From for inconsistencies."
        }
      }
      
      
      
      if(is.na(check10[,2]) == TRUE){
        check10[,2] <- "Check. No inconsistencies."
      }
      
      product_df <- product_df %>% dplyr::select(., -ends_with("Units_Received")) %>% dplyr::rename(., Units_Received = Units_Received2)
      
      
      #### Rename column values, add prefix ####
      # Where "Product" is not in the prefix, add it
      old_col_names <- unique(colnames(product_df))
      index <- 0
      for(j in unique(colnames(product_df))){
        index <- index + 1
        if(!startsWith(j, "Product")){
          colnames(product_df)[index] <- paste0("Product_", j)
        }
      }
      
      
      
      #### Change order of columns to match v2021 version ####
      product_df <- subset(product_df, select=c(Product, Product_Entry_Spreadsheet, Product_Entry_Date, Product_Balance, Product_Units_Received, Product_Received_From, Product_Units_Released,
                                        Product_Released_To, Product_Patient_Name, Product_Patient_ID, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                        Product_No, Product_Balance_Status, Product_Units_Released_perproduct, Product_Units_Extra_KeptinClinic)) # CHANGE WAS MADED HERE; The column name "Product_Units_Extra_KeptinClinic" was added
      
      #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
      product_df <- product_df %>% 
        mutate(
          across(c(Product_Units_Received, Product_Units_Released, Product_Units_Returned), ~replace_na(.x, 0))
        )
      
      
    }
    #### 2017|09 ####
    
    if(year == "2017" & grepl("Sep", tab)){ # TO DO: CONTINUE HERE
      ##### MSD: Adjust dataframe #####
      #### Add variable Date_DataEntry # CHANGE WAS MADE HERE ####
      
      # Add data that this tab was last updated
      
      for (i in 1:nrow(df_alt1)) {
        if (any(grepl("Date:", df_alt1[i,]))) {
          Date_DataEntry <- df_alt1[i, ]
          date <- as.numeric(Date_DataEntry[grep('[0-9]+',Date_DataEntry)])
          date <- as.Date(date, origin = "1899-12-30")
        }
      }
      
      product_df$Product_Entry_Spreadsheet <- date
      
      
      #### Add variables country_code and clinic_code #####
      product_df$Product_Country <- country_code    
      product_df$Product_Hospital <- clinic_code    
      
      #### Add variable table_month and table_year ####    
      latest_date <- as.character(max(product_df$Entry_Date, na.rm = TRUE))
      year <- as.numeric(strsplit(latest_date, "-")[[1]][1]) ; month <- as.character(strsplit(latest_date, "-")[[1]][2])
      
      product_df$table_month <- month
      product_df$table_year <- year
      
      #### Add variable "Patient_Name" and "Product_Patient_ID" # CHANGE WAS MADE HERE, THIS WAS ADDED #####
      
      product_df$Product_Patient_Name <- NA
      product_df$Product_Patient_Name <- as.character(product_df$Released_To)
      product_df$Product_Patient_ID <- product_df$Product_Patient_Name 
      
      # column_patname <- select(product_df, matches("patient"))
      # column_patname <- colnames(product_df[,grepl("patient", colnames(product_df))])
      # 
      # if(length(column_patname) > 0){ # Keep first column which contains "patient" values
      #   product_df$Patient_Name <- product_df[, column_patname[1]]
      # }
      
      
      #### Extend Product Name #####
      product_df <- product_df %>% fill(Product)
      
      #### Extend Entry_Date ####
      product_df <- product_df %>% fill(Entry_Date)
      
      #### Add additional column Product No #####
      product_df <- transform(product_df,Product_No=as.numeric(factor(Product)))
      
      #### Remove rows without changes in distribution ####
      
      delete_rows <- as.vector(NA)
      for(i in 1:nrow(product_df)){
        if((is.na(product_df$Units_Received[i]) == TRUE) & (is.na(product_df$Units_Released[i]) == TRUE)){
          delete_rows[length(delete_rows)+1] <- i
        }
      }
      if(length(delete_rows) > 0 & is.na(delete_rows) == FALSE){ 
        product_df <- product_df[-delete_rows,] 
      }
      
      #### Add additional column Balance_Status ####
      product_df$Balance_Status <- "change"
      for(j in unique(product_df$Product)){
        print(j)
        # Define start
        product_df[(product_df$Units_Received == max(product_df$Units_Received[product_df$Product == j], na.rm = TRUE)) & (is.na(product_df$Released_To) == TRUE | product_df$Released_To == "NA") & (product_df$Product == j) & (product_df$Entry_Date == min(product_df$Entry_Date[product_df$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "start"
        # Define end
        product_df[ (product_df$Product == j) & (product_df$Released_To == tail(product_df[product_df$Product == j, 'Released_To'], n = 1)), 'Balance_Status'] <- "end"
      }
      
      
      #### Add additional column Units_Released_perproduct ####
      product_df$Product_Units_Released_perproduct <- NA
      for(j in unique(product_df$Product)){
        df_sub <- product_df[product_df$Product == j,]
        units <- sum(as.numeric(df_sub$Units_Released), na.rm = TRUE)
        product_df[product_df$Product == j, 'Product_Units_Released_perproduct'] <- units
      }
      
      
      
      #### Add additional column Product_Balance, which equals Product_Balance ####
      check9 <- matrix(NA, nrow = 1, ncol = 2) ; check9[,1] <- "Quality Check 9: Is there any accidental overlap of a START balance event and a CHANGE balance event?"
      check9[,2] <- "Note. No Balance data is available for this spreadsheet version."
      
      product_df$Product_Balance <- product_df$Units_Extra_KeptinClinic
      
      # for(i in 1:nrow(product_df)){
      #   for(j in unique(product_df$Product)){
      #     start_value <- as.numeric(head(product_df$Units_Received[product_df$Product == j], n = 1))
      #     
      #     if(as.numeric(product_df[i, 'Units_Received']) == start_value){ # CHANGE WAS MADE HERE; TYPE OF INPUT WAS CHANGED TO as.numeric()
      #       product_df[i, 'Product_Balance'] <- start_value 
      #       new_value <- start_value
      #     } else{
      #       product_df[i, 'Product_Balance'] <- new_value + na.omit(as.numeric(product_df[i, 'Units_Received'])) -  na.omit(as.numeric(product_df[i, 'Units_Released']))
      #     }
      #     
      
      
      
      #### Recode column values: Units_Received and Received_From (incl. Check10) #####
      
      # 1.  Units_Received should contain numbers, fill with 0s where characters are the moment
      # 2.  Received_From should be name, not numeric. Fill with empty as character where numbers are.   
      check10 <- matrix(NA, nrow = 1, ncol = 2)  
      check10[,1] <- "Quality Check 10: Is there any unexpected number in Column Received From?"
      product_df$Units_Received2 <- NA
      
      for(i in 1:nrow(product_df)){
        if(is.na(product_df$Units_Received[i]) == FALSE & (grepl("[[:lower:]]", product_df$Units_Received[i]) == TRUE | grepl("[[:upper:]]", product_df$Units_Received[i]) == TRUE)){
          product_df$Units_Received2[i] <- as.numeric(0)
        }
        if(is.na(product_df$Units_Received[i]) == TRUE){
          product_df$Units_Received2[i] <- as.numeric(0)
        } else{
          product_df$Units_Received2[i] <- as.numeric(product_df$Units_Received[i])
        }
      }
      
      for(i in 1:nrow(product_df)){
        if(is.na(product_df$Received_From[i]) == FALSE & grepl("[[:digit:]]", product_df$Received_From[i]) == TRUE & grepl("[[:upper:]]", product_df$Received_From[i]) == FALSE){
          check10[,2] <- "Error! Please check in column Received From if there is number which should actually be assigned to column Units Received or Units Released."
          print(i)
        }
        # If there is change-indicating input for Received_From but not for Units_Received
        if((is.na(product_df$Received_From[i]) == FALSE & product_df$Received_From[i] != "NA") & (as.numeric(product_df[i, 'Units_Received']) == 0 | is.na(product_df[i, 'Units_Received']) == TRUE)){
          check10[,2] <- "Error! Please check columns Units Received and Received From for inconsistencies."
        }
        # If there is no input for Received_From but change-indicating for Units_Received
        if((is.na(product_df$Received_From[i]) == TRUE | product_df$Received_From[i] == "NA") & (as.numeric(product_df[i, 'Units_Received']) > 0 & is.na(product_df[i, 'Units_Received']) == FALSE)){
          check10[,2] <- "Error! Please check columns Units Received and Received From for inconsistencies."
        }
      }
      
      
      if(is.na(check10[,2]) == TRUE){
        check10[,2] <- "Check. No inconsistencies."
      }
      
      product_df <- product_df %>% dplyr::select(., -ends_with("Units_Received")) %>% dplyr::rename(., Units_Received = Units_Received2)
      
      #### Add missing columns (empty) ####
      product_df$Returned_By <- NA
      
      #### Rename column values, add prefix ####
      # Where "Product" is not in the prefix, add it
      old_col_names <- unique(colnames(product_df))
      index <- 0
      for(j in unique(colnames(product_df))){
        index <- index + 1
        if(!startsWith(j, "Product")){
          colnames(product_df)[index] <- paste0("Product_", j)
        }
      }
      
      
      #### Change order of columns to match v2021 version ####
      product_df <- subset(product_df, select=c(Product, Product_Entry_Date, Product_Balance, Product_Units_Received, Product_Received_From, Product_Units_Released,
                                        Product_Released_To, Product_Patient_ID, Product_Patient_Name, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                        Product_No, Product_Balance_Status, Product_Units_Released_perproduct, Product_Units_Extra_KeptinClinic, Product_Entry_Spreadsheet)) # CHANGE WAS MADED HERE; The column name "Product_Units_Extra_KeptinClinic" was added
      
      #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
      product_df <- product_df %>% 
        mutate(
          across(c(Product_Units_Received, Product_Units_Released, Product_Units_Returned), ~replace_na(.x, 0))
        )
      
      
    } 
    if(year %notin% c("2017", "2018", "2019", "2020", "2021")){ # If the month and year variables are not legit values, yield an error feedback
      print('Error: The script could not be run. Please check if the month and/or year you have provided are between 2017 and 2021.')
    }  
    
    #### MSD: Save data and quality check feedback  ####      
    
    version_year <- NA
    if(year == "2021"){
      version_year <- "v2021"
    }
    if(year == "2020"){
      version_year <- "v2020"
    }
    if(year == "2019"){
      version_year <- "v2019"
    }
    if(year == "2018"){
      version_year <- "v2018"
    }
    if(year == "2017" & month != "09"){
      version_year <- "v2017"
    }
    if(year == "2017" & month == "09"){
      version_year <- "v072017"
    }
    
    
    # Save quality check feedback file
    
    # Save Feedback data
    feedback_file_name <- paste0(path_output, version_year, '_', country_code, '_', clinic_code, '_',
                                 unique(product_df$Product_table_year), '_QualityCheck.xlsx')
    addWorksheet(wb, tab)
    writeData(wb, tab, df_feedback )
    
    # Save preprocessed data file to final datafile
    product_df$Product_sheet_name <- tab
    #df_final <- rbind(df_final, product_df)
    
    # TODO: Replace list_excel_tabs here with month_list?
    if(tab == list_excel_tabs[1]){
      # df_final <- full_join(df_final, product_df)
      df_final <- product_df
    } else{
      df_final <- rbind(df_final, product_df)
    }
    
    
  }
  
  # Save final feedback
  saveWorkbook(wb, paste0(path_output, unique(df_final$Product_table_year), '_', country_code, '_', clinic_code, '_QualityCheck.xlsx'))
  
  # Save final dataframe containing all tab sheet data 
  col_order <- c("Product_Country", "Product_Hospital", "Product_Entry_Spreadsheet", "Product_table_year", "Product_table_month", "Product", "Product_Entry_Date", "Product_No", "Product_Balance", "Product_Balance_Status", "Product_Units_Received",
                 "Product_Received_From", "Product_Units_Released", "Product_Patient_Name", "Product_Units_Returned",
                 "Product_Returned_By", "Product_Units_Released_perproduct") # CHANGE WAS MADE HERE: COLUMNS "Product_Released_To" and "Product_Patient_ID" were dropped
  df_final <- df_final %>% dplyr::select(., col_order)
  writexl::write_xlsx(df_final, 
                      paste0(path_output, unique(df_final$Product_table_year), '_', country_code, '_', clinic_code, '.xlsx'))
}

#### Add data from final data file to overall dataframe #####

# df_full <- full_join(df_full, df_final)


#### End ####


