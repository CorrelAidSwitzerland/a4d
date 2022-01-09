---------------- # Datacross - A4D Project: Phase 1 ------------- #
  ---------------- # Preprocessing: Medical Supply Data (MSD) ----- #
  ---------------- # Script version: 1.3 -------------------------- #   
  
#### Clear working space and load libraries ####  
# Clear working space #
rm(list = ls())
# Load libraries #
lib_list <- c("tidyverse", "tidyxl", "readxl", "readxl", "stringr", "openxlsx")
lapply(lib_list, library, character.only = TRUE)

#### Define functions #####

`%notin%` <- Negate(`%in%`)




#### DEFINE necessary input ####  

# Information: Define everything in this section that is coloured and in quotes

# Define month and year to which data refers
year <- "2018" # Define number for year, e.g. "2021"
path_import <- "/Volumes/Encrypted_SK/Datacross/A4D/Data/01_rawdata/" # Define path from which data is loaded into this script
path_output <- "/Volumes/Encrypted_SK/Datacross/A4D/Data/02_preprocdata/" # Define path where preprocessed data file shall be stored
hospital <- "YA" # Define name (abbreviation) of hospital
country <- "PB" # Define name of country

input_excel_name <- '02_2018 AN Clinic IX A4D Tracker.xlsx' # Define file name of data set that shall be read in. INCLUDE ENDING (e.g., .xlsx)

#### Loop through each tab sheet ####

# Get excel tabs, and keep only those with the year number
sheets <- excel_sheets(path = paste0(path_import, input_excel_name))
list_excel_tabs <- str_subset(sheets[!grepl(" ", sheets)], "([1:100])")

# Initalize empty feedback dataframe and empty final dataframe #
setwd( path_output)
wb = createWorkbook() # For feedback sheets

# Extract country and hospital name if available # NEW, THIS WAS ADDED
hospital_split <- strsplit(input_excel_name, "AN ")[[1]] ; hospital_split <- strsplit(hospital_split, " A4D")[[2]][1]
hospital <- hospital_split
testdf <- readxl::read_xlsx(paste0(path_import, input_excel_name), sheet = unique(list_excel_tabs)[1]) 
country <- strsplit(na.omit(testdf$`CLINIC SUPPORT PROGRAMME`), "Country_")[[2]][2]
rm(hospital_split,testdf)

# Loop through tabs

for(tab in unique(list_excel_tabs)){
  
  df_alt1 <- readxl::read_xlsx(paste0(path_import, input_excel_name), sheet = tab) 
  
#### Create splits and quality check #####
  #### Tracker version 2021 ####
  if(year == "2021"){
    #### Identify splits ######
    # Get health supply part: 
    # Split when row "MEDICAL SUPPLIES DISTRIBUTION" is followed by row with columns "Product", "Entry Date", "Balance"
    # Get patient recruitments summary part
    # Split when row "PATIENT RECRUITMENT SUMMARY" is followed by rows with columns "Patient ID", "Patient Name", "Province"
    
    for(i in 1:nrow(df_alt1)){
      
      if(any(grepl("MEDICAL", df_alt1[i, ])) &
         any(grepl("Product", df_alt1[i+1, ])) & any(grepl("Entry Date", df_alt1[i+1, ]))){
        
        start_df_msd <- i+1
      }
      
      if(any(grepl("PATIENT RECRUITMENT", df_alt1[i, ])) &
         any(grepl("Patient ID", df_alt1[i+1, ])) & any(grepl("Patient Name", df_alt1[i+1, ]))){
        
        end_df_msd <- i-1
        start_df_patient <- i+1
        end_df_patient <- nrow(df_alt1)
      }
      
    }
    
    
    #### Define medical supplies distribution (MSD) #####
    
    df_msd <- df_alt1[start_df_msd:end_df_msd, ]
    colnames(df_msd) <- df_msd[1,] ; df_msd <- df_msd[-1,] # Redefine columns
    df_msd <- df_msd %>% dplyr::select(    # Define columns to be kept
      c("Product", "Entry Date", "Balance", "Units Received", "Received From", "Units Released", "Released To \r\n(Drop Down List)", "Units Returned", "Returned By")
    )  %>%
      dplyr::rename(Entry_Date = `Entry Date`,
                    Units_Received = `Units Received`,
                    Received_From = `Received From`,
                    Released_To = `Released To \r\n(Drop Down List)`,
                    Returned_By = `Returned By`,
                    Units_Returned = `Units Returned`,
                    Units_Released = `Units Released`
      )
    
    # Remove empty rows
    df_msd <- df_msd %>% filter_all(any_vars(complete.cases(.)))  
    
    
    # Change column "Entry Date" to datetime object
    df_msd$Entry_Date <- as.numeric(df_msd$Entry_Date)  
    df_msd[["Entry_Date"]] <- 
      as.POSIXct( df_msd[["Entry_Date"]] * (60*60*24)
                  , origin="1899-12-30"
                  , tz="GMT")
    
    #### Define patient summary ######
    
    df_patient <- df_alt1[start_df_patient:end_df_patient, ]
    colnames(df_patient) <- df_patient[1,] ; df_patient <- df_patient[-1,] # Redefine columns
    
    
    
    
    
    
    #### Quality check ####
    
    # Define final feedback file which should contain all necessary input columns of this version
    expected_colnames <- c("Product","Entry_Date","Balance","Units_Received","Received_From","Units_Released","Released_To","Units_Returned","Returned_By")
    expected_classes_char <- c("Product", "Received_From", "Released_To", "Returned_By")
    expected_classes_num <- c("Balance", "Units_Received", "Units_Released", "Units_Returned")
    num_rows <- nrow(df_msd)
    df_feedback <- data.frame(matrix(ncol = length(expected_colnames), nrow = num_rows, "CORRECT"))
    colnames(df_feedback) <- expected_colnames
    
    
    # Check1: Is any column name not present?
    if(any(colnames(df_feedback) %notin% colnames(df_msd))){
      missing_cols <- which(colnames(df_feedback) %notin% colnames(df_msd))
      df_feedback[, missing_cols] <- "ISSUE"
    }
    
    #Check2: Are all column classes expected?
    # For character columns
    if(any(sapply(df_msd[, expected_classes_char], class) != "character")){
      wrongclass_cols <- which(sapply(df_msd[, expected_classes_char], class) != "character")
      df_feedback[,wrongclass_cols] <- "ISSUE"
    }
    if(!any(grepl("POSIX", is(df_msd$Entry_Date))) == TRUE){
      df_feedback$Entry_Date <- "ISSUE"
    }
    # For actual numeric columns, if they contain a character value even after trying to trnasform to numeric
    for(j in unique(expected_classes_num)){
      if(any(is.na(as.numeric(na.omit(unlist(df_msd[,j]))))==TRUE)){
        row <- which(is.na(as.numeric(na.omit(unlist(df_msd[,j])))))
        df_feedback[row,j] <- "ISSUE"
      }
    }
    
    # Check3: OPEN  
    
    
  }
  #### Tracker version 2020 ####
  if(year == "2020") {
    #### Identify splits ######
    # Get health supply part:
    # Split when row contains columns with variables "Product", "Date", "Units_Received"
    # Get patient recruitments summary part
    # Split when row "Summary of Patient Recruitment" is followed by row with columns "No", "Patient Name", "Province"
    
    for (i in 1:nrow(df_alt1)) {
      if (any(grepl("Product", df_alt1[i, ])) &
          any(grepl("Date", df_alt1[i, ])) &
          any(grepl("Units Received", df_alt1[i, ]))) {
        start_df_msd <- i + 1
      }
      
      if (any(grepl("Summary of Patient Recruitment", df_alt1[i, ])) &
          any(grepl("No", df_alt1[i + 1, ])) &
          any(grepl("Patient Name", df_alt1[i + 1, ]))) {
        end_df_msd <- i - 1
        start_df_patient <- i + 1
        end_df_patient <- nrow(df_alt1)
      }
      
    }
    
    
    #### For medical supplies distribution (MSD) #####
    
    df_msd <- df_alt1[(start_df_msd - 1):end_df_msd, ]
    colnames(df_msd) <-
      df_msd[1,]
    df_msd <- df_msd[-1,] # Redefine columns
    df_msd <-
      df_msd %>% dplyr::select(
        # Define columns to be kept
        c(
          "Product",
          "Date",
          "Units Received",
          "Received From",
          "Units Released",
          "Released To (select from drop down list)",
          "Units Returned",
          "Returned By"
        )
      )  %>%
      dplyr::rename(., 
                    Entry_Date = `Date`,
                    Units_Received = `Units Received`,
                    Received_From = `Received From`,
                    Released_To = `Released To (select from drop down list)`,
                    Returned_By = `Returned By`,
                    Units_Returned = `Units Returned`,
                    Units_Released = `Units Released`
      )
    
    # Remove empty rows
    df_msd <- df_msd %>% filter_all(any_vars(complete.cases(.)))
    
    
    # Change column "Entry Date" to datetime object
    df_msd$Entry_Date <- as.numeric(df_msd$Entry_Date)
    df_msd[["Entry_Date"]] <-
      as.POSIXct(df_msd[["Entry_Date"]] * (60 * 60 * 24)
                 , origin = "1899-12-30"
                 , tz = "GMT")
    
    #### For patient summary ######
    
    df_patient <- df_alt1[start_df_patient:end_df_patient, ]
    colnames(df_patient) <-
      df_patient[1,]
    df_patient <- df_patient[-1,] # Redefine columns
    
    
    
    #### Quality check ####
    
    # Define final feedback file which should contain all necessary input columns of this version
    expected_colnames <- c("Product","Entry_Date","Units_Received","Units_Released","Released_To","Units_Returned","Returned_By")
    expected_classes_char <- c("Product", "Received_From", "Released_To", "Returned_By")
    expected_classes_num <- c("Received_From",  "Units_Released", "Units_Returned")
    num_rows <- nrow(df_msd)
    df_feedback <- data.frame(matrix(ncol = length(expected_colnames), nrow = num_rows, "CORRECT"))
    colnames(df_feedback) <- expected_colnames
    
    
    # Check1: Is any column name not present?
    if(any(colnames(df_feedback) %notin% colnames(df_msd))){
      missing_cols <- which(colnames(df_feedback) %notin% colnames(df_msd))
      df_feedback[, missing_cols] <- "ISSUE"
    }
    
    #Check2: Are all column classes expected?
    # For character columns
    if(any(sapply(df_msd[, expected_classes_char], class) != "character")){
      wrongclass_cols <- which(sapply(df_msd[, expected_classes_char], class) != "character")
      df_feedback[,wrongclass_cols] <- "ISSUE"
    }
    if(!any(grepl("POSIX", is(df_msd$Entry_Date))) == TRUE){
      df_feedback$Entry_Date <- "ISSUE"
    }
    # For actual numeric columns, if they contain a character value even after trying to trnasform to numeric
    for(j in unique(expected_classes_num)){
      if(any(is.na(as.numeric(na.omit(unlist(df_msd[,j]))))==TRUE)){
        row <- which(is.na(as.numeric(na.omit(unlist(df_msd[,j])))))
        df_feedback[row,j] <- "ISSUE"
      }
    }
    
    # Check3: OPEN  
    
    
    
  }
  #### Tracker version 2019 ####
  if (year == "2019") {
    #### Identify splits ######
    # Get health supply part:
    # Split when row contains columns with variables "Product", "Date", "Units Received"
    # Get patient recruitments summary part
    # Split when row "Summary of Patient Recruitment" is followed by row with columns "No", "Patient Name", "Province"
    
    for (i in 1:nrow(df_alt1)) {
      if (any(grepl("Product", df_alt1[i,])) &
          any(grepl("Date", df_alt1[i,])) &
          any(grepl("Units Received", df_alt1[i,]))) {
        start_df_msd <- i + 1
      }
      
      if (any(grepl("Summary of Patient Recruitment", df_alt1[i,])) &
          any(grepl("No", df_alt1[i + 1,])) &
          any(grepl("Patient Name", df_alt1[i + 1,]))) {
        end_df_msd <- i - 1
        start_df_patient <- i + 1
        end_df_patient <- nrow(df_alt1)
      }
      
    }
    
    
    #### For medical supplies distribution (MSD) #####
    
    df_msd <- df_alt1[(start_df_msd - 1):end_df_msd,]
    colnames(df_msd) <-
      df_msd[1, ]
    df_msd <- df_msd[-1, ] # Redefine columns
    df_msd <-
      df_msd %>% dplyr::select(
        # Define columns to be kept
        c(
          "Product",
          "Date",
          "Units Received",
          "Received From",
          "Units Released",
          "Released To (select from drop down list)",
          "Units Returned",
          "Returned By"
        )
      )  %>%
      dplyr::rename(
        Entry_Date = `Date`,
        Units_Received = `Units Received`,
        Received_From = `Received From`,
        Released_To = `Released To (select from drop down list)`,
        Returned_By = `Returned By`,
        Units_Returned = `Units Returned`,
        Units_Released = `Units Released`
      )
    
    # Remove empty rows
    df_msd <- df_msd %>% filter_all(any_vars(complete.cases(.)))
    
    
    # Change column "Entry Date" to datetime object
    df_msd$Entry_Date <- as.numeric(df_msd$Entry_Date)
    df_msd[["Entry_Date"]] <-
      as.POSIXct(df_msd[["Entry_Date"]] * (60 * 60 * 24)
                 , origin = "1899-12-30"
                 , tz = "GMT")
    
    #### For patient summary ######
    
    df_patient <- df_alt1[start_df_patient:end_df_patient,]
    colnames(df_patient) <-
      df_patient[1, ]
    df_patient <- df_patient[-1, ] # Redefine columns
    
    
    
    #### Quality check ####
    
    # Define final feedback file which should contain all necessary input columns of this version
    expected_colnames <- c("Product","Entry_Date","Units_Received","Received_From", "Units_Released","Released_To","Units_Returned","Returned_By")
    expected_classes_char <- c("Product", "Units_Received",  "Released_To", "Returned_By")
    expected_classes_num <- c("Received_From",  "Units_Released", "Units_Returned")
    num_rows <- nrow(df_msd)
    df_feedback <- data.frame(matrix(ncol = length(expected_colnames), nrow = num_rows, "CORRECT"))
    colnames(df_feedback) <- expected_colnames
    
    
    # Check1: Is any column name not present?
    if(any(colnames(df_feedback) %notin% colnames(df_msd))){
      missing_cols <- which(colnames(df_feedback) %notin% colnames(df_msd))
      df_feedback[, missing_cols] <- "ISSUE"
    }
    
    #Check2: Are all column classes expected?
    # For character columns
    if(any(sapply(df_msd[, expected_classes_char], class) != "character")){
      wrongclass_cols <- which(sapply(df_msd[, expected_classes_char], class) != "character")
      df_feedback[,wrongclass_cols] <- "ISSUE"
    }
    if(!any(grepl("POSIX", is(df_msd$Entry_Date))) == TRUE){
      df_feedback$Entry_Date <- "ISSUE"
    }
    # For actual numeric columns, if they contain a character value even after trying to trnasform to numeric
    for(j in unique(expected_classes_num)){
      if(any(is.na(as.numeric(na.omit(unlist(df_msd[,j]))))==TRUE)){
        row <- which(is.na(as.numeric(na.omit(unlist(df_msd[,j])))))
        df_feedback[row,j] <- "ISSUE"
      }
    }
    
    
    
  }
  #### Tracker version 2018 ####
  if (year == "2018") {
    #### Identify splits ######
    # Get health supply part:
    # Split when row contains columns with variables "Product", "Date", "Units Received"
    # Get patient recruitments summary part
    # Split when row "Summary of Patient Recruitment" is followed by row with columns "No", "Patient Name", "Province"
    
    for (i in 1:nrow(df_alt1)) {
      if (any(grepl("Description of Support", df_alt1[i,])) & # CHANGE WAS MADE HERE; in v1.2 this was "Support"
          any(grepl("Date", df_alt1[i,])) &
          any(grepl("Units Received", df_alt1[i,]))) {
        start_df_msd <- i + 1
      }
      
      if (any(grepl("Summary of Patient Recruitment", df_alt1[i,])) &
          any(grepl("Patient ID", df_alt1[i + 1,])) & # CHANGE WAS MADED HERE; in v1.2 this was "No"
          any(grepl("Patient Name", df_alt1[i + 1,]))) {
        end_df_msd <- i - 1
        start_df_patient <- i + 1
        end_df_patient <- nrow(df_alt1)
      }
      
    }
    
    
    #### For medical supplies distribution (MSD) #####
    
    df_msd <- df_alt1[(start_df_msd - 1):end_df_msd,]
    colnames(df_msd) <-
      df_msd[1, ]
    df_msd <- df_msd[-1, ] # Redefine columns
    
    # CHANGE WAS MADE HERE. Added for V1.3
    # Account for fact that some tabs contain column "Units Returned" but not "Extra Unis Kept in Clinic" and vice versa
    if(any(grepl("Extra Unis Kept in Clinic", colnames(df_msd))) == FALSE){
      df_msd$`Extra Unis Kept in Clinic` <- NA
    }
    if(any(grepl("Units Returned", colnames(df_msd))) == FALSE){
      df_msd$`Units Returned` <- NA
    }
    if(any(grepl("Returned By", colnames(df_msd))) == FALSE){
      df_msd$`Returned By` <- NA
    }
    
    
    df_msd <-
      df_msd %>% dplyr::select(
        # Define columns to be kept
        c(
          "Description of Support", # CHANGE WAS MADE HERE, THIS WAS "Product" BEFOER
          "Date",
          "Units Received",
          "Received From",
          "Units Released",
          "Released To",  # CHANGE WAS MADE HERE; THIS WAS "Released To (select from drop down list)" BEFORE
          "Extra Unis Kept in Clinic", # CHANGE WAS MADED AFTERWARDS, THE COLUMN "Returned By" WAS REMOVED            
          "Returned By"
        )
      )  %>%
      dplyr::rename(
        Product = `Description of Support`,
        Entry_Date = `Date`,
        Units_Received = `Units Received`,
        Received_From = `Received From`,
        Released_To = `Released To`, # CHANGE WAS MADE HERE; THIS WAS "Released To (select from drop down list)" BEFORE
        Returned_By = `Returned By`,
        Units_Returned = `Extra Unis Kept in Clinic`,
        Units_Released = `Units Released`
      )
    
    # Remove empty rows
    df_msd <- df_msd %>% filter_all(any_vars(complete.cases(.)))
    
    
    # Change column "Entry Date" to datetime object
    df_msd$Entry_Date <- as.numeric(df_msd$Entry_Date)
    df_msd[["Entry_Date"]] <-
      as.POSIXct(df_msd[["Entry_Date"]] * (60 * 60 * 24)
                 , origin = "1899-12-30"
                 , tz = "GMT")
    
    #### For patient summary ######
    
    df_patient <- df_alt1[start_df_patient:end_df_patient,]
    colnames(df_patient) <-
      df_patient[1, ]
    df_patient <- df_patient[-1, ] # Redefine columns
    
    
    
    #### Quality check ####
    
    # Define final feedback file which should contain all necessary input columns of this version
    expected_colnames <- c("Product","Entry_Date","Units_Received","Received_From","Units_Released","Released_To","Units_Returned","Returned_By")
    expected_classes_char <- c("Product", "Units_Received", "Released_To", "Returned_By")
    expected_classes_num <- c("Received_From", "Units_Released", "Units_Returned")
    num_rows <- nrow(df_msd)
    df_feedback <- data.frame(matrix(ncol = length(expected_colnames), nrow = num_rows, "CORRECT"))
    colnames(df_feedback) <- expected_colnames
    
    
    # Check1: Is any column name not present?
    if(any(colnames(df_feedback) %notin% colnames(df_msd))){
      missing_cols <- which(colnames(df_feedback) %notin% colnames(df_msd))
      df_feedback[, missing_cols] <- "ISSUE"
    }
    
    #Check2: Are all column classes expected?
    # For character columns
    if(any(sapply(df_msd[, expected_classes_char], class) != "character")){
      wrongclass_cols <- which(sapply(df_msd[, expected_classes_char], class) != "character")
      df_feedback[,wrongclass_cols] <- "ISSUE"
    }
    if(!any(grepl("POSIX", is(df_msd$Entry_Date))) == TRUE){
      df_feedback$Entry_Date <- "ISSUE"
    }
    # For actual numeric columns, if they contain a character value even after trying to trnasform to numeric
    for(j in unique(expected_classes_num)){
      if(any(is.na(as.numeric(na.omit(unlist(df_msd[,j]))))==TRUE)){
        row <- which(is.na(as.numeric(na.omit(unlist(df_msd[,j])))))
        df_feedback[row,j] <- "ISSUE"
      }
    }
    
    # Check3: OPEN  
    
    
    
  }
  #### Tracker version 2017|not09 CHANGE MADE HERE ####
  if (year == "2017" & !grepl("Sep", tab)) {
    #### Identify splits ######
    # Get health supply part:
    # Split when row contains columns with variables "Product", "Date", "Units Received"
    # Get patient recruitments summary part
    # Split when row "Summary of Patient Recruitment" is followed by row with columns "No", "Patient Name", "Province"
    
    rm(start_df_msd, end_df_msd)
    for (i in 1:nrow(df_alt1)) {
      if (any(grepl("Description of Support", df_alt1[i,])) & # CHANGE WAS MADE HERE, THIS WAS "Product" before
          any(grepl("Date", df_alt1[i,])) &
          any(grepl("Units Received", df_alt1[i,]))) {
        start_df_msd <- i + 1
      }
      
      if (any(grepl("Summary of Patient Recruitment", df_alt1[i,])) &
          any(grepl("Patient ID", df_alt1[i + 1,])) & # CHANGE WAS MADE HERE; THIS WAS "No" before
          any(grepl("Patient Name", df_alt1[i + 1,]))) {
        end_df_msd <- i - 1
        start_df_patient <- i + 1
        end_df_patient <- nrow(df_alt1)
      }
    }
    
    # If there is no section for MSD, jump to next monthly tab ; CHANGE WAS MADE HERE; THIS WAS ADDED DUE TO FEB17
    if(!any(grepl("Description of Support", df_alt1))){
      next 
    }
    
    
    #### For medical supplies distribution (MSD) #####
    
    df_msd <- df_alt1[(start_df_msd - 1):end_df_msd,]
    colnames(df_msd) <-
      df_msd[1, ]
    df_msd <- df_msd[-1, ] # Redefine columns
    
    # CHANGE WAS MADE HERE. Added for V1.3
    # Account for fact that some tabs contain column "Units Returned" but not "Extra Unis Kept in Clinic" and vice versa
    if(any(grepl("Extra Unis Kept in Clinic", colnames(df_msd))) == FALSE){
      df_msd$`Extra Unis Kept in Clinic` <- NA
    }
    if(any(grepl("Units Returned", colnames(df_msd))) == FALSE){
      df_msd$`Units Returned` <- NA
    }
    if(any(grepl("Returned By", colnames(df_msd))) == FALSE){
      df_msd$`Returned By` <- NA
    }
    
    df_msd <-
      df_msd %>% dplyr::select(
        # Define columns to be kept
        c(
          "Description of Support", # CHANGE WAS MADED HERE, THIS WAS "Product" BEFORE
          "Date",
          "Units Received",
          "Received From",
          "Units Released",
          "Released To",
          "Units Returned",
          "Returned By",
          "Extra Unis Kept in Clinic" # CHANGE WAS MADED AFTERWARDS, THE COLUMN "Returned By" WAS REMOVED
        )
      )  %>%
      dplyr::rename(
        Product = `Description of Support`,
        Entry_Date = `Date`,
        Units_Received = `Units Received`,
        Received_From = `Received From`,
        Released_To = `Released To`, # CHANGE WAS MADED HERE, THIS WAS "Released To (select from drop down list)" BEFORE
        Returned_By = `Returned By`,
        Units_Returned = `Units Returned`,
        Units_Released = `Units Released`,
        Units_Extra_KeptinClinic = `Extra Unis Kept in Clinic`
      )
    
    # Remove empty rows
    df_msd <- df_msd %>% filter_all(any_vars(complete.cases(.)))
    
    # If there are no data, jump to next month ; CHANGE WAS MADE HERE, THIS WAS ADDED
    if(nrow(df_msd) == 0){
      next
    }
    
    
    # Change column "Entry Date" to datetime object
    df_msd$Entry_Date <- as.numeric(df_msd$Entry_Date)
    df_msd[["Entry_Date"]] <-
      as.POSIXct(df_msd[["Entry_Date"]] * (60 * 60 * 24)
                 , origin = "1899-12-30"
                 , tz = "GMT")
    
    #### For patient summary ######
    
    df_patient <- df_alt1[start_df_patient:end_df_patient,]
    colnames(df_patient) <-
      df_patient[1, ]
    df_patient <- df_patient[-1, ] # Redefine columns
    
    
    
    #### Quality check ####
    
    # Define final feedback file which should contain all necessary input columns of this version
    expected_colnames <- c("Product","Entry_Date","Units_Received","Received_From", "Units_Released","Released_To","Units_Returned","Returned_By")
    expected_classes_char <- c("Product", "Received_From", "Released_To", "Returned_By")
    expected_classes_num <- c("Units_Received",  "Units_Released", "Units_Returned")
    num_rows <- nrow(df_msd)
    df_feedback <- data.frame(matrix(ncol = length(expected_colnames), nrow = num_rows, "CORRECT"))
    colnames(df_feedback) <- expected_colnames
    
    
    # Check1: Is any column name not present?
    if(any(colnames(df_feedback) %notin% colnames(df_msd))){
      missing_cols <- which(colnames(df_feedback) %notin% colnames(df_msd))
      df_feedback[, missing_cols] <- "ISSUE"
    }
    
    #Check2: Are all column classes expected?
    # For character columns
    if(any(sapply(df_msd[, expected_classes_char], class) != "character")){
      wrongclass_cols <- which(sapply(df_msd[, expected_classes_char], class) != "character")
      df_feedback[,wrongclass_cols] <- "ISSUE"
    }
    if(!any(grepl("POSIX", is(df_msd$Entry_Date))) == TRUE){
      df_feedback$Entry_Date <- "ISSUE"
    }
    # For actual numeric columns, if they contain a character value even after trying to trnasform to numeric
    for(j in unique(expected_classes_num)){
      if(any(is.na(as.numeric(na.omit(unlist(df_msd[,j]))))==TRUE)){
        row <- which(is.na(as.numeric(na.omit(unlist(df_msd[,j])))))
        df_feedback[row,j] <- "ISSUE"
      }
    }
    
    # Check3: OPEN  
    
    
    
    
  }
  #### Tracker version 2017|09 ####
  
  if (year == "2017" & grepl("Sep", tab)) {
    #### Identify splits ######
    # Get health supply part:
    # Split when row contains columns with variables "Product", "Date", "Units Received  (bottle/box)"
    # Get patient recruitments summary part
    # Split when row "Summary of Patient Recruitment" is followed by row with columns "ID", "Patient Name", "Province"
    
    for (i in 1:nrow(df_alt1)) {
      if (any(grepl("Description of Support", df_alt1[i,])) &
          any(grepl("Date", df_alt1[i,])) &
          any(grepl("Units Received", df_alt1[i,]))) {
        start_df_msd <- i + 1
      }
      
      if (any(grepl("Summary of Patient Recruitment", df_alt1[i,])) &
          any(grepl("ID", df_alt1[i + 1,])) &
          any(grepl("Patient Name", df_alt1[i + 1,]))) {
        end_df_msd <- i - 1
        start_df_patient <- i + 1
        end_df_patient <- nrow(df_alt1)
      }
      
    }
    
    
    #### For medical supplies distribution (MSD) #####
    
    df_msd <- df_alt1[(start_df_msd - 1):end_df_msd,]
    colnames(df_msd) <-
      df_msd[1, ]
    df_msd <- df_msd[-1, ] # Redefine columns
    
    # CHANGE WAS MADE HERE. Added for V1.3
    # Account for fact that some tabs contain column "Units Returned" but not "Extrar Unis Kept in Clinic" and vice versa
    if(any(grepl("Extra Unis Kept in Clinic", colnames(df_msd))) == FALSE){
      df_msd$`Extra Unis Kept in Clinic` <- NA
    }
    if(any(grepl("Units Returned", colnames(df_msd))) == FALSE){
      df_msd$`Units Returned` <- NA
    }
    
    
    df_msd <-
      df_msd %>% dplyr::select(
        # Define columns to be kept
        c(
          "Description of Support",
          "Date",
          "Units Received", # CHANGE WAS MADE HERE; THIS WAS "Units Received \r\n(bottle/box)" BEFORE
          "Received From",
          "Units Released",  # CHANGE WAS MADE HERE; THIS WAS "Units Released\r\n(box)" BEFORE
          "Units Returned", # CHANGE WAS MADE HERE, THIS WAS ADDED
          "Released To",
          "Extra Unis Kept in Clinic"
        )
      )  %>%
      dplyr::rename(
        Product = `Description of Support`, # CHANGE WAS MADED HERE, THIS WAS "Product" BEFORE
        Units_Returned =`Units Returned`, # CHANGE QWAS MADE HERE; THIS LINE WAS MISSING BEFORE
        Entry_Date = `Date`,
        Units_Received = `Units Received`,
        Received_From = `Received From`,
        Released_To = `Released To`,
        Units_Released = `Units Released`,
        Units_Extra_KeptinClinic = `Extra Unis Kept in Clinic`
      )
    
    # Remove empty rows
    df_msd <- df_msd %>% filter_all(any_vars(complete.cases(.)))
    
    
    # Change column "Entry Date" to datetime object
    df_msd$Entry_Date <- as.numeric(df_msd$Entry_Date)
    df_msd[["Entry_Date"]] <-
      as.POSIXct(df_msd[["Entry_Date"]] * (60 * 60 * 24)
                 , origin = "1899-12-30"
                 , tz = "GMT")
    
    #### For patient summary ######
    
    df_patient <- df_alt1[start_df_patient:end_df_patient,]
    colnames(df_patient) <-
      df_patient[1, ]
    df_patient <- df_patient[-1, ] # Redefine columns
    
    
    
    #### Quality check ####
    
    # Define final feedback file which should contain all necessary input columns of this version
    expected_colnames <- c("Product","Entry_Date","Units_Received","Received_From", "Units_Released","Released_To")
    expected_classes_char <- c("Product", "Received_From", "Released_To")
    expected_classes_num <- c("Units_Received",  "Units_Released")
    num_rows <- nrow(df_msd)
    df_feedback <- data.frame(matrix(ncol = length(expected_colnames), nrow = num_rows, "CORRECT"))
    colnames(df_feedback) <- expected_colnames
    
    
    # Check1: Is any column name not present?
    if(any(colnames(df_feedback) %notin% colnames(df_msd))){
      missing_cols <- which(colnames(df_feedback) %notin% colnames(df_msd))
      df_feedback[, missing_cols] <- "ISSUE"
    }
    
    #Check2: Are all column classes expected?
    # For character columns
    if(any(sapply(df_msd[, expected_classes_char], class) != "character")){
      wrongclass_cols <- which(sapply(df_msd[, expected_classes_char], class) != "character")
      df_feedback[,wrongclass_cols] <- "ISSUE"
    }
    if(!any(grepl("POSIX", is(df_msd$Entry_Date))) == TRUE){
      df_feedback$Entry_Date <- "ISSUE"
    }
    # For actual numeric columns, if they contain a character value even after trying to trnasform to numeric
    for(j in unique(expected_classes_num)){
      if(any(is.na(as.numeric(na.omit(unlist(df_msd[,j]))))==TRUE)){
        row <- which(is.na(as.numeric(na.omit(unlist(df_msd[,j])))))
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
  #### Tracker version 2021 ####
  if(year == "2021"){
    #### Define country #####
    
    # Extract from Patient ID first part (should be country)
    for(i in 1:nrow(df_patient)){
      df_patient$country[i] <- as.character(strsplit(df_patient$`Patient ID`, "_")[[i]][[1]])
    }
    
    # Get most common value
    df_patient$country <- unlist(df_patient$country) 
    country <- names(sort(table(df_patient$country),decreasing=TRUE)[1])
    
    
    
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
    
    df_msd$Product_Entry_Spreadsheet <- date
    
    
    
    #### Add variables country and hospital #####
    df_msd$Product_Country <- country    
    df_msd$Product_Hospital <- hospital    
    
    #### Add variable table_month and table_year ####    
    latest_date <- as.character(max(df_msd$Entry_Date, na.rm = TRUE))
    year <- as.numeric(strsplit(latest_date, "-")[[1]][1]) ; month <- as.character(strsplit(latest_date, "-")[[1]][2])
    
    df_msd$Product_table_month <- month
    df_msd$Product_table_year <- year
    
    #### Rename column "Released_To" to "Patient_Name" #####
    df_msd$Patient_Name <- df_msd$Released_To 
    
    #### Extract Patient Identifier and add to Released_To #####
    df_patient_id <- df_patient %>%
      dplyr::select(`Patient Name`, `Patient ID`)
    df_msd <- dplyr::left_join(df_msd, df_patient_id, by = c("Released_To" = "Patient Name")) 
    df_msd$Released_To <- df_msd$`Patient ID`
    #df_msd <- df_msd %>% select(., -matches("Patient ID"))
    
    #### Rename variable patient ID #####
    df_msd <- dplyr::rename(df_msd, "Patient_ID" = "Patient ID")
    
    #### Extend Product Name #####
    df_msd <- df_msd %>% fill(Product)
    
    #### Add additional column Product No #####
    df_msd <- transform(df_msd,Product_No=as.numeric(factor(Product)))
    
    #### Remove rows without changes in distribution ####
    delete_rows <- as.vector(0)
    for(i in 1:nrow(df_msd)){
      
      if((is.na(df_msd$Units_Released[i]) == TRUE) & (is.na(df_msd$Units_Released[i]) == TRUE) &
         (is.na(df_msd$Released_To[i]) == TRUE) & (is.na(df_msd$Entry_Date[i]) == TRUE)){
        delete_rows[length(delete_rows)+1] <- i
      }
    }
    
    df_msd <- df_msd[-delete_rows,] 
    
    #### Add additional column Balance_Status ####
    df_msd$Balance_Status <-  "change"
    for(j in unique(df_msd$Product)){
      
      # Define start
      df_msd[(df_msd$Balance ==  max(df_msd$Balance[df_msd$Product == j])) & (is.na(df_msd$Units_Released) == TRUE) & (df_msd$Product == j), 'Balance_Status'] <- "start"
      # Define end
      df_msd[(df_msd$Balance ==  min(df_msd$Balance[df_msd$Product == j])) & (is.na(df_msd$Units_Received) == FALSE) & (df_msd$Product == j), 'Balance_Status'] <- "end"
    }
    
    #### Add additional column Units_Released_perproduct ####
    df_msd$Product_Units_Released_perproduct <- NA
    for(j in unique(df_msd$Product)){
      df_sub <- df_msd[df_msd$Product == j,]
      units <- df_sub[nrow(df_sub), 'Units_Released']
      df_msd[df_msd$Product == j, 'Product_Units_Released_perproduct'] <- units
    }
    
    #### Rename column names #####
    # Where "Product" is not in the prefix, add it
    old_col_names <- unique(colnames(df_msd))
    index <- 0
    for(j in unique(colnames(df_msd))){
      index <- index + 1
      if(!startsWith(j, "Product")){
        colnames(df_msd)[index] <- paste0("Product_", j)
      }
    }
    
    #### Change order of columns ####
    df_msd <- subset(df_msd, select=c(Product, Product_Entry_Date, Product_Balance, Product_Units_Received, Product_Received_From, Product_Units_Released,
                                      Product_Released_To, Product_Patient_Name, Product_Patient_ID, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                      Product_No, Product_Balance_Status, Product_Units_Released_perproduct))
    
    
    #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
    df_msd <- df_msd %>% 
      mutate(
        across(c(Product_Units_Received, Product_Units_Released, Product_Units_Returned), ~replace_na(.x, 0))
      )
    
  }  
  #### Tracker version 2020 ####
  if(year == "2020"){
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
    
    df_msd$Product_Entry_Spreadsheet <- date
    
    
    
    #### Add variables country and hospital #####
    df_msd$Product_Country <- country    
    df_msd$Product_Hospital <- hospital    
    
    #### Add variable table_month and table_year ####    
    latest_date <- as.character(max(df_msd$Entry_Date, na.rm = TRUE))
    year <- as.numeric(strsplit(latest_date, "-")[[1]][1]) ; month <- as.character(strsplit(latest_date, "-")[[1]][2])
    
    df_msd$table_month <- month
    df_msd$table_year <- year
    
    #### Rename column "Released_To" to "Patient_Name" #####
    
    df_msd$Patient_Name <- df_msd$Released_To 
    
    
    #### Add additional variable Patient Identifier and assign to Released_To #####
    
    # If there is a column with patient ID in patient table
    if(any(colnames(df_patient) %in% "ID")){
      df_patient_id <- df_patient %>%
        dplyr::select(`Patient Name`, `Patient ID`)
      df_msd <- dplyr::left_join(df_msd, df_patient_id, by = c("Released_To" = "Patient Name"))
      df_msd <- dplyr::rename(df_msd, "Patient_ID" = "Patient ID")
      df_msd$Released_To <- df_msd$Patient_ID
      
    } else{
      df_msd$Patient_ID <- NA ; df_msd$Released_To <- df_msd$Patient_ID
    }
    
    
    #### Extend Product Name #####
    df_msd <- df_msd %>% fill(Product)
    
    #### Extend Entry_Date ####
    df_msd <- df_msd %>% fill(Entry_Date)
    
    #### Add additional column Product No #####
    df_msd <- transform(df_msd,Product_No=as.numeric(factor(Product)))
    
    #### Remove rows without changes in distribution ####
    
    delete_rows <- as.vector(NA)
    for(i in 1:nrow(df_msd)){
      if((is.na(df_msd$Units_Received[i]) == TRUE) & (is.na(df_msd$Units_Released[i]) == TRUE)){
        delete_rows[length(delete_rows)+1] <- i
      }
    }
    if(length(delete_rows) > 0 & is.na(delete_rows) == FALSE){ 
      df_msd <- df_msd[-delete_rows,] 
    }
    
    #### Add additional column Balance_Status ####
    df_msd$Balance_Status <- "change"
    for(j in unique(df_msd$Product)){
      print(j)
      # Define start
      
      df_msd[grepl("START", df_msd$Units_Received) & is.na(df_msd$Units_Received) == FALSE & df_msd$Product == j, 'Balance_Status'] <- "start"
      # df_msd[(df_msd$Received_From == max(df_msd$Received_From[df_msd$Product == j], na.rm = TRUE)) & (is.na(df_msd$Units_Released) == TRUE) & (df_msd$Product == j) & (df_msd$Entry_Date == min(df_msd$Entry_Date[df_msd$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "start"
      
      # Define end
      df_msd[(is.na(df_msd$Units_Released) == TRUE) & (df_msd$Product == j) & (df_msd$Entry_Date == max(df_msd$Entry_Date[df_msd$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "end"
    }
    
    #### Add additional column Units_Released_perproduct ####
    df_msd$Product_Units_Released_perproduct <- NA
    for(j in unique(df_msd$Product)){
      df_sub <- df_msd[df_msd$Product == j,]
      units <- sum(as.numeric(df_sub$Units_Released), na.rm = TRUE)
      df_msd[df_msd$Product == j, 'Product_Units_Released_perproduct'] <- units
    }
    
    
    
    #### Add additional column Product_Balance ####
    
    for(i in 1:nrow(df_msd)){
      for(j in unique(df_msd$Product)){
        
        # 
        if(df_msd$Balance_Status[i] == "start"){
          df_msd[i, 'Product_Balance'] <- df_msd[i, 'Received_From'] 
          start_value <- df_msd[i, 'Received from']
        }
        if(df_msd$Balance_Status[i] == "change"){
          
          release <- NA
          reception <- NA
          if(is.na(as.numeric(df_msd[i, 'Units_Released'])) == TRUE){
            release <- as.numeric(0)
          } else{
            release <- as.numeric(df_msd[i, 'Units_Released'])
          }
          if(is.na(as.numeric(df_msd[i, 'Units_Received'])) == TRUE){
            reception <- as.numeric(0)
          } else{
            reception <- as.numeric(df_msd[i, 'Units_Received'])
          }
          df_msd[i, 'Product_Balance'] <- as.numeric(df_msd[i-1, 'Product_Balance']) -  release  + reception
        }
        
        if(df_msd$Balance_Status[i] == "end"){
          df_msd[i, 'Product_Balance'] <- df_msd[i, 'Received_From'] 
        }
        
      }
    }
    
    
    #### Recode column values: Units_Received and Received_From #####
    
    # 1.  Units_Received should contain numbers, fill with 0s where characters are the moment
    # 2.  Received_From should be name, not numeric. Fill with empty as character where numbers are.   
    
    for(i in 1:nrow(df_msd)){
      if(is.na(df_msd$Units_Received[i]) == FALSE & is.character(df_msd$Units_Received[i]) == TRUE){
        df_msd$Units_Received[i] <- as.numeric(0)
      }
    }
    
    
    for(i in 1:nrow(df_msd)){
      if(is.na(df_msd$Received_From[i]) == FALSE & grepl("[[:digit:]]", df_msd$Received_From[i]) == TRUE){
        df_msd$Received_From[i] <- as.character(NA)
      }
    }
    
    
    #### Rename column values, add prefix ####
    # Where "Product" is not in the prefix, add it
    old_col_names <- unique(colnames(df_msd))
    index <- 0
    for(j in unique(colnames(df_msd))){
      index <- index + 1
      if(!startsWith(j, "Product")){
        colnames(df_msd)[index] <- paste0("Product_", j)
      }
    }
    
    
    
    #### Change order of columns to match v2021 version ####
    df_msd <- subset(df_msd, select=c(Product, Product_Entry_Date, Product_Balance, Product_Units_Received, Product_Received_From, Product_Units_Released,
                                      Product_Released_To, Product_Patient_Name, Product_Patient_ID, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                      Product_No, Product_Balance_Status, Product_Units_Released_perproduct))  
    
    #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
    df_msd <- df_msd %>% 
      mutate(
        across(c(Product_Units_Received, Product_Units_Released, Product_Units_Returned), ~replace_na(.x, 0))
      )
    
    
  }  
  #### Tracker version 2019 ####
  if(year == "2019"){
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
    
    df_msd$Product_Entry_Spreadsheet <- date
    
    
    
    #### Add variables country and hospital #####
    df_msd$Product_Country <- country    
    df_msd$Product_Hospital <- hospital    
    
    #### Add variable table_month and table_year ####    
    latest_date <- as.character(max(df_msd$Entry_Date, na.rm = TRUE))
    year <- as.numeric(strsplit(latest_date, "-")[[1]][1]) ; month <- as.character(strsplit(latest_date, "-")[[1]][2])
    
    df_msd$table_month <- month
    df_msd$table_year <- year
    
    #### Rename column "Released_To" to "Patient_Name" #####
    
    df_msd$Patient_Name <- df_msd$Released_To 
    
    
    #### Add additional variable Patient Identifier and assign to Released_To #####
    
    # If there is a column with patient ID in patient table
    if(any(colnames(df_patient) %in% "ID")){
      df_patient_id <- df_patient %>%
        dplyr::select(`Patient Name`, `Patient ID`)
      df_msd <- dplyr::left_join(df_msd, df_patient_id, by = c("Released_To" = "Patient Name"))
      df_msd <- dplyr::rename(df_msd, "Patient_ID" = "Patient ID")
      df_msd$Released_To <- df_msd$Patient_ID
      
    } else{
      df_msd$Patient_ID <- NA ;  df_msd$Released_To <- df_msd$Patient_ID
    }
    
    
    #### Extend Product Name #####
    df_msd <- df_msd %>% fill(Product)
    
    #### Extend Entry_Date ####
    df_msd <- df_msd %>% fill(Entry_Date)
    
    #### Add additional column Product No #####
    df_msd <- transform(df_msd,Product_No=as.numeric(factor(Product)))
    
    #### Remove rows without changes in distribution ####
    
    delete_rows <- as.vector(NA)
    for(i in 1:nrow(df_msd)){
      if((is.na(df_msd$Units_Received[i]) == TRUE) & (is.na(df_msd$Units_Released[i]) == TRUE)){
        delete_rows[length(delete_rows)+1] <- i
      }
    }
    if(length(delete_rows) > 0 & is.na(delete_rows) == FALSE){ 
      df_msd <- df_msd[-delete_rows,] 
    }
    
    #### Add additional column Balance_Status ####
    df_msd$Balance_Status <- "change"
    for(j in unique(df_msd$Product)){
      print(j)
      # Define start
      
      df_msd[grepl("START", df_msd$Units_Received) & is.na(df_msd$Units_Received) == FALSE & df_msd$Product == j, 'Balance_Status'] <- "start"
      # df_msd[(df_msd$Received_From == max(df_msd$Received_From[df_msd$Product == j], na.rm = TRUE)) & (is.na(df_msd$Units_Released) == TRUE) & (df_msd$Product == j) & (df_msd$Entry_Date == min(df_msd$Entry_Date[df_msd$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "start"
      
      # Define end
      df_msd[(is.na(df_msd$Units_Released) == TRUE) & (df_msd$Product == j) & (df_msd$Entry_Date == max(df_msd$Entry_Date[df_msd$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "end"
    }
    
    #### Add additional column Units_Released_perproduct ####
    df_msd$Product_Units_Released_perproduct <- NA
    for(j in unique(df_msd$Product)){
      df_sub <- df_msd[df_msd$Product == j,]
      units <- sum(as.numeric(df_sub$Units_Released), na.rm = TRUE)
      df_msd[df_msd$Product == j, 'Product_Units_Released_perproduct'] <- units
    }
    
    
    
    #### Add additional column Product_Balance ####
    df_msd$Product_Balance <- NA ; 
    
    for(i in 1:nrow(df_msd)){
      for(j in unique(df_msd$Product)){
        
        # 
        if(df_msd$Balance_Status[i] == "start"){
          df_msd[i, 'Product_Balance'] <- df_msd[i, 'Received_From'] 
          start_value <- df_msd[i, 'Received from']
        }
        if(df_msd$Balance_Status[i] == "change"){
          
          release <- NA
          reception <- NA
          if(is.na(as.numeric(df_msd[i, 'Units_Released'])) == TRUE){
            release <- as.numeric(0)
          } else{
            release <- as.numeric(df_msd[i, 'Units_Released'])
          }
          if(is.na(as.numeric(df_msd[i, 'Units_Received'])) == TRUE){
            reception <- as.numeric(0)
          } else{
            reception <- as.numeric(df_msd[i, 'Units_Received'])
          }
          df_msd[i, 'Product_Balance'] <- as.numeric(df_msd[i-1, 'Product_Balance']) -  release  + reception
        }
        
        if(df_msd$Balance_Status[i] == "end"){
          df_msd[i, 'Product_Balance'] <- df_msd[i, 'Received_From'] 
        }
        
      }
    }
    
    
    #### Recode column values: Units_Received and Received_From #####
    
    # 1.  Units_Received should contain numbers, fill with 0s where characters are the moment
    # 2.  Received_From should be name, not numeric. Fill with empty as character where numbers are.   
    
    for(i in 1:nrow(df_msd)){
      if(is.na(df_msd$Units_Received[i]) == FALSE & is.character(df_msd$Units_Received[i]) == TRUE){
        df_msd$Units_Received[i] <- as.numeric(0)
      }
    }
    
    
    for(i in 1:nrow(df_msd)){
      if(is.na(df_msd$Received_From[i]) == FALSE & grepl("[[:digit:]]", df_msd$Received_From[i]) == TRUE){
        df_msd$Received_From[i] <- as.character(NA)
      }
    }
    
    
    #### Rename column values, add prefix ####
    # Where "Product" is not in the prefix, add it
    old_col_names <- unique(colnames(df_msd))
    index <- 0
    for(j in unique(colnames(df_msd))){
      index <- index + 1
      if(!startsWith(j, "Product")){
        colnames(df_msd)[index] <- paste0("Product_", j)
      }
    }
    
    
    
    #### Change order of columns to match v2021 version ####
    df_msd <- subset(df_msd, select=c(Product, Product_Entry_Date, Product_Balance, Product_Units_Received, Product_Received_From, Product_Units_Released,
                                      Product_Released_To, Product_Patient_Name, Product_Patient_ID, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                      Product_No, Product_Balance_Status, Product_Units_Released_perproduct))
    
    #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
    df_msd <- df_msd %>% 
      mutate(
        across(c(Product_Units_Received, Product_Units_Released, Product_Units_Returned), ~replace_na(.x, 0))
      )
    
    
  }  
  #### Tracker version 2018 CHANGE WAS MADE HERE ####
  if(year == "2018"){
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
    
    df_msd$Product_Entry_Spreadsheet <- date
    
    
    #### Add variables country and hospital #####
    df_msd$Product_Country <- country    
    df_msd$Product_Hospital <- hospital    
    
    #### Add variable table_month and table_year ####    
    latest_date <- as.character(max(df_msd$Entry_Date, na.rm = TRUE))
    year <- as.numeric(strsplit(latest_date, "-")[[1]][1]) ; month <- as.character(strsplit(latest_date, "-")[[1]][2])
    
    df_msd$table_month <- month
    df_msd$table_year <- year
    
    #### Rename column "Released_To" to "Patient_Name" #####
    
    df_msd$Patient_Name <- df_msd$Released_To 
    
    
    #### Add additional variable Patient Identifier and assign to "Released_To" #####
    
    # If there is a column with patient ID in patient table
    if(any(colnames(df_patient) %in% "ID")){
      df_patient_id <- df_patient %>%
        dplyr::select(`Patient Name`, `Patient ID`)
      df_msd <- dplyr::left_join(df_msd, df_patient_id, by = c("Released_To" = "Patient Name"))
      df_msd <- dplyr::rename(df_msd, "Patient_ID" = "Patient ID")
      df_msd$Released_To <- df_msd$Patient_ID
    } else{
      df_msd$Patient_ID <- NA ;  df_msd$Released_To <- df_msd$Patient_ID
    }
    
    
    #### Extend Product Name #####
    df_msd <- df_msd %>% fill(Product)
    
    #### Extend Entry_Date ####
    df_msd <- df_msd %>% fill(Entry_Date)
    
      # THIS WAS ADDED IN V1.3
    # If empty date but entry was made (e.g., due to note of stock), use date of next entry row (e.g., Apr18, row 1 needs date)
    if(any(is.na(df_msd[is.na(df_msd$Product) == FALSE, 'Entry_Date']) == TRUE)){
      df_msd <- df_msd %>% fill(Entry_Date, .direction = "up")
    }
    
    #### Add additional column Product No #####
    df_msd <- transform(df_msd,Product_No=as.numeric(factor(Product)))
    
    #### Remove rows without changes in distribution ####
    
    delete_rows <- as.vector(NA)
    for(i in 1:nrow(df_msd)){
      if((is.na(df_msd$Units_Received[i]) == TRUE) & (is.na(df_msd$Units_Released[i]) == TRUE)){
        delete_rows[length(delete_rows)+1] <- i
      }
    }
    if(length(delete_rows) > 0 & any(is.na(delete_rows) == FALSE)){ 
      df_msd <- df_msd[-as.numeric(na.omit(delete_rows)),]  # CHANGE WAS MADE HERE; THIS DID NOT EXCLUDE NAs BEFORE
      
    }
    
    #### Add additional column Balance_Status ####
    df_msd$Balance_Status <- "change"
    for(j in unique(df_msd$Product)){
      print(j)
      # Define start # CHANGE WAS MADE HERE; in v1.3, the start may also be the first line
      df_msd[grepl("START", df_msd$Units_Received) & is.na(df_msd$Units_Received) == FALSE & df_msd$Product == j, 'Balance_Status'] <- "start"
      
      # CHANGE WAS MADE HERE; in v1.3, the start may also be the first line if there is no "START" argument before
      if(!any(df_msd[grepl("start", df_msd$Balance_Status[df_msd$Product == j])])){
        df_msd[(df_msd$Product == j) & df_msd$Entry_Date == min(df_msd$Entry_Date[df_msd$Product == j]), 'Balance_Status'] <- "start"
        
      }
      
      # df_msd[(df_msd$Received_From == max(df_msd$Received_From[df_msd$Product == j], na.rm = TRUE)) & (is.na(df_msd$Units_Released) == TRUE) & (df_msd$Product == j) & (df_msd$Entry_Date == min(df_msd$Entry_Date[df_msd$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "start"
      
      # Define end
      df_msd[(is.na(df_msd$Units_Released) == TRUE) & (df_msd$Product == j) & (df_msd$Entry_Date == max(df_msd$Entry_Date[df_msd$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "end"
    }
    
    #### Add additional column Units_Released_perproduct ####
    df_msd$Product_Units_Released_perproduct <- NA
    for(j in unique(df_msd$Product)){
      df_sub <- df_msd[df_msd$Product == j,]
      units <- sum(as.numeric(df_sub$Units_Released), na.rm = TRUE)
      df_msd[df_msd$Product == j, 'Product_Units_Released_perproduct'] <- units
    }
    
    
    
    #### Add additional column Product_Balance ####
    
    df_msd$Product_Balance <- NA ; 
    
    for(i in 1:nrow(df_msd)){
      for(j in unique(df_msd$Product)){
        
        # 
        if(df_msd$Balance_Status[i] == "start"){
          df_msd[i, 'Product_Balance'] <- df_msd[i, 'Received_From'] 
          start_value <- df_msd[i, 'Received from']
        }
        if(df_msd$Balance_Status[i] == "change"){
          
          release <- NA
          reception <- NA
          if(is.na(as.numeric(df_msd[i, 'Units_Released'])) == TRUE){
            release <- as.numeric(0)
          } else{
            release <- as.numeric(df_msd[i, 'Units_Released'])
          }
          if(is.na(as.numeric(df_msd[i, 'Units_Received'])) == TRUE){
            reception <- as.numeric(0)
          } else{
            reception <- as.numeric(df_msd[i, 'Units_Received'])
          }
          
          # CHANGE WAS MADE HERE:
          # If start with "change" and no prior information is available, subtract reception from release
          if(i == 1){
            df_msd[i, 'Product_Balance'] <- reception - release
          } else{
            df_msd[i, 'Product_Balance'] <- as.numeric(df_msd[i-1, 'Product_Balance']) -  release  + reception
          }
        }
        
        if(df_msd$Balance_Status[i] == "end"){
          df_msd[i, 'Product_Balance'] <- df_msd[i, 'Received_From'] 
        }
        
      }
    }
    
    
    #### Recode column values: Units_Received and Received_From (incl. Check10) #####
    
    # 1.  Units_Received should contain numbers, fill with 0s where characters are the moment
    # 2.  Received_From should be name, not numeric. Fill with empty as character where numbers are.   
    check10 <- matrix(NA, nrow = 1, ncol = 2)  
    check10[,1] <- "Quality Check 10: Is there any unexpected number in Column Received From?"
    
    df_msd$Units_Received2 <- NA
    for(i in 1:nrow(df_msd)){
      if(is.na(df_msd$Units_Received[i]) == FALSE & is.character(df_msd$Units_Received[i]) == TRUE){
        df_msd[i, 'Units_Received2'] <- as.numeric(0)
      }
    }
    
    
    for(i in 1:nrow(df_msd)){
      # If a start or end balance is met
      if(is.na(df_msd$Received_From[i]) == FALSE & grepl("[[:digit:]]", df_msd$Received_From[i]) == TRUE & any(stringi::stri_detect_fixed(na.omit(df_msd$Units_Received[i]),c("START", "END"))) == TRUE){
        df_msd$Received_From[i] <- as.character(NA)
      }
      # If there is a number in "received_from" without being a start or end balance, yield error
      if(is.na(df_msd$Received_From[i]) == FALSE & grepl("[[:digit:]]", df_msd$Received_From[i]) == TRUE & any(stringi::stri_detect_fixed(na.omit(df_msd$Units_Received[i]),c("START", "END"))) == FALSE){
        check10[,2] <- "Error! Please check in column Received From if there is number which does not represent Start or End balance and should actually be assigned to column Units Received or Units Released."
      }
    }
    if(is.na(check10[,2]) == TRUE){
      check10[,2] <- "Check. No inconsistencies."
    }
    
    df_msd <- df_msd %>% dplyr::select(., -ends_with("Units_Received")) %>% dplyr::rename(., Units_Received = Units_Received2)
    
    
    #### Rename column values, add prefix ####
    # Where "Product" is not in the prefix, add it
    old_col_names <- unique(colnames(df_msd))
    index <- 0
    for(j in unique(colnames(df_msd))){
      index <- index + 1
      if(!startsWith(j, "Product")){
        colnames(df_msd)[index] <- paste0("Product_", j)
      }
    }
    
    
    
    #### Change order of columns to match v2021 version ####
    df_msd <- subset(df_msd, select=c(Product, Product_Entry_Spreadsheet, Product_Entry_Date, Product_Balance, Product_Units_Received, Product_Received_From, Product_Units_Released,
                                      Product_Released_To, Product_Patient_Name, Product_Patient_ID, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                      Product_No, Product_Balance_Status, Product_Units_Released_perproduct))
    
    #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
    df_msd <- df_msd %>% 
      tidyr::replace_na(list(Product_Units_Received = 0, Product_Units_Released = 0, Product_Units_Returned = 0))
    
    
    
    
  } 
  #### Tracker version 2017|not09 CHANGE MADE HERE ####
  if(year == "2017" & !grepl("Sep", tab)){
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
    
    df_msd$Product_Entry_Spreadsheet <- date
    
    #### Add variables country and hospital #####
    df_msd$Product_Country <- country    
    df_msd$Product_Hospital <- hospital    
    
    #### Add variable table_month and table_year ####    
    latest_date <- as.character(max(df_msd$Entry_Date, na.rm = TRUE))
    year <- as.numeric(strsplit(latest_date, "-")[[1]][1]) ; month <- as.character(strsplit(latest_date, "-")[[1]][2])
    
    df_msd$table_month <- month
    df_msd$table_year <- year
    
    #### Rename column "Released_To" to "Patient_Name" #####
    
    df_msd$Patient_Name <- df_msd$Released_To 
    
    
    #### Add additional variable Patient Identifier and assign to "Released_To" #####
    
    # If there is a column with patient ID in patient table
    if(any(colnames(df_patient) %in% "ID")){
      df_patient_id <- df_patient %>%
        dplyr::select(`Patient Name`, `Patient ID`)
      df_msd <- dplyr::left_join(df_msd, df_patient_id, by = c("Released_To" = "Patient Name"))
      df_msd <- dplyr::rename(df_msd, "Patient_ID" = "Patient ID")
      df_msd$Released_To <- df_msd$Patient_ID
    } else{
      df_msd$Patient_ID <- NA ;  df_msd$Released_To <- df_msd$Patient_ID
    }
    
    
    #### Extend Product Name #####
    df_msd <- df_msd %>% fill(Product)
    
    #### Extend Entry_Date ####
    df_msd <- df_msd %>% fill(Entry_Date)
    
    #### Add additional column Product No #####
    df_msd <- transform(df_msd,Product_No=as.numeric(factor(Product)))
    
    #### Remove rows without changes in distribution ####
    
    delete_rows <- as.vector(NA)
    for(i in 1:nrow(df_msd)){
      if((is.na(df_msd$Units_Received[i]) == TRUE) & (is.na(df_msd$Units_Released[i]) == TRUE)){
        delete_rows[length(delete_rows)+1] <- i
      }
    }
    if(length(delete_rows) > 0 & is.na(delete_rows) == FALSE){ 
      df_msd <- df_msd[-delete_rows,] 
    }
    
    #### Add additional column Balance_Status ####
    df_msd$Balance_Status <- "change"
    for(j in unique(df_msd$Product)){
      print(j)
      # Define start
      #(df_msd$Received_From == max(df_msd$Received_From[df_msd$Product == j], na.rm = TRUE)) & (is.na(df_msd$Units_Released) == TRUE) & (df_msd$Product == j) & 
      df_msd[(df_msd$Product == j) & (df_msd$Entry_Date == min(df_msd$Entry_Date[df_msd$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "start"
      # Define end
      df_msd[(is.na(df_msd$Units_Released) == TRUE) & (df_msd$Product == j) & (df_msd$Entry_Date == max(df_msd$Entry_Date[df_msd$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "end"
    }
    
    #### Add additional column Units_Released_perproduct ####
    df_msd$Product_Units_Released_perproduct <- NA
    for(j in unique(df_msd$Product)){
      df_sub <- df_msd[df_msd$Product == j,]
      units <- sum(as.numeric(df_sub$Units_Released), na.rm = TRUE)
      df_msd[df_msd$Product == j, 'Product_Units_Released_perproduct'] <- units
    }
    
    
    
    #### Add additional column Product_Balance and replace Units_columns with 0 where NA ####
    
    # Initiate
    df_msd$Product_Balance <- NA
    
    # Compute Balance based on received vs released # CHANGE WAS MADED HERE, THIS SECTION WAS ADDED
    
    # Fill empty Units_Received rows with 0 for Balance computation
    df_msd <- df_msd %>% mutate_at(vars(
      matches(c("Units_Received", "Units_Released", "Units_Returned"))), ~replace(., is.na(.), 0)) %>%
      mutate(
        Units_Received = as.numeric(gsub("`", "", Units_Received)),
        Units_Released = as.numeric(gsub("`", "", Units_Released)),
        Units_Returned = as.numeric(gsub("`", "", Units_Returned))
      )
    
    # Loop through each product
    for(i in 1:nrow(df_msd)){
      for(j in unique(df_msd$Product)){
        start_value <- as.numeric(head(df_msd$Units_Received[df_msd$Product == j], n = 1))
        
        if(as.numeric(df_msd[i, 'Units_Received']) == start_value){ 
          df_msd[i, 'Product_Balance'] <- start_value
          new_value <- start_value
        } else{
          df_msd[i, 'Product_Balance'] <- new_value + na.omit(as.numeric(df_msd[i, 'Units_Received'])) -  na.omit(as.numeric(df_msd[i, 'Units_Released']))
        }}}
    
    
    #### Recode column values: Units_Received and Received_From (incl. Check10) #####
    
    # 1.  Units_Received should contain numbers, fill with 0s where characters are the moment
    # 2.  Received_From should be name, not numeric. Fill with empty as character where numbers are.   
    check10 <- matrix(NA, nrow = 1, ncol = 2)  
    check10[,1] <- "Quality Check 10: Is there any unexpected number in Column Received From?"
    
    for(i in 1:nrow(df_msd)){
      if(is.na(df_msd$Units_Received[i]) == FALSE & is.character(df_msd$Units_Received[i]) == TRUE){
        df_msd$Units_Received2[i] <- as.numeric(0)
      }
      if(is.na(df_msd$Units_Received[i]) == TRUE){
        df_msd$Units_Received2[i] <- as.numeric(0)
      }
      else{
        df_msd$Units_Received2[i] <- as.numeric(df_msd$Units_Received[i])
      }
    }
    
    for(i in 1:nrow(df_msd)){
      if(is.na(df_msd$Received_From[i]) == FALSE & grepl("[[:digit:]]", df_msd$Received_From[i]) == TRUE){
        check10[,2] <- "Error! Please check in column Received From if there is number which should actually be assigned to column Units Received or Units Released."
      }
      # If there is change-indicating input for Received_From but not for Units_Received
      if((is.na(df_msd$Received_From[i]) == FALSE & df_msd$Received_From[i] != "NA") & ((as.numeric(df_msd[i, 'Units_Received']) == 0 ) | is.na(df_msd[i, 'Units_Received']) == TRUE)){ # CHANGES WERE MADE WITH strsplit to account for wrongfully entered numbers
        check10[,2] <- "Error! Please check columns Units Received and Received From for inconsistencies."
      }
      # If there is no input for Received_From but change-indicating for Units_Received
      if((is.na(df_msd$Received_From[i]) == TRUE | df_msd$Received_From[i] == "NA") & ((as.numeric(df_msd[i, 'Units_Received']) > 0) & is.na(df_msd[i, 'Units_Received']) == FALSE)){ # CHANGES WERE MADE WITH strsplit to account for wrongfully entered numbers
        check10[,2] <- "Error! Please check columns Units Received and Received From for inconsistencies."
      }
    }
    
    
    
    if(is.na(check10[,2]) == TRUE){
      check10[,2] <- "Check. No inconsistencies."
    }
    
    df_msd <- df_msd %>% dplyr::select(., -ends_with("Units_Received")) %>% dplyr::rename(., Units_Received = Units_Received2)
    
    
    #### Rename column values, add prefix ####
    # Where "Product" is not in the prefix, add it
    old_col_names <- unique(colnames(df_msd))
    index <- 0
    for(j in unique(colnames(df_msd))){
      index <- index + 1
      if(!startsWith(j, "Product")){
        colnames(df_msd)[index] <- paste0("Product_", j)
      }
    }
    
    
    
    #### Change order of columns to match v2021 version ####
    df_msd <- subset(df_msd, select=c(Product, Product_Entry_Spreadsheet, Product_Entry_Date, Product_Balance, Product_Units_Received, Product_Received_From, Product_Units_Released,
                                      Product_Released_To, Product_Patient_Name, Product_Patient_ID, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                      Product_No, Product_Balance_Status, Product_Units_Released_perproduct, Product_Units_Extra_KeptinClinic)) # CHANGE WAS MADED HERE; The column name "Product_Units_Extra_KeptinClinic" was added
    
    #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
    df_msd <- df_msd %>% 
      mutate(
        across(c(Product_Units_Received, Product_Units_Released, Product_Units_Returned), ~replace_na(.x, 0))
      )
    
    
  }
  #### Tracker version 2017|09 ####
  
  if(year == "2017" & grepl("Sep", tab)){ # TO DO: CONTINUE HERE
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
    
    df_msd$Product_Entry_Spreadsheet <- date
    
    
    #### Add variables country and hospital #####
    df_msd$Product_Country <- country    
    df_msd$Product_Hospital <- hospital    
    
    #### Add variable table_month and table_year ####    
    latest_date <- as.character(max(df_msd$Entry_Date, na.rm = TRUE))
    year <- as.numeric(strsplit(latest_date, "-")[[1]][1]) ; month <- as.character(strsplit(latest_date, "-")[[1]][2])
    
    df_msd$table_month <- month
    df_msd$table_year <- year
    
    #### Add variable "Patient_Name" and "Product_Patient_ID" # CHANGE WAS MADE HERE, THIS WAS ADDED #####
    
    df_msd$Product_Patient_Name <- NA
    df_msd$Product_Patient_Name <- as.character(df_msd$Released_To)
    df_msd$Product_Patient_ID <- df_msd$Product_Patient_Name 
    
    # column_patname <- select(df_msd, matches("patient"))
    # column_patname <- colnames(df_msd[,grepl("patient", colnames(df_msd))])
    # 
    # if(length(column_patname) > 0){ # Keep first column which contains "patient" values
    #   df_msd$Patient_Name <- df_msd[, column_patname[1]]
    # }
    
    
    #### Extend Product Name #####
    df_msd <- df_msd %>% fill(Product)
    
    #### Extend Entry_Date ####
    df_msd <- df_msd %>% fill(Entry_Date)
    
    #### Add additional column Product No #####
    df_msd <- transform(df_msd,Product_No=as.numeric(factor(Product)))
    
    #### Remove rows without changes in distribution ####
    
    delete_rows <- as.vector(NA)
    for(i in 1:nrow(df_msd)){
      if((is.na(df_msd$Units_Received[i]) == TRUE) & (is.na(df_msd$Units_Released[i]) == TRUE)){
        delete_rows[length(delete_rows)+1] <- i
      }
    }
    if(length(delete_rows) > 0 & is.na(delete_rows) == FALSE){ 
      df_msd <- df_msd[-delete_rows,] 
    }
    
    #### Add additional column Balance_Status ####
    df_msd$Balance_Status <- "change"
    for(j in unique(df_msd$Product)){
      print(j)
      # Define start
      df_msd[(df_msd$Units_Received == max(df_msd$Units_Received[df_msd$Product == j], na.rm = TRUE)) & (is.na(df_msd$Released_To) == TRUE | df_msd$Released_To == "NA") & (df_msd$Product == j) & (df_msd$Entry_Date == min(df_msd$Entry_Date[df_msd$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "start"
      # Define end
      df_msd[ (df_msd$Product == j) & (df_msd$Released_To == tail(df_msd[df_msd$Product == j, 'Released_To'], n = 1)), 'Balance_Status'] <- "end"
    }
    
    
    #### Add additional column Units_Released_perproduct ####
    df_msd$Product_Units_Released_perproduct <- NA
    for(j in unique(df_msd$Product)){
      df_sub <- df_msd[df_msd$Product == j,]
      units <- sum(as.numeric(df_sub$Units_Released), na.rm = TRUE)
      df_msd[df_msd$Product == j, 'Product_Units_Released_perproduct'] <- units
    }
    
    
    
    #### Add additional column Product_Balance, which equals Product_Balance ####
    check9 <- matrix(NA, nrow = 1, ncol = 2) ; check9[,1] <- "Quality Check 9: Is there any accidental overlap of a START balance event and a CHANGE balance event?"
    check9[,2] <- "Note. No Balance data is available for this spreadsheet version."
    
    df_msd$Product_Balance <- df_msd$Units_Extra_KeptinClinic
    
    # for(i in 1:nrow(df_msd)){
    #   for(j in unique(df_msd$Product)){
    #     start_value <- as.numeric(head(df_msd$Units_Received[df_msd$Product == j], n = 1))
    #     
    #     if(as.numeric(df_msd[i, 'Units_Received']) == start_value){ # CHANGE WAS MADE HERE; TYPE OF INPUT WAS CHANGED TO as.numeric()
    #       df_msd[i, 'Product_Balance'] <- start_value 
    #       new_value <- start_value
    #     } else{
    #       df_msd[i, 'Product_Balance'] <- new_value + na.omit(as.numeric(df_msd[i, 'Units_Received'])) -  na.omit(as.numeric(df_msd[i, 'Units_Released']))
    #     }
    #     
    
    
    
    #### Recode column values: Units_Received and Received_From (incl. Check10) #####
    
    # 1.  Units_Received should contain numbers, fill with 0s where characters are the moment
    # 2.  Received_From should be name, not numeric. Fill with empty as character where numbers are.   
    check10 <- matrix(NA, nrow = 1, ncol = 2)  
    check10[,1] <- "Quality Check 10: Is there any unexpected number in Column Received From?"
    df_msd$Units_Received2 <- NA
    
    for(i in 1:nrow(df_msd)){
      if(is.na(df_msd$Units_Received[i]) == FALSE & (grepl("[[:lower:]]", df_msd$Units_Received[i]) == TRUE | grepl("[[:upper:]]", df_msd$Units_Received[i]) == TRUE)){
        df_msd$Units_Received2[i] <- as.numeric(0)
      }
      if(is.na(df_msd$Units_Received[i]) == TRUE){
        df_msd$Units_Received2[i] <- as.numeric(0)
      } else{
        df_msd$Units_Received2[i] <- as.numeric(df_msd$Units_Received[i])
      }
    }
    
    for(i in 1:nrow(df_msd)){
      if(is.na(df_msd$Received_From[i]) == FALSE & grepl("[[:digit:]]", df_msd$Received_From[i]) == TRUE & grepl("[[:upper:]]", df_msd$Received_From[i]) == FALSE){
        check10[,2] <- "Error! Please check in column Received From if there is number which should actually be assigned to column Units Received or Units Released."
        print(i)
      }
      # If there is change-indicating input for Received_From but not for Units_Received
      if((is.na(df_msd$Received_From[i]) == FALSE & df_msd$Received_From[i] != "NA") & (as.numeric(df_msd[i, 'Units_Received']) == 0 | is.na(df_msd[i, 'Units_Received']) == TRUE)){
        check10[,2] <- "Error! Please check columns Units Received and Received From for inconsistencies."
      }
      # If there is no input for Received_From but change-indicating for Units_Received
      if((is.na(df_msd$Received_From[i]) == TRUE | df_msd$Received_From[i] == "NA") & (as.numeric(df_msd[i, 'Units_Received']) > 0 & is.na(df_msd[i, 'Units_Received']) == FALSE)){
        check10[,2] <- "Error! Please check columns Units Received and Received From for inconsistencies."
      }
    }
    
    
    if(is.na(check10[,2]) == TRUE){
      check10[,2] <- "Check. No inconsistencies."
    }
    
    df_msd <- df_msd %>% dplyr::select(., -ends_with("Units_Received")) %>% dplyr::rename(., Units_Received = Units_Received2)
    
    #### Add missing columns (empty) ####
    df_msd$Returned_By <- NA
    
    #### Rename column values, add prefix ####
    # Where "Product" is not in the prefix, add it
    old_col_names <- unique(colnames(df_msd))
    index <- 0
    for(j in unique(colnames(df_msd))){
      index <- index + 1
      if(!startsWith(j, "Product")){
        colnames(df_msd)[index] <- paste0("Product_", j)
      }
    }
    
    
    #### Change order of columns to match v2021 version ####
    df_msd <- subset(df_msd, select=c(Product, Product_Entry_Date, Product_Balance, Product_Units_Received, Product_Received_From, Product_Units_Released,
                                      Product_Released_To, Product_Patient_ID, Product_Patient_Name, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                      Product_No, Product_Balance_Status, Product_Units_Released_perproduct, Product_Units_Extra_KeptinClinic, Product_Entry_Spreadsheet)) # CHANGE WAS MADED HERE; The column name "Product_Units_Extra_KeptinClinic" was added
    
    #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
    df_msd <- df_msd %>% 
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
  feedback_file_name <- paste0(path_output, version_year, '_', country, '_', hospital, '_',
                               unique(df_msd$Product_table_year), '_QualityCheck.xlsx')
  addWorksheet(wb, tab)
  writeData(wb, tab, df_feedback )
  
  # Save preprocessed data file to final datafile
  df_msd$Product_sheet_name <- tab
  #df_final <- rbind(df_final, df_msd)
  if(tab == list_excel_tabs[1]){
    # df_final <- full_join(df_final, df_msd)
    df_final <- df_msd
  } else{
    df_final <- rbind(df_final, df_msd)
  }
  
  
}

# Save final feedback
saveWorkbook(wb, paste0(path_output, unique(df_final$Product_table_year), '_', country, '_', hospital, '_QualityCheck.xlsx'))

# Save final dataframe containing all tab sheet data 
col_order <- c("Product_Country", "Product_Hospital", "Product_Entry_Spreadsheet", "Product_table_year", "Product_table_month", "Product", "Product_Entry_Date", "Product_No", "Product_Balance", "Product_Balance_Status", "Product_Units_Received",
               "Product_Received_From", "Product_Units_Released", "Product_Patient_Name", "Product_Units_Returned",
               "Product_Returned_By", "Product_Units_Released_perproduct") # CHANGE WAS MADE HERE: COLUMNS "Product_Released_To" and "Product_Patient_ID" were dropped
df_final <- df_final %>% dplyr::select(., col_order)
writexl::write_xlsx(df_final, 
                    paste0(path_output, unique(df_final$Product_table_year), '_', country, '_', hospital, '.xlsx'))


#### Add data from final data file to overall dataframe #####

# df_full <- full_join(df_full, df_final)


#### End ####


