---------------- # Datacross - A4D Project: Phase 1 ------------- #
---------------- # Preprocessing: Medical Supply Data (MSD) ----- #
---------------- # Script version: 1.0 -------------------------- #   

---------------- # Clear working space and load libraries ------- ####  
# Clear working space #
rm(list = ls())
# Load libraries #
library(tidyverse)
library(tidyxl)

---------------- # DEFINE necessary input ----------------------- ####  
  
  # Information: Define everything in this section that is coloured and in quotes

  # Define month and year to which data refers
  month <- "08" # Define number for month, e.g., "08" denotes august
  year <- "2021" # Define number for year, e.g. "2021"
  path_import <- "/Users/skuhn/Documents/Privat/DataCross/Projects/Action4Diabetes/Spreadsheet_Versions/20210711_RawbyTyla/" # Define path from which data is loaded into this script
  path_output <- "/Users/skuhn/Documents/Privat/DataCross/Projects/Action4Diabetes/Preprocessing/Output_1/" # Define path where preprocessed data file shall be stored
  hospital <- "HHH" # Define name (abbreviation) of hospital
  country <- "CCC" # Define name of country
  
  df_alt1 <- readxl::read_xlsx(paste0(path_import, '2021 Tracker Template.xlsx'), sheet = "Jan'21") # Define file name AND tab name of data set that shall be read in
  

  
---------------- # Start Script (do not change) ----------------------- ####  

#### Version independent preprocessing #####
  #### Define functions #####
  
  `%notin%` <- Negate(`%in%`)
  
  
  
  #### Identify font styles (for patient summary section) #####
  
  # 1. Add another empty row to top of dataframe to match xlsx cell counting
  df_alt2 <- df_alt1 %>% add_row(., ...1 = NA, .before = 1)
  
  # 2. Get dataframe with local_format_id per cell
  x <- xlsx_cells("/Users/skuhn/Documents/Privat/DataCross/Projects/Action4Diabetes/Spreadsheet_Versions/20210711_RawbyTyla/2021 Tracker Template.xlsx")
  
  # 3. Get format per local_format_id
  formats <- xlsx_formats("/Users/skuhn/Documents/Privat/DataCross/Projects/Action4Diabetes/Spreadsheet_Versions/20210711_RawbyTyla/2021 Tracker Template.xlsx")
  # Presence of bold font by local_format_id
  formats_bold <- formats$local$font$bold
  # Background fill colours by local_format_id
  formats_color <- formats$local$fill$patternFill$fgColor$rgb # position in vector equals the local_format_id
  # Text font colours by local_format_id
  formats$local$font$color$rgb
  
  # unique text colours
  unique(formats$local$font$color$rgb) # quick check for hex8 codes via https://www.rapidtables.com/convert/color/hex-to-rgb.html
  
  
  # 4. Attach column "local_format_id" from x to each corresponding cell in df_alt2
  
  # 5. Attach value from "formats_color" to each corresponding cell in df_alt2 (based on local_format_id)
  
  
  
  
  
  
  
  
#### Start preprocessing depending on year ####    
if(year == "2021"){
  #### Split dataset into two parts #####
  
  #### Identify splits ######
  # Get health supply part: 
  # Split when row "MEDICAL SUPPLIES DISTRIBUTION" is followed by row with columns "Product", "Entry Date", "Balance"
  # Get patient recruitments summary part
  # Split when row "PATIENT RECRUITMENT SUMMARY" is followed by rows with columns "Patient ID", "Patient Name", "Province"
  
  for(i in 1:nrow(df)){
    
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
  
  
  #### Define sub dataframes #####
  #### For medical supplies distribution (MSD) #####
  
  df_msd <- df_alt1[start_df_msd:end_df_msd, ]
  colnames(df_msd) <- df_msd[1,] ; df_msd <- df_msd[-1,] # Redefine columns
  df_msd <- df_msd %>% dplyr::select(    # Define columns to be kept
    c("Product", "Entry Date", "Balance", "Units Received", "Received From", "Units Released", "Released To \r\n(Drop Down List)", "Units Returned", "Returned By")
  )  %>%
    rename(Entry_Date = `Entry Date`,
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
  
  #### For patient summary ######
  
  df_patient <- df_alt1[start_df_patient:end_df_patient, ]
  colnames(df_patient) <- df_patient[1,] ; df_patient <- df_patient[-1,] # Redefine columns
  
  
  
  #### Define country #####
  
  # Extract from Patient ID first part (should be country)
  for(i in 1:nrow(df_patient)){
    df_patient$country[i] <- as.character(strsplit(df_patient$`Patient ID`, "_")[[i]][[1]])
  }
  
  # Get most common value
  df_patient$country <- unlist(df_patient$country) 
  country <- names(sort(table(df_patient$country),decreasing=TRUE)[1])
  
  
  #### MSD: Adjust dataframe #####
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
    
    #### Extract Patient Identifier and add to Released_To #####
    df_patient_id <- df_patient %>%
      dplyr::select(`Patient Name`, `Patient ID`)
    df_msd <- dplyr::left_join(df_msd, df_patient_id, by = c("Released_To" = "Patient Name")) 
    df_msd$Released_To <- df_msd$`Patient ID`
    df_msd <- df_msd %>% select(., -matches("Patient ID"))
    
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
                                      Product_Released_To, Product_Patient_Name, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                      Product_No, Product_Balance_Status, Product_Units_Released_perproduct))
    
    
  #### MSD: Run version-dependent quality check #####
  
    #### Check3: Redundancy in rows #####
    # Test if any row is duplicated
    check3 <- matrix(NA, nrow = 1, ncol = 2) 
    check3[,1] <- "Quality Check 3: Are there duplicated rows?"
    
    df_msd_old <- df_msd
    df_msd <- df_msd %>% distinct()
    if(nrow(df_msd) < nrow(df_msd_old)){
      check3[,2] <- paste0('Error! ', 'Duplicated rows were found. Please check the rows in the input spreadsheet for duplicate rows.')
    } else{
      check3[,2] <- paste0('Check. ', 'No duplicated rows were found.')
    }
    
    
    #### Check4: Test classes and adjust were necessary #####
    check4 <- matrix(NA, nrow = 1, ncol = 2) 
    check4[,1] <- "Quality Check 4: Are there errors in the type (class) of data provided?"
    
    classes <- lapply(df_msd, class)
    classes_aim_name <- c("Product", "Product_Entry_Date", "Product_Balance", "Product_Units_Received", "Product_Received_From", "Product_Units_Released", "Product_Released_To",
                          "Product_Units_Returned", "Product_Returned_By", "Product_Patient_ID", "Product_No", "Product_Balance_Status", "Product_Units_Released_perproduct",
                          "Product_Country", "Product_table_month", "Product_table_year", "Product_Patient_ID", "Product_No", "Product_Balance_Status", "Product_Units_Released_perproduct",
                          "Product_Hospital")
    
    classes_aim_type <- c("character", "POSIXct", "numeric", "numeric", "character", "numeric", "character", "numeric", "character", "character", "numeric", "character",
                          "numeric", "character", "character", "numeric", "character", "numeric", "character", "numeric", "character")
    classes_aim <- data.frame(classes_aim_name, classes_aim_type) # Ideal classes
    
    for(j in unique(names(classes))){
      actual_name <- names(classes[j])
      actual_class <- classes[j][[1]][1]
      aim_class <- classes_aim[classes_aim$classes_aim_name == j, "classes_aim_type"]
      # If the class type matches
      if(class(df_msd[, actual_name])[1] == aim_class ) {
        print(j)
        print("match")
        
      } else{ # If the class type does not match, change type
        
        print(j)
        print("no match")
        
        if(aim_class == "character"){
          df_msd[, j] <- as.character( df_msd[, j])
        }
        if(aim_class == "numeric"){
          df_msd[, j] <- as.numeric( df_msd[, j])
        }
        if(aim_class == "POSIXct"){
          df_msd[, j] <- as.POSIXct.Date()( df_msd[, j])
        }
      }
    }
    
    # Return Output if any value is not compatible with aim class
    container <- list() ; index <- 0
    for(k in unique(names(lapply(df_msd2, class)))){
      actual_name <- names(lapply(df_msd2, class)[k])
      actual_class <- lapply(df_msd2, class)[k][[1]][1]
      aim_class <- classes_aim[classes_aim$classes_aim_name == k, "classes_aim_type"]
      # If the class type matches
      if(class(df_msd2[, actual_name])[1] != aim_class ) {
        index <- index + 1
        container[index] <- k
        
      }
    }
    
    if(length(container) > 0){
      check4[,2] <- paste0('Error! ', 'The following columns have an unexpected class of data: ', container)
    } else{
      check4[,2] <- paste0('Check. ', 'No errors in classes of data.')
    }
    
    
    
    
    #### Check5-7: Test if variable columns Product_Entry_Date, Product_Released_to (Patient name) and Patient_ID contains all relevant information #####
    check5 <- matrix(NA, nrow = 1, ncol = 2) ; check6 <- matrix(NA, nrow = 1, ncol = 2) ; check7 <- matrix(NA, nrow = 1, ncol = 2) 
    check5[,1] <- "Quality Check 5: Are there unexpected values in the column Entry_Date?"
    check7[,1] <- "Quality Check 7: Is the patient ID saved as expected?"
    
    
    # Check5: Product_Entry_Date, e.g. "2020-12-31 GMT", Quality Check 5
    list1 <- strsplit(as.character(df_msd$Product_Entry_Date),'-')
    years <- lapply(list1, `[[`, 1)
    years_unique <- sort(unique(c(as.numeric(unique(years)), as.numeric(unique(years))+1, as.numeric(unique(years))-1))) # year dates +- 1
    
    months <- as.numeric(lapply(list1, `[[`, 2))
    days <- as.numeric(lapply(list1, `[[`, 3))
    
    if((any(years %notin% years_unique) | (any(months > 12 )) |  (any(months < 1 )) | (any(days > 31 )) |  (any(months < 1 )))){
      check5[,2] <- paste0('Error! Please check the column Entry Date for unexpected values out of range.')
    } else{
      check5[,2] <- paste0('Check. No unexpected values in the column Entry_Date.')
      
      # print('Quality Check 5: Are there unexpected values in the column Entry_Date?')
      #print('Check. : No unexpected values in the column Entry_Date.')
      
    }
    
    # Check6: Test if Product_Released_to (Patient Name) is presend and contains all elements
    df_sub2 <- df_msd %>% filter((is.na(Product_Units_Released) == FALSE) & Product_Balance_Status == "change")
    patient_names <- lapply(df_sub2$Product_Released_To, strsplit, " ")
    patient_names_1 <- lapply(patient_names, `[[`, 1) ; patient_names_num <- as.numeric(lapply(patient_names_1, `[`, 2)) ; patient_num_prefix <- as.character(lapply(patient_names_1, `[`, 1))
    
    # Test for missing patient name or inconsistent patient name
    if(any(is.na(df_sub2$Product_Released_To) == TRUE)){
      check6[,1] <- "Quality Check 6a: Is the patient name saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient name is missing.')
    }  
    if(any(patient_names_num != as.numeric())){
      check6[,1] <- "'Quality Check 6b: Is the patient name saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient number is missing.')
    }
    if(any(patient_num_prefix != "Patient")){
      check6[,1] <- "Quality Check 6c: Is the patient name prefix saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient name prefix is missing.')
    }
    if(! ( (any(is.na(df_sub2$Product_Released_To) == TRUE)) | (any(patient_names_num != as.numeric())) | (any(patient_num_prefix != "Patient")) )){
      check6[,1] <- "Quality Check 6: Is the patient name prefix saved as expected?"
      check6[,2] <- paste0('Check. No inconsistencies.')
      
    }
    
    # Check7: Check if each Unit-receiver has a patient_ID
    if(any(is.na(df_sub2$Product_Patient_ID)) == TRUE){
      check7[,2] <- paste0('Error! Please check if all patients who have received units are documented in Patient summary.')
    } else{
      check7[,2] <- paste0('Check. No inconsistencies.')
      
    }
    
    
    
    #### Check8: Test if computation of Balance is correct #####
    check8 <- matrix(NA, nrow = 1, ncol = 2)
    check8[,1] <- "Quality Check 8: Has the column Product_Units_Released been computed correctly?"
    
    balance_inconsist <- 0    
    for(k in unique(df_msd$Product)){
      df_sub <- df_msd %>% dplyr::filter(Product == k)
      difference_entries <- as.numeric(max(df_sub$Product_Balance)) - as.numeric(min(df_sub$Product_Balance))
      if(difference_entries != as.numeric(unique(df_sub$Product_Units_Released_perproduct))){
        balance_inconsist <- balance_inconsist + 1
      }
    } 
    
    if(balance_inconsist > 0){
      check8[,2] <- paste0('Error! Please compare the columns Balance and Product_Units_Received, there are inconsistencies in ', balance_inconsist, ' products.')
      #print('Quality Check 8: Has the column Product_Units_Released been computed correctly?')
      #print(paste0('Error! : Please compare the columns Balance and Product_Units_Received, there are inconsistencies in ', balance_inconsist, ' products.'))
    } else{
      check8[,2] <- paste0('Check. No inconsistencies.')
      #print('Quality Check 8: Has the column Product_Units_Released been computed correctly?')
      #print(paste0('Check. : No inconsistencies.'))
    }   
    
  #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
  df_msd <- df_msd %>% 
    mutate(
      across(c(Product_Units_Received, Product_Units_Released, Product_Units_Returned), ~replace_na(.x, 0))
    )
  
}  
if(year = "2020"){
  #### Split dataset into two parts #####
  
  #### Identify splits ######
  # Get health supply part: 
  # Split when row contains columns with variables "Product", "Date", "Units_Received"
  # Get patient recruitments summary part
  # Split when row "Summary of Patient Recruitment" is followed by row with columns "No", "Patient Name", "Province"
  
  for(i in 1:nrow(df)){
    
    if(any(grepl("Product", df_alt1[i, ])) &
       any(grepl("Date", df_alt1[i, ])) & any(grepl("Units Received", df_alt1[i, ]))){
      
      start_df_msd <- i+1
    }
    
    if(any(grepl("Summary of Patient Recruitment", df_alt1[i, ])) &
       any(grepl("No", df_alt1[i+1, ])) & any(grepl("Patient Name", df_alt1[i+1, ]))){
      
      end_df_msd <- i-1
      start_df_patient <- i+1
      end_df_patient <- nrow(df_alt1)
    }
    
  }
  
  
  #### Define sub dataframes #####
  #### For medical supplies distribution (MSD) #####
  
  df_msd <- df_alt1[(start_df_msd-1):end_df_msd, ]
  colnames(df_msd) <- df_msd[1,] ; df_msd <- df_msd[-1,] # Redefine columns
  df_msd <- df_msd %>% dplyr::select(    # Define columns to be kept
    c("Product", "Date", "Units Received", "Received From", "Units Released", "Released To (select from drop down list)", "Units Returned", "Returned By")
  )  %>%
    rename(Entry_Date = `Date`,
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
    as.POSIXct( df_msd[["Entry_Date"]] * (60*60*24)
                , origin="1899-12-30"
                , tz="GMT")
  
  #### For patient summary ######
  
  df_patient <- df_alt1[start_df_patient:end_df_patient, ]
  colnames(df_patient) <- df_patient[1,] ; df_patient <- df_patient[-1,] # Redefine columns
  
  
  
  #### MSD: Adjust dataframe #####
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
    df_msd[(df_msd$Received_From == max(df_msd$Received_From[df_msd$Product == j], na.rm = TRUE)) & (is.na(df_msd$Units_Released) == TRUE) & (df_msd$Product == j) & (df_msd$Entry_Date == min(df_msd$Entry_Date[df_msd$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "start"
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
  check9 <- matrix(NA, nrow = 1, ncol = 2) ; check9[,1] <- "Quality Check 9: Is there any accidental overlap of a START balance event and a CHANGE balance event?"
  
  df_msd$Product_Balance <- NA ; 
  
  for(j in unique(df_msd$Product)){
    df_sub <- df_msd[df_msd$Product == j,]
    for(i in 1:nrow(df_sub)){
      value <- as.numeric(na.omit(c(df_sub[i, 'Received_From'], df_sub[i, 'Units_Released'])))
      
      # Qualitycheck 9: If there is both a start or end value and aa change value, yield error
      if(length(value) > 1){
        check9[,2] <- paste0("Error! Please check the balance rows of product ", j, ' and make sure that there are no overlaps between start and end events.')
      } else{
        check9[,2] <- paste0("Check. No inconsistencies.")
      }
    }
  }
  
  
  for(i in 1:nrow(df_msd)){
    for(j in unique(df_msd$Product)){
      
      # 
      if(df_msd$Balance_Status[i] == "start"){
        df_msd[i, 'Product_Balance'] <- df_msd[i, 'Received_From'] 
        start_value <- df_msd[i, 'Received from']
      }
      if(df_msd$Balance_Status[i] == "change"){
        df_msd[i, 'Product_Balance'] <- as.numeric(df_msd[i-1, 'Product_Balance']) -  as.numeric(df_msd[i, 'Units_Released']) 
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
                                    Product_Released_To, Product_Patient_Name, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                    Product_No, Product_Balance_Status, Product_Units_Released_perproduct))
  
  
  #### MSD: Run version-dependent quality checks #####
  
    #### Check3: Redundancy in rows #####
    # Test if any row is duplicated
    check3 <- matrix(NA, nrow = 1, ncol = 2) 
    check3[,1] <- "Quality Check 3: Are there duplicated rows?"
    
    df_msd_old <- df_msd
    df_msd <- df_msd %>% distinct()
    if(nrow(df_msd) < nrow(df_msd_old)){
      check3[,2] <- paste0('Error! ', 'Duplicated rows were found. Please check the rows in the input spreadsheet for duplicate rows.')
    } else{
      check3[,2] <- paste0('Check. ', 'No duplicated rows were found.')
    }
    
    
    #### Check4: Test classes and adjust were necessary #####
    check4 <- matrix(NA, nrow = 1, ncol = 2) 
    check4[,1] <- "Quality Check 4: Are there errors in the type (class) of data provided?"
    
    classes <- lapply(df_msd, class)
    classes_aim_name <- c("Product", "Product_Entry_Date", "Product_Balance", "Product_Units_Received", "Product_Received_From", "Product_Units_Released", "Product_Released_To",
                          "Product_Units_Returned", "Product_Returned_By", "Product_Patient_ID", "Product_No", "Product_Balance_Status", "Product_Units_Released_perproduct",
                          "Product_Country", "Product_table_month", "Product_table_year", "Product_Patient_ID", "Product_No", "Product_Balance_Status", "Product_Units_Released_perproduct",
                          "Product_Hospital")
    
    classes_aim_type <- c("character", "POSIXct", "numeric", "numeric", "character", "numeric", "character", "numeric", "character", "character", "numeric", "character",
                          "numeric", "character", "character", "numeric", "character", "numeric", "character", "numeric", "character")
    classes_aim <- data.frame(classes_aim_name, classes_aim_type) # Ideal classes
    
    for(j in unique(names(classes))){
      actual_name <- names(classes[j])
      actual_class <- classes[j][[1]][1]
      aim_class <- classes_aim[classes_aim$classes_aim_name == j, "classes_aim_type"]
      # If the class type matches
      if(class(df_msd[, actual_name])[1] == aim_class ) {
        print(j)
        print("match")
        
      } else{ # If the class type does not match, change type
        
        print(j)
        print("no match")
        
        if(aim_class == "character"){
          df_msd[, j] <- as.character( df_msd[, j])
        }
        if(aim_class == "numeric"){
          df_msd[, j] <- as.numeric( df_msd[, j])
        }
        if(aim_class == "POSIXct"){
          df_msd[, j] <- as.POSIXct.Date()( df_msd[, j])
        }
      }
    }
    
    # Return Output if any value is not compatible with aim class
    container <- list() ; index <- 0
    for(k in unique(names(lapply(df_msd2, class)))){
      actual_name <- names(lapply(df_msd2, class)[k])
      actual_class <- lapply(df_msd2, class)[k][[1]][1]
      aim_class <- classes_aim[classes_aim$classes_aim_name == k, "classes_aim_type"]
      # If the class type matches
      if(class(df_msd2[, actual_name])[1] != aim_class ) {
        index <- index + 1
        container[index] <- k
        
      }
    }
    
    if(length(container) > 0){
      check4[,2] <- paste0('Error! ', 'The following columns have an unexpected class of data: ', container)
    } else{
      check4[,2] <- paste0('Check. ', 'No errors in classes of data.')
    }
    
    
    
    
    #### Check5-7: Test if variable columns Product_Entry_Date, Product_Released_to (Patient name) and Patient_ID contains all relevant information #####
    check5 <- matrix(NA, nrow = 1, ncol = 2) ; check6 <- matrix(NA, nrow = 1, ncol = 2) ; check7 <- matrix(NA, nrow = 1, ncol = 2) 
    check5[,1] <- "Quality Check 5: Are there unexpected values in the column Entry_Date?"
    check7[,1] <- "Quality Check 7: Is the patient ID saved as expected?"
    
    
    # Check5: Product_Entry_Date, e.g. "2020-12-31 GMT", Quality Check 5
    list1 <- strsplit(as.character(df_msd$Product_Entry_Date),'-')
    years <- lapply(list1, `[[`, 1)
    years_unique <- sort(unique(c(as.numeric(unique(years)), as.numeric(unique(years))+1, as.numeric(unique(years))-1))) # year dates +- 1
    
    months <- as.numeric(lapply(list1, `[[`, 2))
    days <- as.numeric(lapply(list1, `[[`, 3))
    
    if((any(years %notin% years_unique) | (any(months > 12 )) |  (any(months < 1 )) | (any(days > 31 )) |  (any(months < 1 )))){
      check5[,2] <- paste0('Error! Please check the column Entry Date for unexpected values out of range.')
    } else{
      check5[,2] <- paste0('Check. No unexpected values in the column Entry_Date.')
      
      # print('Quality Check 5: Are there unexpected values in the column Entry_Date?')
      #print('Check. : No unexpected values in the column Entry_Date.')
      
    }
    
    # Check6: Test if Product_Released_to (Patient Name) is presend and contains all elements
    df_sub2 <- df_msd %>% filter((is.na(Product_Units_Released) == FALSE) & Product_Balance_Status == "change")
    patient_names <- lapply(df_sub2$Product_Released_To, strsplit, " ")
    patient_names_1 <- lapply(patient_names, `[[`, 1) ; patient_names_num <- as.numeric(lapply(patient_names_1, `[`, 2)) ; patient_num_prefix <- as.character(lapply(patient_names_1, `[`, 1))
    
    # Test for missing patient name or inconsistent patient name
    if(any(is.na(df_sub2$Product_Released_To) == TRUE)){
      check6[,1] <- "Quality Check 6a: Is the patient name saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient name is missing.')
    }  
    if(any(patient_names_num != as.numeric())){
      check6[,1] <- "'Quality Check 6b: Is the patient name saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient number is missing.')
    }
    if(any(patient_num_prefix != "Patient")){
      check6[,1] <- "Quality Check 6c: Is the patient name prefix saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient name prefix is missing.')
    }
    if(! ( (any(is.na(df_sub2$Product_Released_To) == TRUE)) | (any(patient_names_num != as.numeric())) | (any(patient_num_prefix != "Patient")) )){
      check6[,1] <- "Quality Check 6: Is the patient name prefix saved as expected?"
      check6[,2] <- paste0('Check. No inconsistencies.')
      
    }
    
    # Check7: Check if each Unit-receiver has a patient_ID
    if(any(is.na(df_sub2$Product_Patient_ID)) == TRUE){
      check7[,2] <- paste0('Error! Please check if all patients who have received units are documented in Patient summary.')
    } else{
      check7[,2] <- paste0('Check. No inconsistencies.')
      
    }
    
    
    
    #### Check8: Test if computation of Balance is correct #####
    check8 <- matrix(NA, nrow = 1, ncol = 2)
    check8[,1] <- "Quality Check 8: Has the column Product_Units_Released been computed correctly?"
    
    balance_inconsist <- 0    
    for(k in unique(df_msd$Product)){
      df_sub <- df_msd %>% dplyr::filter(Product == k)
      difference_entries <- as.numeric(max(df_sub$Product_Balance)) - as.numeric(min(df_sub$Product_Balance))
      if(difference_entries != as.numeric(unique(df_sub$Product_Units_Released_perproduct))){
        balance_inconsist <- balance_inconsist + 1
      }
    } 
    
    if(balance_inconsist > 0){
      check8[,2] <- paste0('Error! Please compare the columns Balance and Product_Units_Received, there are inconsistencies in ', balance_inconsist, ' products.')
      #print('Quality Check 8: Has the column Product_Units_Released been computed correctly?')
      #print(paste0('Error! : Please compare the columns Balance and Product_Units_Received, there are inconsistencies in ', balance_inconsist, ' products.'))
    } else{
      check8[,2] <- paste0('Check. No inconsistencies.')
      #print('Quality Check 8: Has the column Product_Units_Released been computed correctly?')
      #print(paste0('Check. : No inconsistencies.'))
    }   
    
  #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
  df_msd <- df_msd %>% 
    mutate(
      across(c(Product_Units_Received, Product_Units_Released, Product_Units_Returned), ~replace_na(.x, 0))
    )
  
  
}  
if(year == "2019"){
  #### Split dataset into two parts #####
  
  #### Identify splits ######
  # Get health supply part: 
  # Split when row contains columns with variables "Product", "Date", "Units Received"
  # Get patient recruitments summary part
  # Split when row "Summary of Patient Recruitment" is followed by row with columns "No", "Patient Name", "Province"
  
  for(i in 1:nrow(df)){
    
    if(any(grepl("Product", df_alt1[i, ])) &
       any(grepl("Date", df_alt1[i, ])) & any(grepl("Units Received", df_alt1[i, ]))){
      
      start_df_msd <- i+1
    }
    
    if(any(grepl("Summary of Patient Recruitment", df_alt1[i, ])) &
       any(grepl("No", df_alt1[i+1, ])) & any(grepl("Patient Name", df_alt1[i+1, ]))){
      
      end_df_msd <- i-1
      start_df_patient <- i+1
      end_df_patient <- nrow(df_alt1)
    }
    
  }
  
  
  ####  Define sub dataframes #####
  #### For medical supplies distribution (MSD) #####
  
  df_msd <- df_alt1[(start_df_msd-1):end_df_msd, ]
  colnames(df_msd) <- df_msd[1,] ; df_msd <- df_msd[-1,] # Redefine columns
  df_msd <- df_msd %>% dplyr::select(    # Define columns to be kept
    c("Product", "Date", "Units Received", "Received From", "Units Released", "Released To (select from drop down list)", "Units Returned", "Returned By")
  )  %>%
    rename(Entry_Date = `Date`,
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
    as.POSIXct( df_msd[["Entry_Date"]] * (60*60*24)
                , origin="1899-12-30"
                , tz="GMT")
  
  #### For patient summary ######
  
  df_patient <- df_alt1[start_df_patient:end_df_patient, ]
  colnames(df_patient) <- df_patient[1,] ; df_patient <- df_patient[-1,] # Redefine columns
  
  
  
  #### MSD: Adjust dataframe #####
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
    df_msd[(df_msd$Received_From == max(df_msd$Received_From[df_msd$Product == j], na.rm = TRUE)) & (is.na(df_msd$Units_Released) == TRUE) & (df_msd$Product == j) & (df_msd$Entry_Date == min(df_msd$Entry_Date[df_msd$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "start"
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
  check9 <- matrix(NA, nrow = 1, ncol = 2) ; check9[,1] <- "Quality Check 9: Is there any accidental overlap of a START balance event and a CHANGE balance event?"
  
  df_msd$Product_Balance <- NA ; 
  
  for(j in unique(df_msd$Product)){
    df_sub <- df_msd[df_msd$Product == j,]
    for(i in 1:nrow(df_sub)){
      value <- as.numeric(na.omit(c(df_sub[i, 'Received_From'], df_sub[i, 'Units_Released'])))
      
      # Qualitycheck 9: If there is both a start or end value and aa change value, yield error
      if(length(value) > 1){
        check9[,2] <- paste0("Error! Please check the balance rows of product ", j, ' and make sure that there are no overlaps between start and end events.')
      } else{
        check9[,2] <- paste0("Check. No inconsistencies.")
      }
    }
  }
  
  
  for(i in 1:nrow(df_msd)){
    for(j in unique(df_msd$Product)){
      
      # 
      if(df_msd$Balance_Status[i] == "start"){
        df_msd[i, 'Product_Balance'] <- df_msd[i, 'Received_From'] 
        start_value <- df_msd[i, 'Received from']
      }
      if(df_msd$Balance_Status[i] == "change"){
        df_msd[i, 'Product_Balance'] <- as.numeric(df_msd[i-1, 'Product_Balance']) -  as.numeric(df_msd[i, 'Units_Released']) 
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
                                    Product_Released_To, Product_Patient_Name, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                    Product_No, Product_Balance_Status, Product_Units_Released_perproduct))
  
  #### MSD: Run version-dependent quality checks #####
  
    #### Check3: Redundancy in rows #####
    # Test if any row is duplicated
    check3 <- matrix(NA, nrow = 1, ncol = 2) 
    check3[,1] <- "Quality Check 3: Are there duplicated rows?"
    
    df_msd_old <- df_msd
    df_msd <- df_msd %>% distinct()
    if(nrow(df_msd) < nrow(df_msd_old)){
      check3[,2] <- paste0('Error! ', 'Duplicated rows were found. Please check the rows in the input spreadsheet for duplicate rows.')
    } else{
      check3[,2] <- paste0('Check. ', 'No duplicated rows were found.')
    }
    
    
    #### Check4: Test classes and adjust were necessary #####
    check4 <- matrix(NA, nrow = 1, ncol = 2) 
    check4[,1] <- "Quality Check 4: Are there errors in the type (class) of data provided?"
    
    classes <- lapply(df_msd, class)
    classes_aim_name <- c("Product", "Product_Entry_Date", "Product_Balance", "Product_Units_Received", "Product_Received_From", "Product_Units_Released", "Product_Released_To",
                          "Product_Units_Returned", "Product_Returned_By", "Product_Patient_ID", "Product_No", "Product_Balance_Status", "Product_Units_Released_perproduct",
                          "Product_Country", "Product_table_month", "Product_table_year", "Product_Patient_ID", "Product_No", "Product_Balance_Status", "Product_Units_Released_perproduct",
                          "Product_Hospital")
    
    classes_aim_type <- c("character", "POSIXct", "numeric", "numeric", "character", "numeric", "character", "numeric", "character", "character", "numeric", "character",
                          "numeric", "character", "character", "numeric", "character", "numeric", "character", "numeric", "character")
    classes_aim <- data.frame(classes_aim_name, classes_aim_type) # Ideal classes
    
    for(j in unique(names(classes))){
      actual_name <- names(classes[j])
      actual_class <- classes[j][[1]][1]
      aim_class <- classes_aim[classes_aim$classes_aim_name == j, "classes_aim_type"]
      # If the class type matches
      if(class(df_msd[, actual_name])[1] == aim_class ) {
        print(j)
        print("match")
        
      } else{ # If the class type does not match, change type
        
        print(j)
        print("no match")
        
        if(aim_class == "character"){
          df_msd[, j] <- as.character( df_msd[, j])
        }
        if(aim_class == "numeric"){
          df_msd[, j] <- as.numeric( df_msd[, j])
        }
        if(aim_class == "POSIXct"){
          df_msd[, j] <- as.POSIXct.Date()( df_msd[, j])
        }
      }
    }
    
    # Return Output if any value is not compatible with aim class
    container <- list() ; index <- 0
    for(k in unique(names(lapply(df_msd2, class)))){
      actual_name <- names(lapply(df_msd2, class)[k])
      actual_class <- lapply(df_msd2, class)[k][[1]][1]
      aim_class <- classes_aim[classes_aim$classes_aim_name == k, "classes_aim_type"]
      # If the class type matches
      if(class(df_msd2[, actual_name])[1] != aim_class ) {
        index <- index + 1
        container[index] <- k
        
      }
    }
    
    if(length(container) > 0){
      check4[,2] <- paste0('Error! ', 'The following columns have an unexpected class of data: ', container)
    } else{
      check4[,2] <- paste0('Check. ', 'No errors in classes of data.')
    }
    
    
    
    
    #### Check5-7: Test if variable columns Product_Entry_Date, Product_Released_to (Patient name) and Patient_ID contains all relevant information #####
    check5 <- matrix(NA, nrow = 1, ncol = 2) ; check6 <- matrix(NA, nrow = 1, ncol = 2) ; check7 <- matrix(NA, nrow = 1, ncol = 2) 
    check5[,1] <- "Quality Check 5: Are there unexpected values in the column Entry_Date?"
    check7[,1] <- "Quality Check 7: Is the patient ID saved as expected?"
    
    
    # Check5: Product_Entry_Date, e.g. "2020-12-31 GMT", Quality Check 5
    list1 <- strsplit(as.character(df_msd$Product_Entry_Date),'-')
    years <- lapply(list1, `[[`, 1)
    years_unique <- sort(unique(c(as.numeric(unique(years)), as.numeric(unique(years))+1, as.numeric(unique(years))-1))) # year dates +- 1
    
    months <- as.numeric(lapply(list1, `[[`, 2))
    days <- as.numeric(lapply(list1, `[[`, 3))
    
    if((any(years %notin% years_unique) | (any(months > 12 )) |  (any(months < 1 )) | (any(days > 31 )) |  (any(months < 1 )))){
      check5[,2] <- paste0('Error! Please check the column Entry Date for unexpected values out of range.')
    } else{
      check5[,2] <- paste0('Check. No unexpected values in the column Entry_Date.')
      
      # print('Quality Check 5: Are there unexpected values in the column Entry_Date?')
      #print('Check. : No unexpected values in the column Entry_Date.')
      
    }
    
    # Check6: Test if Product_Released_to (Patient Name) is presend and contains all elements
    df_sub2 <- df_msd %>% filter((is.na(Product_Units_Released) == FALSE) & Product_Balance_Status == "change")
    patient_names <- lapply(df_sub2$Product_Released_To, strsplit, " ")
    patient_names_1 <- lapply(patient_names, `[[`, 1) ; patient_names_num <- as.numeric(lapply(patient_names_1, `[`, 2)) ; patient_num_prefix <- as.character(lapply(patient_names_1, `[`, 1))
    
    # Test for missing patient name or inconsistent patient name
    if(any(is.na(df_sub2$Product_Released_To) == TRUE)){
      check6[,1] <- "Quality Check 6a: Is the patient name saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient name is missing.')
    }  
    if(any(patient_names_num != as.numeric())){
      check6[,1] <- "'Quality Check 6b: Is the patient name saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient number is missing.')
    }
    if(any(patient_num_prefix != "Patient")){
      check6[,1] <- "Quality Check 6c: Is the patient name prefix saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient name prefix is missing.')
    }
    if(! ( (any(is.na(df_sub2$Product_Released_To) == TRUE)) | (any(patient_names_num != as.numeric())) | (any(patient_num_prefix != "Patient")) )){
      check6[,1] <- "Quality Check 6: Is the patient name prefix saved as expected?"
      check6[,2] <- paste0('Check. No inconsistencies.')
      
    }
    
    # Check7: Check if each Unit-receiver has a patient_ID
    if(any(is.na(df_sub2$Product_Patient_ID)) == TRUE){
      check7[,2] <- paste0('Error! Please check if all patients who have received units are documented in Patient summary.')
    } else{
      check7[,2] <- paste0('Check. No inconsistencies.')
      
    }
    
    
    
    #### Check8: Test if computation of Balance is correct #####
    check8 <- matrix(NA, nrow = 1, ncol = 2)
    check8[,1] <- "Quality Check 8: Has the column Product_Units_Released been computed correctly?"
    
    balance_inconsist <- 0    
    for(k in unique(df_msd$Product)){
      df_sub <- df_msd %>% dplyr::filter(Product == k)
      difference_entries <- as.numeric(max(df_sub$Product_Balance)) - as.numeric(min(df_sub$Product_Balance))
      if(difference_entries != as.numeric(unique(df_sub$Product_Units_Released_perproduct))){
        balance_inconsist <- balance_inconsist + 1
      }
    } 
    
    if(balance_inconsist > 0){
      check8[,2] <- paste0('Error! Please compare the columns Balance and Product_Units_Received, there are inconsistencies in ', balance_inconsist, ' products.')
      #print('Quality Check 8: Has the column Product_Units_Released been computed correctly?')
      #print(paste0('Error! : Please compare the columns Balance and Product_Units_Received, there are inconsistencies in ', balance_inconsist, ' products.'))
    } else{
      check8[,2] <- paste0('Check. No inconsistencies.')
      #print('Quality Check 8: Has the column Product_Units_Released been computed correctly?')
      #print(paste0('Check. : No inconsistencies.'))
    }   
    
  #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
  df_msd <- df_msd %>% 
    mutate(
      across(c(Product_Units_Received, Product_Units_Released, Product_Units_Returned), ~replace_na(.x, 0))
    )
  
  
}  
if(year = "2018"){
  #### Split dataset into two parts #####
  
  #### Identify splits ######
  # Get health supply part: 
  # Split when row contains columns with variables "Product", "Date", "Units Received"
  # Get patient recruitments summary part
  # Split when row "Summary of Patient Recruitment" is followed by row with columns "No", "Patient Name", "Province"
  
  for(i in 1:nrow(df)){
    
    if(any(grepl("Product", df_alt1[i, ])) &
       any(grepl("Date", df_alt1[i, ])) & any(grepl("Units Received", df_alt1[i, ]))){
      
      start_df_msd <- i+1
    }
    
    if(any(grepl("Summary of Patient Recruitment", df_alt1[i, ])) &
       any(grepl("No", df_alt1[i+1, ])) & any(grepl("Patient Name", df_alt1[i+1, ]))){
      
      end_df_msd <- i-1
      start_df_patient <- i+1
      end_df_patient <- nrow(df_alt1)
    }
    
  }
  
  
  ####  Define sub dataframes #####
  #### For medical supplies distribution (MSD) #####
  
  df_msd <- df_alt1[(start_df_msd-1):end_df_msd, ]
  colnames(df_msd) <- df_msd[1,] ; df_msd <- df_msd[-1,] # Redefine columns
  df_msd <- df_msd %>% dplyr::select(    # Define columns to be kept
    c("Product", "Date", "Units Received", "Received From", "Units Released", "Released To (select from drop down list)", "Units Returned", "Returned By")
  )  %>%
    rename(Entry_Date = `Date`,
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
    as.POSIXct( df_msd[["Entry_Date"]] * (60*60*24)
                , origin="1899-12-30"
                , tz="GMT")
  
  #### For patient summary ######
  
  df_patient <- df_alt1[start_df_patient:end_df_patient, ]
  colnames(df_patient) <- df_patient[1,] ; df_patient <- df_patient[-1,] # Redefine columns
  
  
  
  #### MSD: Adjust dataframe #####
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
    df_msd[(df_msd$Received_From == max(df_msd$Received_From[df_msd$Product == j], na.rm = TRUE)) & (is.na(df_msd$Units_Released) == TRUE) & (df_msd$Product == j) & (df_msd$Entry_Date == min(df_msd$Entry_Date[df_msd$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "start"
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
  check9 <- matrix(NA, nrow = 1, ncol = 2) ; check9[,1] <- "Quality Check 9: Is there any accidental overlap of a START balance event and a CHANGE balance event?"
  
  df_msd$Product_Balance <- NA ; 
  
  for(j in unique(df_msd$Product)){
    df_sub <- df_msd[df_msd$Product == j,]
    for(i in 1:nrow(df_sub)){
      value <- as.numeric(na.omit(c(df_sub[i, 'Received_From'], df_sub[i, 'Units_Released'])))
      
      # Qualitycheck 9: If there is both a start or end value and aa change value, yield error
      if(length(value) > 1){
        check9[,2] <- paste0("Error! Please check the balance rows of product ", j, ' and make sure that there are no overlaps between start and end events.')
      } else{
        check9[,2] <- paste0("Check. No inconsistencies.")
      }
    }
  }
  
  
  for(i in 1:nrow(df_msd)){
    for(j in unique(df_msd$Product)){
      
      # 
      if(df_msd$Balance_Status[i] == "start"){
        df_msd[i, 'Product_Balance'] <- df_msd[i, 'Received_From'] 
        start_value <- df_msd[i, 'Received from']
      }
      if(df_msd$Balance_Status[i] == "change"){
        df_msd[i, 'Product_Balance'] <- as.numeric(df_msd[i-1, 'Product_Balance']) -  as.numeric(df_msd[i, 'Units_Released']) 
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
  
  for(i in 1:nrow(df_msd)){
    if(is.na(df_msd$Units_Received[i]) == FALSE & is.character(df_msd$Units_Received[i]) == TRUE){
      df_msd$Units_Received2[i] <- as.numeric(0)
    }
  }
  
  
  for(i in 1:nrow(df_msd)){
    # If a start or end balance is met
    if(is.na(df_msd$Received_From[i]) == FALSE & grepl("[[:digit:]]", df_msd$Received_From[i]) == TRUE & any(stri_detect_fixed(na.omit(df_msd$Units_Received[i]),c("START", "END"))) == TRUE){
      df_msd$Received_From[i] <- as.character(NA)
    }
    # If there is a number in "received_from" without being a start or end balance, yield error
    if(is.na(df_msd$Received_From[i]) == FALSE & grepl("[[:digit:]]", df_msd$Received_From[i]) == TRUE & any(stri_detect_fixed(na.omit(df_msd$Units_Received[i]),c("START", "END"))) == FALSE){
      check10[,2] <- "Error! Please check in column Received From if there is number which does not represent Start or End balance and should actually be assigned to column Units Received or Units Released."
    }
  }
  if(is.na(check10[,2]) == TRUE){
    check10[,2] <- "Check. No inconsistencies."
  }
  
  df_msd <- df_msd %>% select(., -ends_with("Units_Received")) %>% rename(., Units_Received = Units_Received2)
  
  
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
                                    Product_Released_To, Product_Patient_Name, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                    Product_No, Product_Balance_Status, Product_Units_Released_perproduct))
  
  #### MSD: Run version-dependent quality check #####
  
    #### Check3: Redundancy in rows #####
    # Test if any row is duplicated
    check3 <- matrix(NA, nrow = 1, ncol = 2) 
    check3[,1] <- "Quality Check 3: Are there duplicated rows?"
    
    df_msd_old <- df_msd
    df_msd <- df_msd %>% distinct()
    if(nrow(df_msd) < nrow(df_msd_old)){
      check3[,2] <- paste0('Error! ', 'Duplicated rows were found. Please check the rows in the input spreadsheet for duplicate rows.')
    } else{
      check3[,2] <- paste0('Check. ', 'No duplicated rows were found.')
    }
    
    
    #### Check4: Test classes and adjust were necessary #####
    check4 <- matrix(NA, nrow = 1, ncol = 2) 
    check4[,1] <- "Quality Check 4: Are there errors in the type (class) of data provided?"
    
    classes <- lapply(df_msd, class)
    classes_aim_name <- c("Product", "Product_Entry_Date", "Product_Balance", "Product_Units_Received", "Product_Received_From", "Product_Units_Released", "Product_Released_To",
                          "Product_Units_Returned", "Product_Returned_By", "Product_Patient_ID", "Product_No", "Product_Balance_Status", "Product_Units_Released_perproduct",
                          "Product_Country", "Product_table_month", "Product_table_year", "Product_Patient_ID", "Product_No", "Product_Balance_Status", "Product_Units_Released_perproduct",
                          "Product_Hospital")
    
    classes_aim_type <- c("character", "POSIXct", "numeric", "numeric", "character", "numeric", "character", "numeric", "character", "character", "numeric", "character",
                          "numeric", "character", "character", "numeric", "character", "numeric", "character", "numeric", "character")
    classes_aim <- data.frame(classes_aim_name, classes_aim_type) # Ideal classes
    
    for(j in unique(names(classes))){
      actual_name <- names(classes[j])
      actual_class <- classes[j][[1]][1]
      aim_class <- classes_aim[classes_aim$classes_aim_name == j, "classes_aim_type"]
      # If the class type matches
      if(class(df_msd[, actual_name])[1] == aim_class ) {
        print(j)
        print("match")
        
      } else{ # If the class type does not match, change type
        
        print(j)
        print("no match")
        
        if(aim_class == "character"){
          df_msd[, j] <- as.character( df_msd[, j])
        }
        if(aim_class == "numeric"){
          df_msd[, j] <- as.numeric( df_msd[, j])
        }
        if(aim_class == "POSIXct"){
          df_msd[, j] <- as.POSIXct.Date()( df_msd[, j])
        }
      }
    }
    
    # Return Output if any value is not compatible with aim class
    container <- list() ; index <- 0
    for(k in unique(names(lapply(df_msd, class)))){
      actual_name <- names(lapply(df_msd, class)[k])
      actual_class <- lapply(df_msd, class)[k][[1]][1]
      aim_class <- classes_aim[classes_aim$classes_aim_name == k, "classes_aim_type"]
      # If the class type matches
      if(class(df_msd[, actual_name])[1] != aim_class ) {
        index <- index + 1
        container[index] <- k
        
      }
    }
    
    if(length(container) > 0){
      check4[,2] <- paste0('Error! ', 'The following columns have an unexpected class of data: ', container)
    } else{
      check4[,2] <- paste0('Check. ', 'No errors in classes of data.')
    }
    
    
    
    
    #### Check5-7: Test if variable columns Product_Entry_Date, Product_Released_to (Patient name) and Patient_ID contains all relevant information #####
    check5 <- matrix(NA, nrow = 1, ncol = 2) ; check6 <- matrix(NA, nrow = 1, ncol = 2) ; check7 <- matrix(NA, nrow = 1, ncol = 2) 
    check5[,1] <- "Quality Check 5: Are there unexpected values in the column Entry_Date?"
    check7[,1] <- "Quality Check 7: Is the patient ID saved as expected?"
    
    
    # Check5: Product_Entry_Date, e.g. "2020-12-31 GMT", Quality Check 5
    list1 <- strsplit(as.character(df_msd$Product_Entry_Date),'-')
    years <- lapply(list1, `[[`, 1)
    years_unique <- sort(unique(c(as.numeric(unique(years)), as.numeric(unique(years))+1, as.numeric(unique(years))-1))) # year dates +- 1
    
    months <- as.numeric(lapply(list1, `[[`, 2))
    days <- as.numeric(lapply(list1, `[[`, 3))
    
    if((any(years %notin% years_unique) | (any(months > 12 )) |  (any(months < 1 )) | (any(days > 31 )) |  (any(months < 1 )))){
      check5[,2] <- paste0('Error! Please check the column Entry Date for unexpected values out of range.')
    } else{
      check5[,2] <- paste0('Check. No unexpected values in the column Entry_Date.')
      
      # print('Quality Check 5: Are there unexpected values in the column Entry_Date?')
      #print('Check. : No unexpected values in the column Entry_Date.')
      
    }
    
    # Check6: Test if Product_Released_to (Patient Name) is presend and contains all elements
    df_sub2 <- df_msd %>% filter((is.na(Product_Units_Released) == FALSE) & Product_Balance_Status == "change")
    patient_names <- lapply(df_sub2$Product_Released_To, strsplit, " ")
    patient_names_1 <- lapply(patient_names, `[[`, 1) ; patient_names_num <- as.numeric(lapply(patient_names_1, `[`, 2)) ; patient_num_prefix <- as.character(lapply(patient_names_1, `[`, 1))
    
    # Test for missing patient name or inconsistent patient name
    if(any(is.na(df_sub2$Product_Released_To) == TRUE)){
      check6[,1] <- "Quality Check 6a: Is the patient name saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient name is missing.')
    }  
    if(any(patient_names_num != as.numeric())){
      check6[,1] <- "'Quality Check 6b: Is the patient name saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient number is missing.')
    }
    if(any(patient_num_prefix != "Patient")){
      check6[,1] <- "Quality Check 6c: Is the patient name prefix saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient name prefix is missing.')
    }
    if(! ( (any(is.na(df_sub2$Product_Released_To) == TRUE)) | (any(patient_names_num != as.numeric())) | (any(patient_num_prefix != "Patient")) )){
      check6[,1] <- "Quality Check 6: Is the patient name prefix saved as expected?"
      check6[,2] <- paste0('Check. No inconsistencies.')
      
    }
    
    # Check7: Check if each Unit-receiver has a patient_ID
    if(any(is.na(df_sub2$Product_Patient_ID)) == TRUE){
      check7[,2] <- paste0('Error! Please check if all patients who have received units are documented in Patient summary.')
    } else{
      check7[,2] <- paste0('Check. No inconsistencies.')
      
    }
    
    
    
    #### Check8: Test if computation of Balance is correct #####
    check8 <- matrix(NA, nrow = 1, ncol = 2)
    check8[,1] <- "Quality Check 8: Has the column Product_Units_Released been computed correctly?"
    
    balance_inconsist <- 0    
    for(k in unique(df_msd$Product)){
      df_sub <- df_msd %>% dplyr::filter(Product == k)
      difference_entries <- as.numeric(max(df_sub$Product_Balance)) - as.numeric(min(df_sub$Product_Balance))
      if(difference_entries != as.numeric(unique(df_sub$Product_Units_Released_perproduct))){
        balance_inconsist <- balance_inconsist + 1
      }
    } 
    
    if(balance_inconsist > 0){
      check8[,2] <- paste0('Error! Please compare the columns Balance and Product_Units_Received, there are inconsistencies in ', balance_inconsist, ' products.')
      #print('Quality Check 8: Has the column Product_Units_Released been computed correctly?')
      #print(paste0('Error! : Please compare the columns Balance and Product_Units_Received, there are inconsistencies in ', balance_inconsist, ' products.'))
    } else{
      check8[,2] <- paste0('Check. No inconsistencies.')
      #print('Quality Check 8: Has the column Product_Units_Released been computed correctly?')
      #print(paste0('Check. : No inconsistencies.'))
    }   
    
  #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
  df_msd <- df_msd %>% 
    mutate(
      across(c(Product_Units_Received, Product_Units_Released, Product_Units_Returned), ~replace_na(.x, 0))
    )
  
  
}  
if(year = "2017" & month != "09"){
  #### Split dataset into two parts #####
  
  #### Identify splits ######
  # Get health supply part: 
  # Split when row contains columns with variables "Product", "Date", "Units Received"
  # Get patient recruitments summary part
  # Split when row "Summary of Patient Recruitment" is followed by row with columns "No", "Patient Name", "Province"
  
  for(i in 1:nrow(df)){
    
    if(any(grepl("Product", df_alt1[i, ])) &
       any(grepl("Date", df_alt1[i, ])) & any(grepl("Units Received", df_alt1[i, ]))){
      
      start_df_msd <- i+1
    }
    
    if(any(grepl("Summary of Patient Recruitment", df_alt1[i, ])) &
       any(grepl("No", df_alt1[i+1, ])) & any(grepl("Patient Name", df_alt1[i+1, ]))){
      
      end_df_msd <- i-1
      start_df_patient <- i+1
      end_df_patient <- nrow(df_alt1)
    }
    
  }
  
  
  ####  Define sub dataframes #####
  #### For medical supplies distribution (MSD) #####
  
  df_msd <- df_alt1[(start_df_msd-1):end_df_msd, ]
  colnames(df_msd) <- df_msd[1,] ; df_msd <- df_msd[-1,] # Redefine columns
  df_msd <- df_msd %>% dplyr::select(    # Define columns to be kept
    c("Product", "Date", "Units Received", "Received From", "Units Released", "Released To (select from drop down list)", "Units Returned", "Returned By")
  )  %>%
    rename(Entry_Date = `Date`,
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
    as.POSIXct( df_msd[["Entry_Date"]] * (60*60*24)
                , origin="1899-12-30"
                , tz="GMT")
  
  #### For patient summary ######
  
  df_patient <- df_alt1[start_df_patient:end_df_patient, ]
  colnames(df_patient) <- df_patient[1,] ; df_patient <- df_patient[-1,] # Redefine columns
  
  
  
  #### MSD: Adjust dataframe #####
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
    df_msd[(df_msd$Received_From == max(df_msd$Received_From[df_msd$Product == j], na.rm = TRUE)) & (is.na(df_msd$Units_Released) == TRUE) & (df_msd$Product == j) & (df_msd$Entry_Date == min(df_msd$Entry_Date[df_msd$Product == j], na.rm = TRUE)), 'Balance_Status'] <- "start"
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
  check9 <- matrix(NA, nrow = 1, ncol = 2) ; check9[,1] <- "Quality Check 9: Is there any accidental overlap of a START balance event and a CHANGE balance event?"
  check9[,2] <- "Note. No Balance data is available for this spreadsheet version."
  
  df_msd$Product_Balance <- NA 
  
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
      df_msd$Units_Received2[i] <- as.numeric(df_msd$Units_Received2[i])
    }
  }
  
  for(i in 1:nrow(df_msd)){
    if(is.na(df_msd$Received_From[i]) == FALSE & grepl("[[:digit:]]", df_msd$Received_From[i]) == TRUE){
      check10[,2] <- "Error! Please check in column Received From if there is number which should actually be assigned to column Units Received or Units Released."
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
  
  df_msd <- df_msd %>% select(., -ends_with("Units_Received")) %>% rename(., Units_Received = Units_Received2)
  
  
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
                                    Product_Released_To, Product_Patient_Name, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                    Product_No, Product_Balance_Status, Product_Units_Released_perproduct))
  
  #### MSD: Run version-dependent quality check #####
  
    #### Check3: Redundancy in rows #####
    # Test if any row is duplicated
    check3 <- matrix(NA, nrow = 1, ncol = 2) 
    check3[,1] <- "Quality Check 3: Are there duplicated rows?"
    
    df_msd_old <- df_msd
    df_msd <- df_msd %>% distinct()
    if(nrow(df_msd) < nrow(df_msd_old)){
      check3[,2] <- paste0('Error! ', 'Duplicated rows were found. Please check the rows in the input spreadsheet for duplicate rows.')
    } else{
      check3[,2] <- paste0('Check. ', 'No duplicated rows were found.')
    }
    
    
    #### Check4: Test classes and adjust were necessary #####
    check4 <- matrix(NA, nrow = 1, ncol = 2) 
    check4[,1] <- "Quality Check 4: Are there errors in the type (class) of data provided?"
    
    classes <- lapply(df_msd, class)
    classes_aim_name <- c("Product", "Product_Entry_Date", "Product_Balance", "Product_Units_Received", "Product_Received_From", "Product_Units_Released", "Product_Released_To",
                          "Product_Units_Returned", "Product_Returned_By", "Product_Patient_ID", "Product_No", "Product_Balance_Status", "Product_Units_Released_perproduct",
                          "Product_Country", "Product_table_month", "Product_table_year", "Product_Patient_ID", "Product_No", "Product_Balance_Status", "Product_Units_Released_perproduct",
                          "Product_Hospital")
    
    classes_aim_type <- c("character", "POSIXct", "numeric", "numeric", "character", "numeric", "character", "numeric", "character", "character", "numeric", "character",
                          "numeric", "character", "character", "numeric", "character", "numeric", "character", "numeric", "character")
    classes_aim <- data.frame(classes_aim_name, classes_aim_type) # Ideal classes
    
    for(j in unique(names(classes))){
      actual_name <- names(classes[j])
      actual_class <- classes[j][[1]][1]
      aim_class <- classes_aim[classes_aim$classes_aim_name == j, "classes_aim_type"]
      # If the class type matches
      if(class(df_msd[, actual_name])[1] == aim_class ) {
        print(j)
        print("match")
        
      } else{ # If the class type does not match, change type
        
        print(j)
        print("no match")
        
        if(aim_class == "character"){
          df_msd[, j] <- as.character( df_msd[, j])
        }
        if(aim_class == "numeric"){
          df_msd[, j] <- as.numeric( df_msd[, j])
        }
        if(aim_class == "POSIXct"){
          df_msd[, j] <- as.POSIXct.Date()( df_msd[, j])
        }
      }
    }
    
    # Return Output if any value is not compatible with aim class
    container <- list() ; index <- 0
    for(k in unique(names(lapply(df_msd, class)))){
      actual_name <- names(lapply(df_msd, class)[k])
      actual_class <- lapply(df_msd, class)[k][[1]][1]
      aim_class <- classes_aim[classes_aim$classes_aim_name == k, "classes_aim_type"]
      # If the class type matches
      if(class(df_msd[, actual_name])[1] != aim_class ) {
        index <- index + 1
        container[index] <- k
        
      }
    }
    
    if(length(container) > 0){
      check4[,2] <- paste0('Error! ', 'The following columns have an unexpected class of data: ', container)
    } else{
      check4[,2] <- paste0('Check. ', 'No errors in classes of data.')
    }
    
    
    
    
    #### Check5-7: Test if variable columns Product_Entry_Date, Product_Released_to (Patient name) and Patient_ID contains all relevant information #####
    check5 <- matrix(NA, nrow = 1, ncol = 2) ; check6 <- matrix(NA, nrow = 1, ncol = 2) ; check7 <- matrix(NA, nrow = 1, ncol = 2) 
    check5[,1] <- "Quality Check 5: Are there unexpected values in the column Entry_Date?"
    check7[,1] <- "Quality Check 7: Is the patient ID saved as expected?"
    
    
    # Check5: Product_Entry_Date, e.g. "2020-12-31 GMT", Quality Check 5
    list1 <- strsplit(as.character(df_msd$Product_Entry_Date),'-')
    years <- lapply(list1, `[[`, 1)
    years_unique <- sort(unique(c(as.numeric(unique(years)), as.numeric(unique(years))+1, as.numeric(unique(years))-1))) # year dates +- 1
    
    months <- as.numeric(lapply(list1, `[[`, 2))
    days <- as.numeric(lapply(list1, `[[`, 3))
    
    if((any(years %notin% years_unique) | (any(months > 12 )) |  (any(months < 1 )) | (any(days > 31 )) |  (any(months < 1 )))){
      check5[,2] <- paste0('Error! Please check the column Entry Date for unexpected values out of range.')
    } else{
      check5[,2] <- paste0('Check. No unexpected values in the column Entry_Date.')
      
      # print('Quality Check 5: Are there unexpected values in the column Entry_Date?')
      #print('Check. : No unexpected values in the column Entry_Date.')
      
    }
    
    # Check6: Test if Product_Released_to (Patient Name) is presend and contains all elements
    df_sub2 <- df_msd %>% filter((is.na(Product_Units_Released) == FALSE) & Product_Balance_Status == "change")
    patient_names <- lapply(df_sub2$Product_Released_To, strsplit, " ")
    patient_names_1 <- lapply(patient_names, `[[`, 1) ; patient_names_num <- as.numeric(lapply(patient_names_1, `[`, 2)) ; patient_num_prefix <- as.character(lapply(patient_names_1, `[`, 1))
    
    # Test for missing patient name or inconsistent patient name
    if(any(is.na(df_sub2$Product_Released_To) == TRUE)){
      check6[,1] <- "Quality Check 6a: Is the patient name saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient name is missing.')
    }  
    if(any(patient_names_num != as.numeric())){
      check6[,1] <- "'Quality Check 6b: Is the patient name saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient number is missing.')
    }
    if(any(patient_num_prefix != "Patient")){
      check6[,1] <- "Quality Check 6c: Is the patient name prefix saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient name prefix is missing.')
    }
    if(! ( (any(is.na(df_sub2$Product_Released_To) == TRUE)) | (any(patient_names_num != as.numeric())) | (any(patient_num_prefix != "Patient")) )){
      check6[,1] <- "Quality Check 6: Is the patient name prefix saved as expected?"
      check6[,2] <- paste0('Check. No inconsistencies.')
      
    }
    
    # Check7: Check if each Unit-receiver has a patient_ID
    if(any(is.na(df_sub2$Product_Patient_ID)) == TRUE){
      check7[,2] <- paste0('Error! Please check if all patients who have received units are documented in Patient summary.')
    } else{
      check7[,2] <- paste0('Check. No inconsistencies.')
      
    }
    
    
    
    #### Check8: Test if computation of Balance is correct #####
    check8 <- matrix(NA, nrow = 1, ncol = 2)
    check8[,1] <- "Quality Check 8: Has the column Product_Units_Released been computed correctly?"
    
    balance_inconsist <- 0    
    for(k in unique(df_msd$Product)){
      df_sub <- df_msd %>% dplyr::filter(Product == k)
      difference_entries <- as.numeric(max(df_sub$Product_Balance)) - as.numeric(min(df_sub$Product_Balance))
      if(difference_entries != as.numeric(unique(df_sub$Product_Units_Released_perproduct))){
        balance_inconsist <- balance_inconsist + 1
      }
    } 
    
    if(balance_inconsist > 0){
      check8[,2] <- paste0('Error! Please compare the columns Balance and Product_Units_Received, there are inconsistencies in ', balance_inconsist, ' products.')
      #print('Quality Check 8: Has the column Product_Units_Released been computed correctly?')
      #print(paste0('Error! : Please compare the columns Balance and Product_Units_Received, there are inconsistencies in ', balance_inconsist, ' products.'))
    } else{
      check8[,2] <- paste0('Check. No inconsistencies.')
      #print('Quality Check 8: Has the column Product_Units_Released been computed correctly?')
      #print(paste0('Check. : No inconsistencies.'))
    }   
    
  #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
  df_msd <- df_msd %>% 
    mutate(
      across(c(Product_Units_Received, Product_Units_Released, Product_Units_Returned), ~replace_na(.x, 0))
    )
  
  
} 
  
if(year = "2017" & month = "09"){
  #### Split dataset into two parts #####
  
  #### Identify splits ######
  # Get health supply part: 
  # Split when row contains columns with variables "Product", "Date", "Units Received  (bottle/box)"
  # Get patient recruitments summary part
  # Split when row "Summary of Patient Recruitment" is followed by row with columns "ID", "Patient Name", "Province"
  
  for(i in 1:nrow(df)){
    
    if(any(grepl("Product", df_alt1[i, ])) &
       any(grepl("Date", df_alt1[i, ])) & any(grepl("Units Received", df_alt1[i, ]))){
      
      start_df_msd <- i+1
    }
    
    if(any(grepl("Summary of Patient Recruitment", df_alt1[i, ])) &
       any(grepl("ID", df_alt1[i+1, ])) & any(grepl("Patient Name", df_alt1[i+1, ]))){
      
      end_df_msd <- i-1
      start_df_patient <- i+1
      end_df_patient <- nrow(df_alt1)
    }
    
  }
  
  
  ####  Define sub dataframes #####
  #### For medical supplies distribution (MSD) #####
  
  df_msd <- df_alt1[(start_df_msd-1):end_df_msd, ]
  colnames(df_msd) <- df_msd[1,] ; df_msd <- df_msd[-1,] # Redefine columns
  df_msd <- df_msd %>% dplyr::select(    # Define columns to be kept
    c("Product", "Date", "Units Received \r\n(bottle/box)", "Received From", "Units Released\r\n(box)", "Released To")
  )  %>%
    rename(Entry_Date = `Date`,
           Units_Received = `Units Received \r\n(bottle/box)`,
           Received_From = `Received From`,
           Released_To = `Released To`,
           Units_Released = `Units Released\r\n(box)`
    )
  
  # Remove empty rows
  df_msd <- df_msd %>% filter_all(any_vars(complete.cases(.)))  
  
  
  # Change column "Entry Date" to datetime object
  df_msd$Entry_Date <- as.numeric(df_msd$Entry_Date)  
  df_msd[["Entry_Date"]] <- 
    as.POSIXct( df_msd[["Entry_Date"]] * (60*60*24)
                , origin="1899-12-30"
                , tz="GMT")
  
  #### For patient summary ######
  
  df_patient <- df_alt1[start_df_patient:end_df_patient, ]
  colnames(df_patient) <- df_patient[1,] ; df_patient <- df_patient[-1,] # Redefine columns
  
  
  
  #### MSD: Adjust dataframe #####
  #### Add variables country and hospital #####
  df_msd$Product_Country <- country    
  df_msd$Product_Hospital <- hospital    
  
  #### Add variable table_month and table_year ####    
  latest_date <- as.character(max(df_msd$Entry_Date, na.rm = TRUE))
  year <- as.numeric(strsplit(latest_date, "-")[[1]][1]) ; month <- as.character(strsplit(latest_date, "-")[[1]][2])
  
  df_msd$table_month <- month
  df_msd$table_year <- year
  
  #### Add variable "Patient_Name" #####
  
  df_msd$Patient_Name <- NA
  column_patname <- select(df_msd, matches("patient"))
  column_patname <- colnames(df_msd[,grepl("patient", colnames(df_msd))])
  
  if(length(column_patname) > 0){ # Keep first column which contains "patient" values
    df_msd$Patient_Name <- df_msd[, column_patname[1]]
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
  
  
  
  #### Add additional column Product_Balance ####
  check9 <- matrix(NA, nrow = 1, ncol = 2) ; check9[,1] <- "Quality Check 9: Is there any accidental overlap of a START balance event and a CHANGE balance event?"
  check9[,2] <- "Note. No Balance data is available for this spreadsheet version."
  
  df_msd$Product_Balance <- NA 
  
  for(i in 1:nrow(df_msd)){
    for(j in unique(df_msd$Product)){
      start_value <- as.numeric(head(df_msd$Units_Received[df_msd$Product == j], n = 1))
      
      if(df_msd[i, 'Units_Received'] == start_value){
        df_msd[i, 'Product_Balance'] <- start_value
        new_value <- start_value
      } else{
        df_msd[i, 'Product_Balance'] <- new_value + na.omit(as.numeric(df_msd[i, 'Units_Received'])) -  na.omit(as.numeric(df_msd[i, 'Units_Released']))
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
  
  df_msd <- df_msd %>% select(., -ends_with("Units_Received")) %>% rename(., Units_Received = Units_Received2)
  
  #### Add missing columns (empty) ####
  df_msd$Units_Returned <- NA
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
                                    Product_Released_To, Product_Patient_Name, Product_Units_Returned, Product_Returned_By, Product_Country, Product_Hospital, Product_table_month, Product_table_year,
                                    Product_No, Product_Balance_Status, Product_Units_Released_perproduct))
  
  #### Run version-dependent quality check #####
  
    #### Check3: Redundancy in rows #####
    # Test if any row is duplicated
    check3 <- matrix(NA, nrow = 1, ncol = 2) 
    check3[,1] <- "Quality Check 3: Are there duplicated rows?"
    
    df_msd_old <- df_msd
    df_msd <- df_msd %>% distinct()
    if(nrow(df_msd) < nrow(df_msd_old)){
      check3[,2] <- paste0('Error! ', 'Duplicated rows were found. Please check the rows in the input spreadsheet for duplicate rows.')
    } else{
      check3[,2] <- paste0('Check. ', 'No duplicated rows were found.')
    }
    
    
    #### Check4: Test classes and adjust were necessary #####
    check4 <- matrix(NA, nrow = 1, ncol = 2) 
    check4[,1] <- "Quality Check 4: Are there errors in the type (class) of data provided?"
    
    classes <- lapply(df_msd, class)
    classes_aim_name <- c("Product", "Product_Entry_Date", "Product_Balance", "Product_Units_Received", "Product_Received_From", "Product_Units_Released", "Product_Released_To",
                          "Product_Units_Returned", "Product_Returned_By", "Product_Patient_ID", "Product_No", "Product_Balance_Status", "Product_Units_Released_perproduct",
                          "Product_Country", "Product_table_month", "Product_table_year", "Product_Patient_ID", "Product_No", "Product_Balance_Status", "Product_Units_Released_perproduct",
                          "Product_Hospital")
    
    classes_aim_type <- c("character", "POSIXct", "numeric", "numeric", "character", "numeric", "character", "numeric", "character", "character", "numeric", "character",
                          "numeric", "character", "character", "numeric", "character", "numeric", "character", "numeric", "character")
    classes_aim <- data.frame(classes_aim_name, classes_aim_type) # Ideal classes
    
    for(j in unique(names(classes))){
      actual_name <- names(classes[j])
      actual_class <- classes[j][[1]][1]
      aim_class <- classes_aim[classes_aim$classes_aim_name == j, "classes_aim_type"]
      # If the class type matches
      if(class(df_msd[, actual_name])[1] == aim_class ) {
        print(j)
        print("match")
        
      } else{ # If the class type does not match, change type
        
        print(j)
        print("no match")
        
        if(aim_class == "character"){
          df_msd[, j] <- as.character( df_msd[, j])
        }
        if(aim_class == "numeric"){
          df_msd[, j] <- as.numeric( df_msd[, j])
        }
        if(aim_class == "POSIXct"){
          df_msd[, j] <- as.POSIXct.Date()( df_msd[, j])
        }
      }
    }
    
    # Return Output if any value is not compatible with aim class
    container <- list() ; index <- 0
    for(k in unique(names(lapply(df_msd, class)))){
      actual_name <- names(lapply(df_msd, class)[k])
      actual_class <- lapply(df_msd, class)[k][[1]][1]
      aim_class <- classes_aim[classes_aim$classes_aim_name == k, "classes_aim_type"]
      # If the class type matches
      if(class(df_msd[, actual_name])[1] != aim_class ) {
        index <- index + 1
        container[index] <- k
        
      }
    }
    
    if(length(container) > 0){
      check4[,2] <- paste0('Error! ', 'The following columns have an unexpected class of data: ', container)
    } else{
      check4[,2] <- paste0('Check. ', 'No errors in classes of data.')
    }
    
    
    
    
    #### Check5-7: Test if variable columns Product_Entry_Date, Product_Released_to (Patient name) and Patient_ID contains all relevant information #####
    check5 <- matrix(NA, nrow = 1, ncol = 2) ; check6 <- matrix(NA, nrow = 1, ncol = 2) ; check7 <- matrix(NA, nrow = 1, ncol = 2) 
    check5[,1] <- "Quality Check 5: Are there unexpected values in the column Entry_Date?"
    check7[,1] <- "Quality Check 7: Is the patient ID saved as expected?"
    
    
    # Check5: Product_Entry_Date, e.g. "2020-12-31 GMT", Quality Check 5
    list1 <- strsplit(as.character(df_msd$Product_Entry_Date),'-')
    years <- lapply(list1, `[[`, 1)
    years_unique <- sort(unique(c(as.numeric(unique(years)), as.numeric(unique(years))+1, as.numeric(unique(years))-1))) # year dates +- 1
    
    months <- as.numeric(lapply(list1, `[[`, 2))
    days <- as.numeric(lapply(list1, `[[`, 3))
    
    if((any(years %notin% years_unique) | (any(months > 12 )) |  (any(months < 1 )) | (any(days > 31 )) |  (any(months < 1 )))){
      check5[,2] <- paste0('Error! Please check the column Entry Date for unexpected values out of range.')
    } else{
      check5[,2] <- paste0('Check. No unexpected values in the column Entry_Date.')
      
      # print('Quality Check 5: Are there unexpected values in the column Entry_Date?')
      #print('Check. : No unexpected values in the column Entry_Date.')
      
    }
    
    # Check6: Test if Product_Released_to (Patient Name) is presend and contains all elements
    df_sub2 <- df_msd %>% filter((is.na(Product_Units_Released) == FALSE) & Product_Balance_Status == "change")
    patient_names <- lapply(df_sub2$Product_Released_To, strsplit, " ")
    patient_names_1 <- lapply(patient_names, `[[`, 1) ; patient_names_num <- as.numeric(lapply(patient_names_1, `[`, 2)) ; patient_num_prefix <- as.character(lapply(patient_names_1, `[`, 1))
    
    # Test for missing patient name or inconsistent patient name
    if(any(is.na(df_sub2$Product_Released_To) == TRUE)){
      check6[,1] <- "Quality Check 6a: Is the patient name saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient name is missing.')
    }  
    if(any(patient_names_num != as.numeric())){
      check6[,1] <- "'Quality Check 6b: Is the patient name saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient number is missing.')
    }
    if(any(patient_num_prefix != "Patient")){
      check6[,1] <- "Quality Check 6c: Is the patient name prefix saved as expected?"
      check6[,2] <- paste0('Error! Please check column Units_Released_to, at least one patient name prefix is missing.')
    }
    if(! ( (any(is.na(df_sub2$Product_Released_To) == TRUE)) | (any(patient_names_num != as.numeric())) | (any(patient_num_prefix != "Patient")) )){
      check6[,1] <- "Quality Check 6: Is the patient name prefix saved as expected?"
      check6[,2] <- paste0('Check. No inconsistencies.')
      
    }
    
    # Check7: Check if each Unit-receiver has a patient_ID
    if(any(is.na(df_sub2$Product_Patient_ID)) == TRUE){
      check7[,2] <- paste0('Error! Please check if all patients who have received units are documented in Patient summary.')
    } else{
      check7[,2] <- paste0('Check. No inconsistencies.')
      
    }
    
    
    
    #### Check8: Test if computation of Balance is correct #####
    check8 <- matrix(NA, nrow = 1, ncol = 2)
    check8[,1] <- "Quality Check 8: Has the column Product_Units_Released been computed correctly?"
    
    balance_inconsist <- 0    
    for(k in unique(df_msd$Product)){
      df_sub <- df_msd %>% dplyr::filter(Product == k)
      difference_entries <- as.numeric(max(df_sub$Product_Balance)) - as.numeric(min(df_sub$Product_Balance))
      if(difference_entries != as.numeric(unique(df_sub$Product_Units_Released_perproduct))){
        balance_inconsist <- balance_inconsist + 1
      }
    } 
    
    if(balance_inconsist > 0){
      check8[,2] <- paste0('Error! Please compare the columns Balance and Product_Units_Received, there are inconsistencies in ', balance_inconsist, ' products.')
      #print('Quality Check 8: Has the column Product_Units_Released been computed correctly?')
      #print(paste0('Error! : Please compare the columns Balance and Product_Units_Received, there are inconsistencies in ', balance_inconsist, ' products.'))
    } else{
      check8[,2] <- paste0('Check. No inconsistencies.')
      #print('Quality Check 8: Has the column Product_Units_Released been computed correctly?')
      #print(paste0('Check. : No inconsistencies.'))
    }   
    
  #### MSD: Recode columns "Product Units Received", "Product Units Released" and "Product Units returned" with 0 where NA ####
  df_msd <- df_msd %>% 
    mutate(
      across(c(Product_Units_Received, Product_Units_Released, Product_Units_Returned), ~replace_na(.x, 0))
    )
  
  
} else{ # If the month and year variables are not legit values, yield an error feedback
  print('Error: The script could not be run. Please check if the month and/or year you have provided are between 2017 and 2021.')
}  
#### Run version-independent quality checks (do not change) ####
  #### Check1 and Check2: Test if all relevant columns are present (and no duplicate columns) #####
  check1 <- matrix(NA, nrow = 1, ncol = 2) ; check2 <- matrix(NA, nrow = 1, ncol = 2)
  check1[, 1] <- "Quality Check 1: Are all and only relevant colum names present?"
  check2[,1] <- "Quality Check 2: Are there duplicated colum names?"
  
  rel_cols <- c("Product", "Product_Entry_Date", "Product_Balance", "Product_Units_Received", "Product_Received_From", "Product_Units_Released", "Product_Released_To",
                "Product_Units_Returned", "Product_Returned_By", "Product_Patient_ID", "Product_No", "Product_Balance_Status", "Product_Units_Released_perproduct",
                "Product_Country", "Product_table_month", "Product_table_year", "Product_Patient_ID", "Product_No", "Product_Balance_Status", "Product_Units_Released_perproduct",
                "Product_Hospital")
  
  # If relevant columns are missing
  if(any(rel_cols %notin% colnames(df_msd))){
    missing_col_names <- rel_cols[which(rel_cols %notin% colnames(df_msd))]
    check1[, 2] <- paste0('Error! ', 'The following column name is missing in the Medical Supply Data table: ', missing_col_names)
  } else if(any(colnames(df_msd) %notin% rel_cols)){  # If there are too many columns
    excess_col_names <- df_msd[which(colnames(df_msd) %notin% rel_cols)]
    check1[, 2] <- paste0('Error! ', 'The following column name was not expected in the Medical Supply Data table: ', missing_col_names)
  } else{
    check1[, 2] <- paste0('Check. ', 'All column names are present and as expected in the Medical Supply Data table.')
  }
  
  # If columns were duplicated
  if(any(duplicated(colnames(df_msd))) == TRUE){
    check2[, 2] <- paste0('Error! ', 'Please check the column names of the input spreadsheet for duplicates.')
  } else{
    check2[, 2] <- paste0('Check. ', 'All column names appear only once.')
  }
  
  
  
  
  
#### MSD: Save data and quality check feedback ####      
  
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
  df_feedback <- as.data.frame(rbind(check1, check2, check3, check4, check5, check6, check7, check8, check9, check10))
  writexl::write_xlsx(df_feedback, 
                      paste0(path_output, version_year, '_', country, '_', hospital, '_', unique(df_msd$Product_table_month),
                             '_', unique(df_msd$Product_table_year), 'qualitycheck.xlsx'))
  # Save preprocessed data file  
  # write.table(df_msd, row.names = FALSE, sep = ";",
  #             paste0(path_output, 'v072017_', country, '_', hospital, '_', unique(df_msd$Product_table_month),
  #                  '_', unique(df_msd$Product_table_year), '.csv'))      
  writexl::write_xlsx(df_msd, 
                      paste0(path_output, version_year, '_', country, '_', hospital, '_', unique(df_msd$Product_table_month),
                             '_', unique(df_msd$Product_table_year), '.xlsx'))
  
  
  
  
  
  ##