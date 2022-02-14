# DESCRIPTION -------------------------------------------------------------

# "reading_a4d_tracker" is a function that reads an excel workbook (.xlsx file) that contains a4d monthly trackers and creates a tidy dataframe with that data. 
# It takes into account all the sheets in the excel workbook that contain patient data, and binds these together. (e.g., if a workbook contains sheets Jan'18, Feb'18
#  and Mar'18, patient data from each sheet will be combined into one data frame)
# 
# INPUT ARGUMENTS: -
#       tracker_data_file = full path of the excel workbook that contains a4d monthly trackers (format: string e.g., "users/Documents/file.xlsx")
#       year = year of tracker data (format: numeric. e.g., 2018)
#       country = country code (format: character string e.g., "xyz")
#       clinic = clinic code (format: character string e.g., "xyz")
#      
#      example: reading_a4d_tracker(tracker_data_file = "~/Desktop/2017 Tracker Template.xlsx",  
#                                    year = 2017,
#                                    clinic = "ABC",
#                                    country = "DEF")
#       
#  FUNCTION OUTPUT: 
#       tidydata: "tidy" dataframe with patient data with the following columns (format: character. This is to facilitate merging of dataframes)
# [1] "no"                                
# [2] "patient_name"                      
# [3] "province"                          
# [4] "gender"                            
# [5] "dob"                               
# [6] "age"                               
# [7] "age_diagnosis"                     
# [8] "recruitment_date"                  
# [9] "baseline_hba1c_prc"                
# [10] "updated_hba1c_prc"                 
# [11] "updated_hba1c_date"                
# [12] "baseline_fbg_mldl"                 
# [13] "updated_fbg_mldl"                  
# [14] "updated_fbg_date"                  
# [15] "support_from_a4d"                  
# [16] "testing_fqr"                       
# [17] "est_strips_pmoth"                  
# [18] "status"                            
# [19] "updated_fbg_sample"                
# [20] "tracker_year"                      
# [21] "clinic_code"                       
# [22] "country_code"                      
# [23] "sheet_name"                        
# [24] "insulin_regimen"                   
# [25] "blood_pressure_sys_mmhg"           
# [26] "blood_pressure_dias_mmhg"          
# [27] "weight"                            
# [28] "height"                            
# [29] "bmi"                               
# [30] "bmi_date"                          
# [31] "edu_occ"                           
# [32] "hospitalisation"                   
# [33] "last_clinic_visit_date"            
# [34] "additional_support"                
# [35] "id"                                
# [36] "latest_complication_screening_type"
# [37] "latest_complication_screening_date"
# [38] "remarks"                           

# PACKAGES ----------------------------------------------------------------


library(tidyverse)
library(readxl)
library(zoo)
library(readr)
library(lubridate)
library(data.table)
library(tidyr)
library(dplyr)

## TODO:
# - Add logic to extract columns from "DC_V2_Anon Example
# - Need to fix 2017 - 2021 trackers (2017, 2018 done...)

tracker_data_file <- "/Volumes/A4D_project/01_2019 AN Clinic_YA A4D Tracker.xlsx"



extract_country_clinic_code <- function(tracker_data){
  # extract country and clinic
  country_code <- toupper(tracker_data[grepl("country",tolower(tracker_data[,2])),2])
  clinic_code <- toupper(tracker_data[grepl("clinic",tolower(tracker_data[,2])),2])
  country_code <- substr(country_code, 9, 10)
  clinic_code <- substr(clinic_code, 8, 9)
  
  if (is.na(country_code) |is.na(clinic_code)) {
    # extract country and clinic
    id_loc <- na.exclude(tracker_data[str_detect(tracker_data[,2], "_"),2])[1]
    country_code <- substr(id_loc, 1, 2)
    clinic_code <- substr(id_loc, 4, 5)
  }      
  
  output_list <- list("country_code"=country_code, "clinic_code"=clinic_code)
  return(output_list)
  
  ## alternative: 
  # if (sheet_num == 1) {
  #   # extract country and clinic
  #   country_code <- tolower(tracker_data$CLINIC.SUPPORT.PROGRAMME[grepl("country",tolower(tracker_data$CLINIC.SUPPORT.PROGRAMME))])
  #   clinic_code <- tolower(tracker_data$CLINIC.SUPPORT.PROGRAMME[grepl("clinic",tolower(tracker_data$CLINIC.SUPPORT.PROGRAMME))])
  #   print(country_code)
  #   
  # }
  
  # extract country and clinic codes 
  # current approach doesn't work for 2018+ -> no clinic info upfront, need to be read out from the patient IDs / filename 
  
}
  

extract_patient_data <- function(tracker_data, country_code, clinic_code){
  
  i <- min(which(tracker_data$CLINIC.SUPPORT.PROGRAMME == "Patient ID"))
  all_rows_with_patient_id = tracker_data$CLINIC.SUPPORT.PROGRAMME %like% paste0(country_code, "_")
  j <- max(which(all_rows_with_patient_id))
  patient_df <- data.frame(tracker_data[i:j,])
  
  # rename cols
  patient_df_headers <- patient_df[1,]
  colnames(patient_df) <- patient_df_headers
  
  # remove 1st column & row
  patient_df <- patient_df[,-1]
  patient_df <- patient_df[-1,]
  
  # view(patient_df)
  return(patient_df)
  
}

# @Description: Imports the patient df, cleanse it and match it against 
#               column synonyms to unify column names
# @columns_synonyms: Long format output of read_column_synonyms to match columns
harmonize_patient_data_columns <- function(patient_df, columns_synonyms){
        patient_df_headers_org <- colnames(patient_df)
        patient_df_headers <- colnames(patient_df) %>%
                                sanitize_column_name
        # TODO: better tracking of erroneous entries -> export both i and unformatted colname?
        problematic_indices <- list()
        
        # iterate through the header names 
        for(i in 1:length(patient_df_headers)){
          this_headername <- patient_df_headers[i]
          
          # deal with empty headers by filling in the previous header with an id
          if (i==1){
            previous_headername <- "first header"
          } else {
            previous_headername <- patient_df_headers_org[i-1]
          }
          # if header is empty, carry on the previous header name with a counter at the end
          if( is.na(this_headername)){
            print(c("found null column at index ", i))
            this_headername <- paste(c(previous_headername, " col", i), collapse="") 
            this_headername <- sanitize_column_name(this_headername)
            patient_df_headers[i] <- this_headername
          }
          
          # look for a matching headername in the synonyms table
          # check if the header name is present in current dictionary
          header_name_present = any(this_headername == columns_synonyms$name_to_be_matched)
          if (header_name_present){
                patient_df_headers[i] <- columns_synonyms$name_clean[which(this_headername == columns_synonyms$name_to_be_matched)]
          }
          else {
            print(i)
            print(this_headername)
            problematic_indices <- append(problematic_indices, i)
          }
        }      
        # view(patient_df_headers)
        if(length(problematic_indices)>0){
          print("problematic column names found")
          view(problematic_indices)
          view(patient_df_headers_org)
          view(patient_df_headers)
        }
        
        colnames(patient_df) <- patient_df_headers
        return(patient_df)
}

# @Description: Imports the codebook, cleans, removes duplicates and transforms it
#               into long df format
# @codebook_data_file: Path to codebook
# @sheet: Sheet either being "synonyms_PatientData" or "synonyms_ProductData"
#         to receive the synonyms of the respective data 
read_column_synonyms <- function(codebook_data_file, sheet){
  columns_synonyms <- codebook_data_file %>%
          read_xlsx(sheet) %>%
          as_tibble() %>%
          pivot_longer(cols = everything(),
                       names_to = "name_clean",
                       values_to = "name_to_be_matched") %>%
          subset(!is.na(name_to_be_matched)) %>%
          lapply(sanitize_column_name) %>%
          as_tibble() %>%
          group_by(name_to_be_matched) %>%
          slice(1) %>%
          ungroup()
  view(columns_synonyms)
  return(columns_synonyms)
}

sanitize_column_name <- function(column_name){
  column_name_clean <- column_name %>%
          tolower() %>%            # all lowercase
          str_squish()             # merge multiple spaces into one

  # column_name_clean <- column_name 
  
  return(column_name_clean)
}


extract_date_from_measurement_column <- function(patient_df, colname){
  # produces columns with names coherent with original naming before refactor 
  colname_value <- paste(c(colname,""), collapse = "")
  colname_core = sub('[_][^_]+$', '', colname) #remove last element after "_"
  colname_date <- paste(c(colname_core,"_date"), collapse = "")
  patient_df <- separate_(data=patient_df, col=colname,
                          into=c(colname_value,colname_date),sep = "([(])")
  print(c("separated column: ", colname))
  return(patient_df)
}

initial_clean_up_patient_df <- function(patient_df){
  # TODO: implement a check that first checks if a given column exists before attempting to mutate 
  
   patient_df <- patient_df %>% mutate(updated_fbg_sample = sub("^([[:alpha:]]*).*", "\\1", updated_fbg_mgdl),
                                       updated_fbg_mgdl = gsub("[^0-9.-]", "", updated_fbg_mgdl))

   patient_df <- patient_df %>%
           mutate(recruitment_date= as.POSIXct(as.numeric(recruitment_date)* (60*60*24),
                                               origin="1899-12-30",
                                               tz="GMT"),
                  updated_hba1c_date = gsub(")", "", updated_hba1c_date),
                  updated_fbg_date = gsub(")", "", updated_fbg_date),
                  updated_fbg_date=  as.Date(as.yearmon(updated_fbg_date, "%b-%y")),
                  updated_hba1c_date= as.Date(as.yearmon(updated_fbg_date, "%b-%y"))
  )
  
  
}

# FUNCTION TO READ THE A4D MONTHLY TRACKER --> PATIENT DATA --------------------------------------------------------
reading_a4d_tracker <- function(tracker_data_file, codebook) {
  
        columns_synonyms <- codebook

        # list the sheets in excel workbook & filter these
        sheet_list <- excel_sheets(tracker_data_file)

        # MONTHLY SHEETS: only select sheets with monthly data
        month_list <-sheet_list[na.omit(pmatch(month.abb, sheet_list))]

        # AN PATIENT DATA SHEET: select sheet in workbook with PATIENT AN DATA
        # THIS WON'T WORK WITH AN EXAMPLE TRACKER!!! 
        patient_sheet <-sheet_list[na.omit(grepl("AN Data", sheet_list))]
        # print(patient_sheet)
        
        # AN PATIENT DATA DATA (merge/join at the end of the if year):
        an_patient_data <- data.frame(read_xlsx(tracker_data_file, patient_sheet))
        all_patient_ids <- an_patient_data$Patient.ID
        print("patient AN Data extracted")
        
        # Extract year
        year <- 2000 + unique(parse_number(month_list))
        print(year)
        tidy_tracker_list <- NULL
        
        sheet_num <- 1
        for (CurrSheet in month_list) {
          
                print(CurrSheet)
                
                tracker_data <- data.frame(read_xlsx(tracker_data_file, CurrSheet))
                print("tracker read in")
                
                cc_codes <- extract_country_clinic_code(tracker_data)
                country_code <- cc_codes$country_code
                clinic_code <- cc_codes$clinic_code
                print("country and clinic code extracted")
                
                # view(tracker_data)
                
                
                ####------------2017 PATIENT DATA ----------------------------###
                #### 2017 ####
                
                if (year == 2017) {

                        patient_df = extract_patient_data(tracker_data, country_code)
                        print("patient df extracted")
                        patient_df = harmonize_patient_data_columns(patient_df, columns_synonyms)
                        print("finished harmonizing patient df")
                        
                        # fix dates (split dates in cells)
                        patient_df <- extract_date_from_measurement_column(patient_df, "updated_hba1c_prc")
                        patient_df <- extract_date_from_measurement_column(patient_df, "updated_fbg_mgdl")
                        print("date extracted from compound cols")
                        
                        patient_df <- initial_clean_up_patient_df(patient_df)
                        print("initial cleaning done")

                        patient_df <- patient_df %>%
                               mutate(sheet_name = CurrSheet,
                                    tracker_mo = match(substr(CurrSheet, 1, 3),month.abb),
                                    tracker_year = year)
                        print("added tracker metadata")
                        # view(patient_df)
                        
                        # TODO: for some reason the N/A detection doesn't work  
                        # patient_df[patient_df == "NA"] <- NA
                        # print("filled in the NA")
                        
                        print("--------------------")
                        
                } # 2017 tracker
                
                ####------------ 2018 PATIENT DATA ----------------------------###
                # 2018 ####
                if (year == 2018) {
                        
                        if (sheet_num == 1) {
                                # extract country and clinic
                                country_code <- tolower(tracker_data$CLINIC.SUPPORT.PROGRAMME[grepl("country",tolower(tracker_data$CLINIC.SUPPORT.PROGRAMME))])
                                clinic_code <- tolower(tracker_data$CLINIC.SUPPORT.PROGRAMME[grepl("clinic",tolower(tracker_data$CLINIC.SUPPORT.PROGRAMME))])
                                
                        }
                        
                        #Index (i.e., from which row to select the data)
                        i <- min(which(tracker_data$CLINIC.SUPPORT.PROGRAMME == "Patient ID"))
                        j <- max(which(tracker_data$CLINIC.SUPPORT.PROGRAMME %like% toupper(substr(country_code, 9, 10))))
                        # selecting relevant rows
                        patient_df <- data.frame(tracker_data[i:j,])
                        
                        
                        # rename cols
                        colnames(patient_df) <- patient_df[1,]
                        
                        # remove 1st column & row
                        patient_df <- patient_df[,-1]
                        patient_df <- patient_df[-1,]
                        
                        
                        # remove certain columns (part of AN tab)
                        patient_df <- patient_df[,-c(2,3,5)]
                        
                        
                        # THIS NEEDS TO BE THE SAME/HARMONIZED FOR ALL DATAFRAMES THAT WE WILL LATER COMBINE
                        # extracted manually from excel tracker, slightly edited the name, and then added via datapasta
                        
                        colnames(patient_df) <- c("id",
                                                  "gender",
                                                  "age",
                                                  "age_diagnosis",
                                                  "recruitment_date",
                                                  "baseline_hba1c_prc", 
                                                  "updated_hba1c_prc", # also has date
                                                  "baseline_fbg_mgdl", 
                                                  "updated_fbg_mgdl",# also has date + whether it is CBG or SMBG
                                                  "support_from_a4d",
                                                  "insulin_regimen",
                                                  "insulin_dosage", 
                                                  "testing_fqr", 
                                                  "required_insulin", 
                                                  "product_name", # hidden column for certain tabs....
                                                  "est_strips_pmoth",
                                                  "status")
                        
                        # # remove additional unwanted columns
                        # patient_df <- patient_df %>% select(-contains("hidden_column"))
                        # 
                        
                        # fix dates (split dates in cells)
                        patient_df <- patient_df %>% separate(updated_hba1c_prc, c("updated_hba1c_prc","updated_hba1c_date"), sep = "([(])")
                        patient_df <- patient_df %>% separate(updated_fbg_mgdl, c("updated_fbg_mgdl","updated_fbg_date"), sep = "([(])")
                        
                        
                        # fix updated_fbg_mgdl (split values with how blood sample was taken)
                        # patient_df <- patient_df %>% mutate(updated_fbg_sample = sub("^([[:alpha:]]*).*", "\\1", updated_fbg_mgdl),
                        #                                     updated_fbg_mgdl = gsub("[^0-9.-]", "", updated_fbg_mgdl))
                        
                        patient_df <- patient_df %>%
                                mutate(recruitment_date= as.POSIXct(as.numeric(recruitment_date)* (60*60*24),
                                                                    origin="1899-12-30",
                                                                    tz="GMT"),
                                       updated_hba1c_date = gsub(")", "", updated_hba1c_date),
                                       updated_fbg_date = gsub(")", "", updated_fbg_date) ,
                                       updated_fbg_date=  as.Date(as.yearmon(updated_fbg_date, "%b-%y")), 
                                       updated_hba1c_date= as.Date(as.yearmon(updated_fbg_date, "%b-%y")), 
                                       sheet_name = CurrSheet,
                                       tracker_mo = match(substr(CurrSheet, 1, 3),month.abb),
                                       tracker_year = year)
                        
                        
                        patient_df[patient_df == "NA"] <- NA 

                        
                } # 2018 tracker
                
                ####------------ 2019 PATIENT DATA ----------------------------###
                #### 2019 ####
                
                if (year == 2019) {
                        
                        
                        #Index (i.e., from which row to select the data)
                        i <- min(which(tracker_data[,2] %like% paste0(country_code, "_",clinic_code)))
                        j <- max(which(tracker_data[,2] %like%  paste0(country_code, "_",clinic_code)))
                        # selecting relevant rows
                        patient_df <- data.frame(tracker_data[i:j,])
                        
                        # 
                        # # remove unnecessary rows
                        # patient_df <- patient_df[-c(1:min(which(patient_df[,1] %like% country_code))),]
                        
                        
                        # remove certain columns (part of AN tab)
                        patient_df <- patient_df[,-c(1,3,4,6)]
                        
                        
                        
                        # THIS NEEDS TO BE THE SAME/HARMONIZED FOR ALL DATAFRAMES THAT WE WILL LATER COMBINE
                        # extracted manually from excel tracker, slightly edited the name, and then added via datapasta
                        if (ncol(patient_df) == 22 & !is.Date(patient_df[1,ncol(patient_df)])) {
                                
                                
                                colnames(patient_df)  <- c("id",
                                                           "gender",
                                                           "age",
                                                           "age_diagnosis",
                                                           "recruitment_date",
                                                           "baseline_hba1c_prc",
                                                           "updated_hba1c_prc",
                                                           "updated_hba1c_date",
                                                           "baseline_fbg_mldl",
                                                           "updated_fbg_mldl", # also has whether it is CBG or SMBG
                                                           "updated_fbg_date",
                                                           "blood_pressure_mmhg", # combines systolic and diastolic mmhg
                                                           "weight",
                                                           "height",
                                                           "bmi",
                                                           "bmi_date",
                                                           "edu_occ",
                                                           "hospitalisation",
                                                           "support_from_a4d",
                                                           "status",
                                                           "last_clinic_visit_date",
                                                           "opportunity_intervention")
                                
                        }
                        
                        
                        if (ncol(patient_df) == 25) {
                                
                                colnames(patient_df)  <- c("id",
                                                           "gender",
                                                           "age",
                                                           "age_diagnosis",
                                                           "recruitment_date",
                                                           "baseline_hba1c_prc",
                                                           "updated_hba1c_prc",
                                                           "updated_hba1c_date",
                                                           "baseline_fbg_mldl",
                                                           "updated_fbg_mldl", # also has whether it is CBG or SMBG
                                                           "updated_fbg_date",
                                                           "testing_fqr",
                                                           "insulin_regimen",
                                                           "blood_pressure_mmhg", # combines systolic and diastolic mmhg
                                                           "weight",
                                                           "height",
                                                           "bmi",
                                                           "bmi_date",
                                                           "edu_occ",
                                                           "hospitalisation",
                                                           "complications",
                                                           "family_support",
                                                           "support_from_a4d",
                                                           "status",
                                                           "last_clinic_visit_date")
                                
                        }
                        
                        
                        
                        if (ncol(patient_df) == 22 & is.Date(patient_df[1,ncol(patient_df)])) {
                                
                                
                                colnames(patient_df)  <- c("id",
                                                           "gender",
                                                           "age",
                                                           "age_diagnosis",
                                                           "recruitment_date",
                                                           "baseline_hba1c_prc",
                                                           "updated_hba1c_prc",
                                                           "updated_hba1c_date",
                                                           "baseline_fbg_mldl",
                                                           "updated_fbg_mldl", # also has whether it is CBG or SMBG
                                                           "updated_fbg_date",
                                                           "testing_fqr",
                                                           "insulin_regimen",
                                                           "blood_pressure_mmhg", # combines systolic and diastolic mmhg
                                                           "weight",
                                                           "height",
                                                           "bmi",
                                                           "bmi_date",
                                                           "edu_occ",
                                                           "support_from_a4d",
                                                           "status",
                                                           "last_clinic_visit_date")
                        }
                        
                        
                        #NOT OPTIMAL.... NEEDS SOME TWEEKING!!!!!!!
                        # fix updated_fbg_mldl (split values with how blood sample was taken)
                        # patient_df <- patient_df %>% mutate(updated_fbg_sample = sub("^([[:alpha:]]*).*", "\\1", updated_fbg_mldl),
                        #                                     updated_fbg_mldl = gsub("[^0-9.-]", "", updated_fbg_mldl))
                        # 
                        # 
                        # fix dates
                        patient_df <- patient_df %>%
                                mutate(recruitment_date = as.POSIXct(as.numeric(recruitment_date)* (60*60*24),
                                                                     origin="1899-12-30",
                                                                     tz="GMT"),
                                       last_clinic_visit_date= as.POSIXct(as.numeric(last_clinic_visit_date)* (60*60*24),
                                                                          origin="1899-12-30",
                                                                          tz="GMT"),
                                       bmi_date= as.POSIXct(as.numeric(bmi_date)* (60*60*24),
                                                            origin="1899-12-30",
                                                            tz="GMT"),
                                       bmi_date= format(as.Date(bmi_date), "%Y-%m"),
                                       updated_fbg_date= as.POSIXct(as.numeric(updated_fbg_date)* (60*60*24),
                                                                    origin="1899-12-30",
                                                                    tz="GMT"),
                                       updated_hba1c_date= as.POSIXct(as.numeric(updated_hba1c_date)* (60*60*24),
                                                                      origin="1899-12-30",
                                                                      tz="GMT"),
                                       updated_fbg_date = case_when(year(updated_fbg_date) < 100 ~ updated_fbg_date %m+% years(2000), TRUE ~ updated_fbg_date),
                                       updated_hba1c_date = case_when(year(updated_hba1c_date) < 100 ~ updated_hba1c_date %m+% years(2000), TRUE ~ updated_hba1c_date),
                                       sheet_name = CurrSheet,
                                       tracker_mo = match(substr(CurrSheet, 1, 3),month.abb),
                                       tracker_year = year) 
                        
                        
                        #  BMI fix
                        patient_df <- patient_df %>%
                                mutate(bmi = if_else(is.na(height) | is.na(weight), NA_character_, bmi))
                        
                        # fix blood pressure (split systolic and diastolic mmHg)
                        patient_df <- patient_df %>% separate(blood_pressure_mmhg, c("blood_pressure_sys_mmhg","blood_pressure_dias_mmhg"), sep = "([/])")
                        
                        
                        if (ncol(an_patient_data) == 6) {
                                an_patient_data <- an_patient_data[,-2]
                        }
                        
                        colnames(an_patient_data) <- c("id","patient_name", "province", "dob", "edu_occ")
                        
                        
                        # ADDING AN PATIENT DATA
                        patient_df <- patient_df %>% select(-edu_occ) %>% left_join(an_patient_data, by = "id")
                        
                        
                } # 2019 tracker
                
                ####------------ 2020 PATIENT DATA ----------------------------###
                #### 2020 ####
                
                if (year == 2020) {
                        
                        
                        
                        #Index (i.e., from which row to select the data)
                        i <- min(which(tracker_data$CLINIC.SUPPORT.PROGRAMME == "Summary of Patient Recruitment"))
                        
                        # selecting relevant rows
                        patient_df <- data.frame(tracker_data[i:nrow(tracker_data),])
                        # some column names are split
                        patient_df[1,] <- paste0(patient_df[1,], patient_df[2,]) 
                        
                        # rename column names (NOT VERY NICE with incomplete columns names as some name are split between two columns 
                        # (Updated HbA1c & Updated FBG & Latest Complication Screening))
                        names(patient_df) <- patient_df[1,]
                        patient_df <- patient_df[-c(1:3),]
                        names(patient_df) <- str_remove(names(patient_df), "NA")
                        
                        
                        
                        # THIS NEEDS TO BE THE SAME/HARMONIZED FOR ALL DATAFRAMES THAT WE WILL LATER COMBINE
                        # extracted manually from excel tracker, slightly edited the name, and then added via datapasta
                        col_names <- c("no",
                                       "patient_name",
                                       "province",
                                       "gender",
                                       "dob",
                                       "age",
                                       "age_diagnosis",
                                       "recruitment_date",
                                       "baseline_hba1c_prc",
                                       "updated_hba1c_prc",
                                       "updated_hba1c_date",
                                       "baseline_fbg_mldl",
                                       "updated_fbg_mldl", # also has whether it is CBG or SMBG
                                       "updated_fbg_date",
                                       "testing_fqr",
                                       "insulin_regimen",
                                       "blood_pressure_mmhg", # combines systolic and diastolic mmhg
                                       "weight",
                                       "height",
                                       "bmi",
                                       "bmi_date",
                                       "edu_occ",
                                       "hospitalisation",
                                       "support_from_a4d",
                                       "status",
                                       "last_clinic_visit_date",
                                       "additional_support")
                        
                        colnames(patient_df) <- col_names
                        
                        #NOT OPTIMAL.... NEEDS SOME TWEEKING!!!!!!!
                        # fix updated_fbg_mldl (split values with how blood sample was taken)
                        patient_df <- patient_df %>% mutate(updated_fbg_sample = sub("^([[:alpha:]]*).*", "\\1", updated_fbg_mldl),
                                                            updated_fbg_mldl = gsub("[^0-9.-]", "", updated_fbg_mldl))
                        
                        
                        # fix dates
                        patient_df <- patient_df %>%
                                mutate(dob= as.POSIXct(as.numeric(dob)* (60*60*24),
                                                       origin="1899-12-30",
                                                       tz="GMT"),
                                       recruitment_date= as.POSIXct(as.numeric(recruitment_date)* (60*60*24),
                                                                    origin="1899-12-30",
                                                                    tz="GMT"),
                                       last_clinic_visit_date= as.POSIXct(as.numeric(last_clinic_visit_date)* (60*60*24),
                                                                          origin="1899-12-30",
                                                                          tz="GMT"),
                                       bmi_date= as.POSIXct(as.numeric(bmi_date)* (60*60*24),
                                                            origin="1899-12-30",
                                                            tz="GMT"),
                                       bmi_date= format(as.Date(bmi_date), "%Y-%m"),
                                       updated_fbg_date= as.POSIXct(as.numeric(updated_fbg_date)* (60*60*24),
                                                                    origin="1899-12-30",
                                                                    tz="GMT"),
                                       updated_hba1c_date= as.POSIXct(as.numeric(updated_hba1c_date)* (60*60*24),
                                                                      origin="1899-12-30",
                                                                      tz="GMT"),
                                       updated_fbg_date = case_when(year(updated_fbg_date) < 100 ~ updated_fbg_date %m+% years(2000), TRUE ~ updated_fbg_date),
                                       updated_hba1c_date = case_when(year(updated_hba1c_date) < 100 ~ updated_hba1c_date %m+% years(2000), TRUE ~ updated_hba1c_date),
                                       tracker_year = year)  %>% 
                                mutate(clinic_code = clinic,
                                       country_code = country)
                        
                        
                        # fix blood pressure (split systolic and diastolic mmHg)
                        patient_df <- patient_df %>% separate(blood_pressure_mmhg, c("blood_pressure_sys_mmhg","blood_pressure_dias_mmhg"), sep = "([/])")
                        
                        
                        
                } # 2020 tracker
                
                
                ####------------2021 PATIENT DATA ----------------------------###
                #### 2021 ####
                
                
                if (year == 2021) {
                        
                        #Index (i.e., from which row to select the data)
                        i <- min(which(tracker_data$...2 == "Patient ID"))
                        
                        # selecting relevant rows
                        patient_df <- data.frame(tracker_data[i:nrow(tracker_data),])
                        # some column names are split
                        patient_df[1,] <- paste0(patient_df[1,], patient_df[2,]) 
                        
                        # rename column names (NOT VERY NICE with incomplete columns names as some name are split between two columns 
                        # (Updated HbA1c & Updated FBG & Latest Complication Screening))
                        names(patient_df) <- patient_df[1,]
                        patient_df <- patient_df[-c(1:2),]
                        names(patient_df) <- str_remove(names(patient_df), "NA")
                        
                        
                        
                        # THIS NEEDS TO BE THE SAME/HARMONIZED FOR ALL DATAFRAMES THAT WE WILL LATER COMBINE
                        # extracted manually from excel tracker, slightly edited the name, and then added via datapasta
                        col_names <- c("no",
                                       "id",
                                       "patient_name",
                                       "province",
                                       "gender",
                                       "dob",
                                       "age",
                                       "age_diagnosis",
                                       "recruitment_date",
                                       "baseline_hba1c_prc",
                                       "updated_hba1c_prc",
                                       "updated_hba1c_date",
                                       "baseline_fbg_mldl",
                                       "updated_fbg_mldl", # also has whether it is CBG or SMBG
                                       "updated_fbg_date",
                                       "testing_fqr",
                                       "insulin_regimen",
                                       "blood_pressure_mmhg",# combines systolic and diastolic mmhg
                                       "weight",
                                       "height",
                                       "bmi",
                                       "bmi_date",
                                       "edu_occ",
                                       "hospitalisation",
                                       "support_from_a4d",
                                       "status",
                                       "last_clinic_visit_date",
                                       "latest_complication_screening_type",
                                       "latest_complication_screening_date",
                                       "remarks",
                                       "additional_support")
                        
                        colnames(patient_df) <- col_names
                        
                        #NOT OPTIMAL.... NEEDS SOME TWEEKING!!!!!!!
                        # fix updated_fbg_mldl (split values with how blood sample was taken)
                        patient_df <- patient_df %>% mutate(updated_fbg_sample = sub("^([[:alpha:]]*).*", "\\1", updated_fbg_mldl),
                                                            updated_fbg_mldl = gsub("[^0-9.-]", "", updated_fbg_mldl))
                        
                        
                        # fix dates
                        patient_df <- patient_df %>%
                                mutate(dob= as.POSIXct(as.numeric(dob)* (60*60*24),
                                                       origin="1899-12-30",
                                                       tz="GMT"),
                                       recruitment_date= as.POSIXct(as.numeric(recruitment_date)* (60*60*24),
                                                                    origin="1899-12-30",
                                                                    tz="GMT"),
                                       last_clinic_visit_date= as.POSIXct(as.numeric(last_clinic_visit_date)* (60*60*24),
                                                                          origin="1899-12-30",
                                                                          tz="GMT"),
                                       bmi_date= as.POSIXct(as.numeric(bmi_date)* (60*60*24),
                                                            origin="1899-12-30",
                                                            tz="GMT"),
                                       bmi_date= format(as.Date(bmi_date), "%Y-%m"),
                                       latest_complication_screening_date= as.POSIXct(as.numeric(latest_complication_screening_date)* (60*60*24),
                                                                                      origin="1899-12-30",
                                                                                      tz="GMT"),
                                       updated_fbg_date = as.POSIXct(as.numeric(updated_fbg_date)* (60*60*24),
                                                                     origin="1899-12-30",
                                                                     tz="GMT"),
                                       updated_hba1c_date = as.POSIXct(as.numeric(updated_hba1c_date)* (60*60*24),
                                                                       origin="1899-12-30",
                                                                       tz="GMT"),
                                       updated_fbg_date = case_when(year(updated_fbg_date) < 100 ~ updated_fbg_date %m+% years(2000), TRUE ~ updated_fbg_date),
                                       updated_hba1c_date = case_when(year(updated_hba1c_date) < 100 ~ updated_hba1c_date %m+% years(2000), TRUE ~ updated_hba1c_date),
                                       tracker_year = year)  %>% 
                                mutate(clinic_code = clinic,
                                       country_code = country)
                        
                        
                        # fix blood pressure (split systolic and diastolic mmHg)
                        patient_df <- patient_df %>% separate(blood_pressure_mmhg, c("blood_pressure_sys_mmhg","blood_pressure_dias_mmhg"), sep = "([/])")
                        
                        
                } # 2021 tracker
                
                
                #### Save data ####
                # save data in a list
                tidy_tracker_list[[sheet_num]] <- patient_df %>% 
                        mutate(across(everything(), as.character)) # all data is converted as characters otherwise many errors emerge
                
                sheet_num <- sheet_num + 1        
                
                
        } # sheet for loop       
        
        
        # standard df, consistent for all tracker years
        standard_df <- tibble(
                patient_name = character(),
                province = character(),
                gender = character(),
                dob = character(),
                age = character(),
                age_diagnosis = character(),
                recruitment_date = character(),
                baseline_hba1c_prc = character(),
                updated_hba1c_prc = character(),
                updated_hba1c_date = character(),
                baseline_fbg_mgdl = character(),
                updated_fbg_mgdl = character(),
                updated_fbg_date = character(),
                support_from_a4d = character(),
                testing_fqr = character(),
                est_strips_pmoth = character(),
                status = character(),
                updated_fbg_sample = character(),
                tracker_year = character(),
                clinic_code = character(),
                country_code = character(),
                sheet_name = character(),
                insulin_regimen = character(),
                blood_pressure_sys_mmhg = character(),
                blood_pressure_dias_mmhg = character(),
                weight = character(),
                height = character(),
                bmi = character(),
                bmi_date = character(),
                edu_occ = character(),
                hospitalisation = character(),
                last_clinic_visit_date = character(),
                additional_support = character(),
                id = character(),
                latest_complication_screening_type = character(),
                latest_complication_screening_date = character(),
                remarks = character()
        )
        
        
        tidydata <- bind_rows(tidy_tracker_list) %>%
                bind_rows(standard_df)
        
        filename <- paste0("tracker_", unique(tidydata$country_code), "_", unique(tidydata$clinic_code), "_", unique(tidydata$tracker_year))
        
        tracker_info <- list(tidydata,filename)
        
        return(tracker_info)
        
}     






