# DESCRIPTION -------------------------------------------------------------

# "reading_a4d_tracker" is a function that reads an excel workbook (.xlsx file) that contains a4d monthly trackers and creates a tidy dataframe with that data. 
# It takes into account all the sheets in the excel workbook that contain patient data, and binds these together. (e.g., if a workbook contains sheets Jan'18, Feb'18
#  and Mar'18, patient data from each sheet will be combined into one data frame)
# 
# INPUT ARGUMENTS: 
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


# FUNCTION TO READ THE A4D MONTHLY TRACKER --> PATIENT DATA --------------------------------------------------------
reading_a4d_tracker <- function(tracker_data_file, year, country, clinic) {
        
        
        # list the sheets in excel workbook & filter these (only select sheets with monthly data)
        sheet_list <- excel_sheets(tracker_data_file)
        sheet_list <-sheet_list[na.omit(pmatch(month.abb, sheet_list))]
        
        
        tidy_tracker_list <- NULL
        sheet_num <- 1
        
        
        for (CurrSheet in sheet_list) {
                
                tracker_data <- data.frame(read_xlsx(tracker_data_file, CurrSheet))
                
                
                ####------------2017 PATIENT DATA ----------------------------###
                #### 2017 ####
                
                if (year == 2017) {
                        #Index (i.e., from which row to select the data)
                        i <- min(which(tracker_data$PATIENT.ASSISTANCE.PROGRAMME == "No"))
                        j <- min(which(tracker_data$PATIENT.ASSISTANCE.PROGRAMME == "Suggestion/Feedback")) -1
                        # selecting relevant rows
                        patient_df <- data.frame(tracker_data[i:j,])
                        
                        # some columns are hidden
                        names(patient_df) <- patient_df[1,]
                        patient_df <- patient_df[-1,]
                        
                        
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
                                       "updated_hba1c_prc", # also has date
                                       "baseline_fbg_mldl", 
                                       "updated_fbg_mldl",# also has date + whether it is CBG or SMBG
                                       "support_from_a4d",
                                       "insulin_regimen",# hidden col
                                       "insulin_dosage", # hidden col
                                       "testing_fqr", 
                                       "required_insulin", # hidden col
                                       "NA", # hidden col
                                       "est_strips_pmoth",
                                       "status")
                        
                        colnames(patient_df) <- col_names
                        
                        
                        
                        # fix dates (split dates in cells)
                        patient_df <- patient_df %>% separate(updated_hba1c_prc, c("updated_hba1c_prc","updated_hba1c_date"), sep = "([(])")
                        patient_df <- patient_df %>% separate(updated_fbg_mldl, c("updated_fbg_mldl","updated_fbg_date"), sep = "([(])")
                        
                        #NOT OPTIMAL.... NEEDS SOME TWEEKING!!!!!!!
                        # fix updated_fbg_mldl (split values with how blood sample was taken)
                        patient_df <- patient_df %>% mutate(updated_fbg_sample = sub("^([[:alpha:]]*).*", "\\1", updated_fbg_mldl),
                                                            updated_fbg_mldl = gsub("[^0-9.-]", "", updated_fbg_mldl))
                        
                        
                        patient_df <- patient_df %>%
                                mutate(dob= as.POSIXct(as.numeric(dob)* (60*60*24),
                                                       origin="1899-12-30",
                                                       tz="GMT"),
                                       recruitment_date= as.POSIXct(as.numeric(recruitment_date)* (60*60*24),
                                                                    origin="1899-12-30",
                                                                    tz="GMT"),
                                       updated_hba1c_date = gsub(")", "", updated_hba1c_date),
                                       updated_fbg_date = gsub(")", "", updated_fbg_date),
                                       updated_fbg_date =  as.Date(updated_fbg_date, format =  "%d/%m/%Y"),
                                       updated_hba1c_date =  as.Date(updated_hba1c_date, format =  "%d/%m/%Y"),
                                       updated_fbg_date = case_when(year(updated_fbg_date) < 100 ~ updated_fbg_date %m+% years(2000), TRUE ~ updated_fbg_date),
                                       updated_hba1c_date = case_when(year(updated_hba1c_date) < 100 ~ updated_hba1c_date %m+% years(2000), TRUE ~ updated_hba1c_date),
                                       tracker_year = year) %>% 
                                select(-c("insulin_regimen",
                                          "insulin_dosage", 
                                          "required_insulin", 
                                          "NA"))  %>%  # removing hidden columns
                                mutate(clinic_code = clinic,
                                       country_code = country)
                        
                        
                } # 2017 tracker
                
                ####------------ 2018 PATIENT DATA ----------------------------###
                # 2018 ####
                if (year == 2018) {
                        
                        #Index (i.e., from which row to select the data)
                        i <- min(which(tracker_data$PATIENT.ASSISTANCE.PROGRAMME == "No"))
                        j <- min(which(tracker_data$PATIENT.ASSISTANCE.PROGRAMME == "Suggestion/Feedback")) -1
                        # selecting relevant rows
                        patient_df <- data.frame(tracker_data[i:j,])
                        
                        # some columns are hidden
                        names(patient_df) <- patient_df[1,]
                        patient_df <- patient_df[-1,]
                        
                        
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
                                       "updated_hba1c_prc", # also has date
                                       "baseline_fbg_mldl", 
                                       "updated_fbg_mldl",# also has date + whether it is CBG or SMBG
                                       "support_from_a4d",
                                       "insulin_regimen",# hidden col
                                       "insulin_dosage", # hidden col
                                       "testing_fqr", 
                                       "required_insulin", # hidden col
                                       "NA", # hidden col
                                       "est_strips_pmoth",
                                       "status")
                        
                        colnames(patient_df) <- col_names
                        
                        
                        
                        # fix dates (split dates in cells)
                        patient_df <- patient_df %>% separate(updated_hba1c_prc, c("updated_hba1c_prc","updated_hba1c_date"), sep = "([(])")
                        patient_df <- patient_df %>% separate(updated_fbg_mldl, c("updated_fbg_mldl","updated_fbg_date"), sep = "([(])")
                        
                        #NOT OPTIMAL.... NEEDS SOME TWEEKING!!!!!!!
                        # fix updated_fbg_mldl (split values with how blood sample was taken)
                        patient_df <- patient_df %>% mutate(updated_fbg_sample = sub("^([[:alpha:]]*).*", "\\1", updated_fbg_mldl),
                                                            updated_fbg_mldl = gsub("[^0-9.-]", "", updated_fbg_mldl))
                        
                        
                        patient_df <- patient_df %>%
                                mutate(dob= as.POSIXct(as.numeric(dob)* (60*60*24),
                                                       origin="1899-12-30",
                                                       tz="GMT"),
                                       recruitment_date= as.POSIXct(as.numeric(recruitment_date)* (60*60*24),
                                                                    origin="1899-12-30",
                                                                    tz="GMT"),
                                       updated_hba1c_date = gsub(")", "", updated_hba1c_date),
                                       updated_fbg_date = gsub(")", "", updated_fbg_date),
                                       updated_fbg_date=  as.Date(as.yearmon(updated_fbg_date, "%b-%y")), # need to find a way to make it work
                                       updated_hba1c_date=  as.Date(updated_hba1c_date, format =  "%d/%m/%Y"), 
                                       updated_fbg_date = case_when(year(updated_fbg_date) < 100 ~ updated_fbg_date %m+% years(2000), TRUE ~ updated_fbg_date),
                                       updated_hba1c_date = case_when(year(updated_hba1c_date) < 100 ~ updated_hba1c_date %m+% years(2000), TRUE ~ updated_hba1c_date),
                                       tracker_year = year) %>% 
                                select(-c("insulin_regimen",
                                          "insulin_dosage", 
                                          "required_insulin", 
                                          "NA"))  %>% 
                                mutate(clinic_code = clinic,
                                       country_code = country)
                        
                } # 2018 tracker
                
                ####------------ 2019 PATIENT DATA ----------------------------###
                #### 2019 ####
                
                if (year == 2019) {
                        
                        #Index (i.e., from which row to select the data)
                        i <- min(which(tracker_data$CLINIC.SUPPORT.PROGRAMME == "No"))
                        
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
                        mutate(sheet_name = CurrSheet) %>%
                        mutate(across(everything(), as.character)) # all data is converted as characters otherwise many errors emerge
                
                sheet_num <- sheet_num + 1        
                
                
        } # sheet for loop       
        
        
        # standard df, consistent for all tracker years
        standard_df <- tibble(
                no = character(),
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
                baseline_fbg_mldl = character(),
                updated_fbg_mldl = character(),
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
                bind_rows(standard_df) %>% 
                filter(!is.na(dob))# STILL NEED TO FIND A BETTER WAY REMOVE ROWS WITH NO DATA
        
        return(tidydata)
        
}     






