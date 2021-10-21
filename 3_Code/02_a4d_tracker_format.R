


# DESCRIPTION -------------------------------------------------------------

#This function cleans the output of the "tidy" function by reformatting certain columns.
# Basic functions are created and then applied to each piece of data avaialbe in one row.



# PACKAGES ----------------------------------------------------------------

library(tidyverse)

# FUNCTIONS TO FIX DATA ---------------------------------------------------

# [5,8,11,14,30,33,37] dates ####     
# ______________________________________________
#### DATE OF BIRTH/RECRUITEMENT/UPDATED FBG/ UPDATED HB1AC/LAST CLINIC DATE
date_fix <- function(d) {
  d <- try(as.Date(parse_date_time(d)), silent = TRUE)
  if (class(d) == "try-error") {
    d  <- as.Date("9999","-", "99","-", "99") }
  
  return(d)
}


# [1] "no" ####                                
# [2] "patient_name" ####                      
# [3] "province" ####                          
# [4] "gender" ####                            
# [6] "age" ####                               
# [7] "age_diagnosis" ####  
# ______________________________________________
#### AGE DIAGNOSIS
# agediag_fix <- function(d) {
#   
#   if (grepl("birth", d)) {
#     d <- 0
#   }
#   else {
#     d <- try(as.numeric(d))
#     
#     if (class(d) == "try-error") {
#       
#       else {
#         d  <- 99999
#       }
#       
#     }
#   }
#   return(d)
# }


# [9,10] "hba1c_prc" ####    
# ______________________________________________
#### HBA1C 
hba1c_fix <- function(d) {
  d <- try(as.numeric(d), silent = TRUE)
  if (class(d) == "try-error") {
    d  <- 999999 }
  return(d)
}


# [12, 13] "fbg_mldl" ####  
# ______________________________________________
#### FBG 
fbg_fix <- function(d) {
  d <- try(as.numeric(d), silent = TRUE)
  if (class(d) == "try-error") {
    d  <- 999999 }
  return(d)
}


# [15] "support_from_a4d" ####   
# ______________________________________________
#### SUPPORT A4D
supporta4d_fix <- function(d) {
  d <- try(as.character(d), silent = TRUE)
  if (!d %in% c("Partial", "SAC", "Full")) {
    d  <- "999999" }
  return(d)
}


# [16] "testing_fqr" ####   
# ______________________________________________
#### TESTING FQR 
testfqr_fix <- function(d) {
  d <- try(as.numeric(d), silent = TRUE)
  if (class(d) == "try-error") {
    d  <- 999999 }
  return(d)
}


# [17] "est_strips_pmoth" ####  
# ______________________________________________
#### STRIPS NEEDED
strips_fix <- function(d) {
  d <- try(as.numeric(d), silent = TRUE)
  if (class(d) == "try-error") {
    d  <- 999999 }
  return(d)
}


# [18] "status" #### 
# ______________________________________________
#### STATUS
status_fix <- function(d) {
  d <- try(as.character(d), silent = TRUE)
  if (!d %in% c("Active", "Deseaced", "Discontinued")) {
    d  <- "999999" }
  return(d)
}


# [19] "updated_fbg_sample" ####
# ______________________________________________
#### UPDATED FBG SAMPLE
fbg_sample_fix <- function(d) {
  d <- try(as.character(d), silent = TRUE)
  if (!d %in% c("SMBG", "CBG")) {
    d  <- "999999" }
  return(d)
}


# [20] "tracker_year" ####                      
# [21] "clinic_code" ####                       
# [22] "country_code" ####                      
# [23] "sheet_name" ####                        
# [24] "insulin_regimen" #### 
# ______________________________________________
#### INSULIN REGIMEN
insulin_reg_fix <- function(d) {
  d <- try(as.character(d), silent = TRUE)
  if (!tolower(d) %in% c("basal-bolus", "premixed 30/70 bd", "	insulin pump")) {
    d  <- "999999" }
  return(d)
}


# [25] "blood_pressure_sys_mmhg" ####           
# [26] "blood_pressure_dias_mmhg" ####          
# [27] "weight" ####                            
# [28] "height" ####                            
# [29] "bmi" ####                               
# [31] "edu_occ" ####                           
# [32] "hospitalisation" ####                   
# [33] "last_clinic_visit_date" ####           
# [34] "additional_support" ####                
# [35] "id" ####                                
# [36] "latest_complication_screening_type" ####
# [38] "remarks" ####

























#### BLOOD PRESSURE SYS
# need to add a condition about reasonable values
#### BLOOD PRESSURE DYAS





# MAIN/WRAPPER FUNCTION ---------------------------------------------------

# row by row check the data + create 2 output tables: one with the actual clen values the hte other with the data checks

cleaning_a4d_tracker <- function(data) {
  
  data_c <- data
  # create a clean data frame
  data_c[,] <- NA

  for (i in 1:nrow(data)) {
    
    
    data_c$dob[i] <- date_fix(data[i,]$dob)
    data_c$updated_fbg_date[i] = date_fix(data[i,]$updated_fbg_date)
    data_c$updated_hba1c_date[i] = date_fix(data[i,]$updated_hba1c_date)
    data_c$bmi_date[i] = date_fix(data[i,]$bmi_date)
    data_c$recruitment_date[i] = date_fix(data[i,]$recruitment_date)
    data_c$last_clinic_visit_date[i] = date_fix(data[i,]$last_clinic_visit_date)
    data_c$latest_complication_screening_date[i] = date_fix(data[i,]$latest_complication_screening_date)
    
    # data[i,]$age_diagnosis = agediag_fix(data[i,]$age_diagnosis)
    
    data_c$updated_hba1c_prc[i] = hba1c_fix(data[i,]$updated_hba1c_prc)
    data_c$baseline_hba1c_prc[i] = hba1c_fix(data[i,]$baseline_hba1c_prc)
    
    data_c$updated_fbg_sample[i] = fbg_sample_fix(data[i,]$updated_fbg_sample)
    
    data_c$insulin_regimen[i] = insulin_reg_fix(data[i,]$insulin_regimen)
    
    
    
  }
  
  
  return(data_c)
}




# TEST --------------------------------------------------------------------

# reading the data frame

# Run test only if script is not sourced
if (!interactive()) {
  spec_cols <- cols(
    no = col_character(),
    patient_name = col_character(),
    province = col_character(),
    gender = col_character(),
    dob = col_character(),
    age = col_character(),
    age_diagnosis = col_character(),
    recruitment_date = col_character(),
    baseline_hba1c_prc = col_character(),
    updated_hba1c_prc = col_character(),
    updated_hba1c_date = col_character(),
    baseline_fbg_mldl = col_character(),
    updated_fbg_mldl = col_character(),
    updated_fbg_date = col_character(),
    support_from_a4d = col_character(),
    testing_fqr = col_character(),
    est_strips_pmoth = col_character(),
    status = col_character(),
    updated_fbg_sample = col_character(),
    tracker_year = col_character(),
    clinic_code = col_character(),
    country_code = col_character(),
    sheet_name = col_character(),
    insulin_regimen = col_character(),
    blood_pressure_sys_mmhg = col_character(),
    blood_pressure_dias_mmhg = col_character(),
    weight = col_character(),
    height = col_character(),
    bmi = col_character(),
    bmi_date = col_character(),
    edu_occ = col_character(),
    hospitalisation = col_character(),
    last_clinic_visit_date = col_character(),
    additional_support = col_character(),
    id = col_character(),
    latest_complication_screening_type = col_character(),
    latest_complication_screening_date = col_character(),
    remarks = col_character()
  )
  
  
  data <- read_csv("~/Desktop/A4DTracker_Overview.csv", col_types = spec_cols)
  
  
  testing <- cleaning_a4d_tracker(data = data)
}


