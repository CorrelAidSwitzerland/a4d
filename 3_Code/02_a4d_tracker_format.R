


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
# No Check

# [3] "province" ####                          
# TODO: Check needed?

# [4] "gender" ####     
# TODO: Check for F/M, female/male ..

# [6] "age" ####               
# TODO: Double check age by taking dif between birth & last visit date

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
# TODO: Check whether values are realistic, what units are used
hba1c_fix <- function(d) {
  d <- try(as.numeric(d), silent = TRUE)
  if (class(d) == "try-error") {
    d  <- 999999 }
  return(d)
}


# [12, 13] "fbg_mldl" ####  
# ______________________________________________
#### FBG 
# TODO: Check whether values are realistic, what units are used
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
# TODO: Integer values make sense, why 3.2 ... ?

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
# TODO: Replace "" values with NA
fbg_sample_fix <- function(d) {
  d <- try(as.character(d), silent = TRUE)
  if (!d %in% c("SMBG", "CBG")) {
    d  <- "999999" }
  return(d)
}


# [20] "tracker_year" ####   
# TODO: Read out single year, check that all years are the same and match the input of the year function?

# [21] "clinic_code" ####                       
# TODO: When we have data, using list of clinics to double check?

# [22] "country_code" ####                      
# TODO: When we have data, using list of countries to double check?


# [23] "sheet_name" ####                        
# TODO: Transform to "tracker_month"

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
# TODO: Check realistic values and filter based on that


# [26] "blood_pressure_dias_mmhg" ####          
# TODO: Check realistic values and filter based on that

# [27] "weight" ####                            
# TODO: Check realistic values and filter based on that


# [28] "height" ####    
# TODO: Check realistic values and filter based on that

# [29] "bmi" ####      
# TODO: Check realistic values and filter based on that

# [31] "edu_occ" ####         
# TODO: match Thai words with englisch:
# ประถมศึกษาปีที่= elementary
# อนุบาล=kindergarden
# Take years behind in consideration, e.g."ประถมศึกษาปีที่ 4" 


# [32] "hospitalisation" ####         
# TODO: Check column in detail, very complex date column with outwritten text, set "NA" to NA

# [34] "additional_support" ####   
# ?

# [35] "id" ####                                
# what id?

# [36] "latest_complication_screening_type" ####
# take as chr?

# [38] "remarks" ####
# take as chr?


#### BLOOD PRESSURE SYS
# TODO: need to add a condition about reasonable values
#### BLOOD PRESSURE DYAS





# MAIN/WRAPPER FUNCTION ---------------------------------------------------

# Process: Check data rowwise
# Output 1: Cleaned data table
# Output 2: Table containing data checklist

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


# Run test only if script is not sourced
if (!interactive()) {
  # reading the data frame
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


