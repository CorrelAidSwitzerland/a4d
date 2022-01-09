


# DESCRIPTION -------------------------------------------------------------

#This function cleans the output of the "tidy" function by reformatting certain columns.
# Basic functions are created and then applied to each piece of data avaialbe in one row.





# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(readr)


# Base Functions ####

check_numeric_borders <- function(vector,
                                  max,
                                  min){
  vector <- as.numeric(vector)
  vector <- ifelse(vector >= max,
                   NA,
                   vector)
  vector <- ifelse(vector < min,
                   NA,
                   vector)
}

replace_empty_string_with_NA <- function(string_vector){
  output <- ifelse(string_vector == "", NA, string_vector)
}




# FUNCTIONS TO FIX DATA ---------------------------------------------------

#### [5,8,11,14,30,33,37] dates #####
# ______________________________________________
#### DATE OF BIRTH/RECRUITEMENT/UPDATED FBG/ UPDATED HB1AC/LAST CLINIC DATE
fix_date_cols <- function(d) {
  d <- try(as.Date(d, format="%Y-%m-%d"), silent = TRUE)
  if (class(d) == "try-error") {
    d  <- as.Date("9999","-", "99","-", "99") }
  
  return(d)
}


# [1,2,3] String IDs without NAs  ####               
fix_chr_without_NAs <- function(d) {
  na_exist_in_d <- try(any(is.na(d)), silent = TRUE)
  if (class(d) == "try-error" | na_exist_in_d) {
    d  <- 999999 }
  return(d)
}

# [4] "gender" ####  
# ______________________________________________


## Synonyms for gender
par_synonyms_lower_female <- c("female", "girl", "woman", "fem", "feminine", "f")
par_synonyms_lower_male <- c("male", "boy", "man", "masculine", "m")

replace_gender_synonyms <- function(x, 
                                    synonyms_f = par_synonyms_lower_female,
                                    synonyms_m = par_synonyms_lower_male){
  y <- case_when(
    tolower(x) %in% synonyms_f ~ "F",
    tolower(x) %in% synonyms_m ~ "M",
    TRUE ~ "Other"
  )
}

fix_gender <- function(d) {
  cleaned_gender <- try(replace_gender_synonyms(d), silent = TRUE)
  if (class(cleaned_gender) == "try-error") {
    cleaned_gender  <- 999999 }
  return(cleaned_gender)
}




# [6] "age" ####               
# ______________________________________________
# TODO: Double check age by taking dif between birth date & last visit date

fix_age <- function(x){
  x <- try(as.numeric(x), silent = TRUE)
  if (class(x) == "try-error") {
    x <- 999999 }
  return(x)
}


# [7] "age_diagnosis" ####
# ______________________________________________
#### AGE DIAGNOSIS
# TODO: Improve error handling by taking care of "1y 6m"
fix_age_diagnosis <- function(d) {

  if (grepl("birth", d)) {
    d <- 0
  } else {
    d <- try(as.numeric(d))
    if (class(d) == "try-error") {
        d <- 99999
        }
    }
  return(d)
}


# [9,10] "hba1c_prc" ####
# ______________________________________________
#### HBA1C 
# TODO Operationalization: Show where NA values exist

# Set realistic hb1c values [%]
par_lower_hb1c <- 2
par_upper_hb1c <- 30

# Functions for replacement
exclude_unrealistic_hba1c <- function(x,
                                      lower_hb1c,
                                      upper_hb1c){
  x <- as.numeric(x)
  y <- replace(x, (x < lower_hb1c | x > upper_hb1c), NA)
}

fix_hba1c <- function(d) {
  d <- try(exclude_unrealistic_hba1c(d, par_lower_hb1c, par_upper_hb1c), 
           silent = TRUE)
  if (class(d) == "try-error") {
    d  <- 999999 }
  return(d)
}


# [12, 13] "fbg_mldl" ####  
# ______________________________________________
# Apply to baseline and updated values

#### FBG 
# TODO Operationalization: Show where NA values exist
# TODO: Include in final function
# Todo: Validate that correct hospital and country ids were used to assign units (see Tyla Mail 05/11/21)
# TODO: FBG value input have two values e.g. "120-145". Column needs to be transformed to make
#       correct sanity checks and transformation.


# Assign countries & hospitals to unit of fbg measurement
mmol_countries <- c()
mmol_hospitals <- c(
  "Clinic_LU", "Clinic_PE", "Clinic_YA", "Clinic_PU")
mg_countries <- c(
  "Country_1", "Country_2", "Country_3")
mg_hospitals <- c(
  "Clinic_DW", "Clinic_TN", "Clinic_EO", "Clinic_VF", "Clinic_BR",
  "Clinic_KH", "Clinic_XD", "Clinic_QG", "Clinic_YB", "Clinic_FY",
  "Clinic_CJ", "Clinic_VW", "Clinic_IX", "Clinic_YA", "Clinic_ZB",
  "Clinic_EU", "Clinic_IH"
)

# @Description: For a given country/hospital the fbg unit is returned
# @country_id: ID of the country where patient values were taken
# @hospital_id: ID of the hospital where patient values were taken
# @Output: String with fbg unit "mg/dL", "mmol/L" or NA
assign_fbg_unit_per_hospital <- function(
  hospital_id, country_id,
  mmol_ct = mmol_countries,
  mmol_hos = mmol_hospitals,
  mg_ct = mg_countries,
  mg_hos = mg_hospitals){
  
  returned_unit <- case_when(
    is.na(hospital_id) & is.na(country_id) ~ NA,
    hospital_id %in% mg_hos ~ "mg/dL",
    country_id %in% mg_ct ~ "mg/dL",
    hospital_id %in% mmol_hos ~ "mmol/L",
    country_id %in% mmol_ct ~ "mmol/L",
    TRUE ~ NA
  )
  
  if(is.na(returned_unit)){
    warning("FBG unit used by hospital could not be matched ~ Assumed to be mmol/L")
  }
  return(returned_unit)
}

# @Description: Makes sure FBG values are in mmol/L
# @fbg: Any fbg value in mg/dL or mmol/L
# @country_id: ID of the country where patient values were taken
# @hospital_id: ID of the hospital where patient values were taken
# @Output: FBG value in mmol/L or NA if not matched
transform_fbg_in_mg <- function(fbg, country_id, hospital_id) {
  
  factor_mmol_in_mg <- 18.02
  measure_unit <- assign_fbg_unit_per_hospital(country_id = country_id, 
                                               hospital_id = hospital_id)
  
  fbg_mmol <- case_when(
    measure_unit == "mg/dL" ~ fbg / factor_mml_in_mg,
    measure_unit == "mmol/L" ~ fbg ,
    is.na(measure_unit) ~ fbg,
    TRUE ~ NA
  )
  
  return(as.numeric(fbg_mmol))
}


fbg_mmol_lower_bound <- 0
fbg_mmol_upper_bound <- 136.5 # https://www.cleveland19.com/story/1425584/ohio-man-holds-world-record-of-highest-blood-sugar/

# @Description: Check if FBG value is realistic
# @fbg_mmol: FBG value in mmol/L
# @fbg_min, fbg_max: Lower and upper bound of realistic fbg values
# @Output: FBG mmol value if no error. Otherwise raised error & NA
sanity_check_fbg_mmol <- function(fbg_mmol, min_fbg = fbg_mmol_lower_bound,
                                max_fbg = fbg_mmol_upper_bound) {
  fbg_result <- case_when(
    fbg_mmol >= min_fbg & fbg_mmol <= max_fbg ~ fbg_mmol,
    TRUE ~ NA
  )
  
  if(is.na(fbg_result)){
    stop("ERROR: FBG value outside realistic scale")
  }
  
  return(fbg_result)
}


fbg_fix <- function(fbg, country, hospital) {
  d <- try(sanity_check_fbg_mmol(transform_fbg_in_mg(
    fbg, country_id = country, hospital_id = hospital)),
    silent = TRUE)
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

# If ranges, take mean
replace_testfqr_strings_mean <- function(x){
  y <- unlist(map(str_split(x, pattern = "-"), 
           function(z) mean(as.numeric(z))))
}

testfqr_fix <- function(d) {
  d <- try(replace_testfqr_strings_mean(d), silent = TRUE)
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
fix_fbg_sample <- function(d) {
  d <- try(replace_empty_string_with_NA(as.character(d)), silent = TRUE)
  if (!d %in% c("SMBG", "CBG")) {
    d  <- "999999" }
  return(d)
}


# [20] "tracker_year" ####   
# TODO: Read out single year, check that all years are the same and match the input of the year function?

# [21] "clinic_code" ####                       
# TODO: When we have data, using list of clinics to double check?
# TODO Operationalization: Read out clinic table in database to check data with

# [22] "country_code" ####                      
# TODO: When we have data, using list of countries to double check?
# TODO Operationalization: Read out clinic table in database to check data with


# [23] "sheet_name" ####                        
# TODO: Include in final function

parse_sheet_name <- function(x){
  y <- unlist(map(as.character(x), function(z)
    (format(readr::parse_date(z,"%b'%y"), format = "%Y-%m"))))
}

transform_sheet_name_to_tracker_month <- function(x) {
  x <- try(parse_sheet_name(x), silent = TRUE)
  if (class(x) == "try-error") {
    x <- "999999" }
  return(x)
}


# [24] "insulin_regimen" #### 
# ______________________________________________
#### INSULIN REGIMEN
fix_insulin_reg <- function(d) {
  d <- try(as.character(d), silent = TRUE)
  if (!tolower(d) %in% c("basal-bolus", "premixed 30/70 bd", "	insulin pump")) {
    d  <- "999999" }
  return(d)
}


# [25] "blood_pressure_sys_mmhg" #### 
# ______________________________________________   
par_highest_blood_pressure_sys <- 250
par_lowest_blood_pressure_sys <- 20

fix_blood_pressure_sys <- function(x) {
  x <- try(check_numeric_borders(
    x,
    par_highest_blood_pressure_sys,
    par_lowest_blood_pressure_sys), silent = TRUE)
  if (class(x) == "try-error") {
    x <- "999999" }
  return(x)
}

# [26] "blood_pressure_dias_mmhg" ####    
# ______________________________________________      

par_highest_blood_pressure_dias <- 220
par_lowest_blood_pressure_dias <- 20

fix_blood_pressure_dias <- function(x) {
  x <- try(check_numeric_borders(
    x,
    par_highest_blood_pressure_dias,
    par_lowest_blood_pressure_dias), silent = TRUE)
  if (class(x) == "try-error") {
    x <- "999999" }
  return(x)
}

# [27] "weight" ####             
# ______________________________________________               
par_max_weight_kg <- 200
par_min_weight_kg <- 0

fix_weight <- function(x) {
  x <- try(check_numeric_borders(x, par_max_weight_kg, par_min_weight_kg), 
           silent = TRUE)
  if (class(x) == "try-error") {
    x <- "999999" }
  return(x)
}


# [28] "height" ####    
# ______________________________________________
par_max_height <- 200
par_min_height <- 0

transform_cm_to_m <- function(height){
  height <- as.numeric(height)
  height <- ifelse(height > 50,
                   height/100,
                   height)
}

fix_height <- function(x) {
  x <- try(
    check_numeric_borders(
      transform_cm_to_m(x), 
      par_max_height, par_min_height), 
           silent = TRUE)
  if (class(x) == "try-error") {
    x <- "999999" }
  return(x)
}

# [29] "bmi" ####      
# ______________________________________________
par_max_bmi <- 60
par_min_bmi <- 4

replace_NA_bmi <- function(bmi_vector,
                           height_vector,
                           weight_vector){
  calc_bmi <- as.numeric(weight_vector) / (height_vector^2)
  
  output <- ifelse(is.na(bmi_vector), calc_bmi, bmi_vector)
}

fix_bmi <- function(x, fixed_weight_kg, fixed_height_m) {
  x <- try(check_numeric_borders(x, par_max_bmi, par_min_bmi), 
           silent = TRUE)
  if (class(x) == "try-error") {
    x <- "999999" }
  return(x)
}


# [31] "edu_occ" ####         
# ______________________________________________
# TODO: match Thai words with englisch:
# ประถมศึกษาปีที่= elementary
# อนุบาล=kindergarden
# Take years behind in consideration, e.g."ประถมศึกษาปีที่ 4" 


# [32] "hospitalisation" ####     
# ______________________________________________    
# TODO: Check column in detail, very complex date column with outwritten text, set "NA" to NA

# [34] "additional_support" ####   
# ______________________________________________
# ?

# [35] "id" ####                  
# ______________________________________________              
# what id?

# [36] "latest_complication_screening_type" ####
# ______________________________________________
# take as chr?

# [38] "remarks" ####
# ______________________________________________
# take as chr?



# TODO: Add checks & new columns for DC_V2_Anon Example csv



# MAIN/WRAPPER FUNCTION ---------------------------------------------------

# Process: Check data rowwise
# Output 1: Cleaned data table
# Output 2: Table containing data checklist

cleaning_a4d_tracker <- function(data) {
  
  
  
  # TODO: Transform for loop by dplyr::mutate
  
  data_c <- data
  # create a clean data frame
  data_c[,] <- NA

  for (i in 1:nrow(data)) {
    
    # TODO: Check if [i,]$column can be replcaed by $column[i]
    
    # Dates
    data_c$dob[i] <- fix_date_cols(data$dob[i])
    data_c$updated_fbg_date[i] = fix_date_cols(data$updated_fbg_date[i])
    data_c$updated_hba1c_date[i] = fix_date_cols(data$updated_hba1c_date[i])
    
    # TODO: Rework function to capture bmi date being "2020-12" not "2020-12-02"
    data_c$bmi_date[i] = fix_date_cols(data$bmi_date[i])
    data_c$recruitment_date[i] = fix_date_cols(data$recruitment_date[i])
    data_c$last_clinic_visit_date[i] = fix_date_cols(data$last_clinic_visit_date[i])
    data_c$latest_complication_screening_date[i] = fix_date_cols(
      data$latest_complication_screening_date[i])
    
    # Static patient information
    # TODO: How are Hospital ID, Country ID and general ID captured?
    data_c$id[i] <- fix_chr_without_NAs(data$id[i])
    data_c$patient_name[i] <- fix_chr_without_NAs(data$patient_name[i])
    data_c$province[i] <- fix_chr_without_NAs(data$province[i])
    
    data_c$gender[i] <- fix_gender(data$gender[i])
    data_c$age[i]    <- fix_age(data$age[i])
    data$age_diagnosis[i] <- fix_age_diagnosis(data$age_diagnosis[i])
    
    # Dynamic body information
    data_c$height[i] <- fix_height(data$height[i])
    data_c$weight[i] <- fix_weight(data$weight[i])
    data_c$bmi[i]    <- fix_bmi(data$bmi[i], data_c$weight[i], data_c$height[i] )
    
    # Blood values
    data_c$updated_hba1c_prc[i]  <- fix_hba1c(data$updated_hba1c_prc[i])
    data_c$baseline_hba1c_prc[i] <- fix_hba1c(data$baseline_hba1c_prc[i])
    
    data_c$insulin_regimen[i]    <- fix_insulin_reg(data$insulin_regimen[i])
    
    # FBG
    # Need to take care of two values "120-152" instead of one "125"
    # data_c$updated_fbg_mmoll <- fbg_fix(data_c$updated_fbg_mldl,
    #                                     country  = data_c$country_id,
    #                                     hospital = data_c$hospital_id)
    # Need to do the same for FBG baseline
    # data_c$baseline_fbg_mmoll <- fbg_fix(data_c$baseline_fbg_mldl,
    #                                     country  = data_c$country_id,
    #                                     hospital = data_c$hospital_id)
    data_c$updated_fbg_sample[i] <- fix_fbg_sample(data$updated_fbg_sample[i])
    
    data_c$blood_pressure_dias_mmhg[i] <- fix_blood_pressure_dias(
      data$blood_pressure_dias_mmhg[i])
    
    data_c$blood_pressure_sys_mmhg[i] <- fix_blood_pressure_sys(
      data$blood_pressure_sys_mmhg[i])
    
    # Other
    data_c$support_from_a4d[i] <- supporta4d_fix(data$support_from_a4d[i])
    data_c$testing_fqr[i] <- testfqr_fix(data$testing_fqr[i])
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



# TODOs:
## 1. Variables until [16] testing_fqr are part of the final wrapper. EVerything afterwards
#      (and buggy ones lige age_diagnosis) need to be finalized and added to the wrapper
## 2. Final check if all variables have been transformed correctly


