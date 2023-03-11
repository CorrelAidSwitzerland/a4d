


# DESCRIPTION -------------------------------------------------------------

#This function cleans the output of the "tidy" function by reformatting certain columns.
# Basic functions are created and then applied to each piece of data avaialbe in one row.



# dat <- read.csv("/Volumes/A4D_project/clean_a4d_data.csv")

# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)


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



mm_yyyy_transform <- function(d) {
  yr <- as.numeric(gsub('.*-([0-9]+).*','\\1',d)) 
  
  
  if (yr < 100) {yr <- yr + 2000}
  
  yr <- as.character(yr)
  
  month <- sub("^([[:alpha:]]*).*", "\\1", d)
  
  m <- match(month,month.abb)
  
  d <- try(as.Date(paste(yr,m, "01", sep = "-"), format="%Y-%m-%d"), silent = TRUE)
  if (class(d) == "try-error") {
    d  <- as.Date("9999","-", "99","-", "99")
  }
  
  return(d)
}


date_transform <- function(d) {
  da <- try(as.Date(d), silent = TRUE)
  if (class(da) == "try-error") {
    da <- try(as.Date(as.numeric(d), origin = "1899-12-30"), silent = TRUE)
    if (class(da) == "try-error") {
      da  <- as.Date("9999","-", "99","-", "99") 
      
      if (lubridate::year(da)< 17) {lubridate::year(da) <- lubridate::year(da) + 2000}
      
    }
  }
  
  return(da)
} 



# FUNCTIONS TO FIX DATA ---------------------------------------------------


# dates -------------------------------------------------------------------


fix_date_cols <- function(d) {
  
  if (!is.na(d)){
    
    d <- gsub(".*[(]|[)].*", "", d)
    
    categ <- any(str_detect(d,c(letters,LETTERS)))
    
    
    
    if(categ == T) {
      
      d <- mm_yyyy_transform(d)
      
    }
    
    else {
      
      d <-date_transform(d)
      
    }
    
  }
  else{
    d <- NA
  }
  
  return(d)
}



# gender ####  

## Synonyms for gender
par_synonyms_lower_female <- c("female", "girl", "woman", "fem", "feminine", "f")
par_synonyms_lower_male <- c("male", "boy", "man", "masculine", "m")

replace_gender_synonyms <- function(d, 
                                    synonyms_f = par_synonyms_lower_female,
                                    synonyms_m = par_synonyms_lower_male){
  y <- case_when(
    tolower(d) %in% synonyms_f ~ "F",
    tolower(d) %in% synonyms_m ~ "M",
    TRUE ~ "Other"
  )
}

fix_gender <- function(d) {
  cleaned_gender <- try(replace_gender_synonyms(d), silent = TRUE)
  if (class(cleaned_gender) == "try-error") {
    cleaned_gender  <- 999999 }
  return(cleaned_gender)
}




# age ####                
# ______________________________________________
# TODO: Double check age by taking dif between birth date & last visit date

fix_age <- function(d){
  d <- try(as.numeric(d), silent = TRUE)
  if (class(d) == "try-error") {
    d <- 999999 }
  return(d)
}


# age_diagnosis ####       
# ______________________________________________


# Description: Transforms "1 y 6m" to "1.5" age, if not possible return NA
extract_age_from_y_m <- function(age){
  final_age <- NA_real_
  suppressWarnings(
    if(is.na(as.numeric(age))){
      age_str <- str_split(age, " |y|m") %>%
        unlist()
      age_str <- subset(age_str, age_str != "")
      
      if(length(age_str) > 1){
        years <- as.numeric(age_str[1])
        months <- mean(as.numeric(age_str[-1]))
      } else {
        years <- age_str
        months <- 0
      }
      
      final_age <- years + (round(months / 12, 1))
    }
  )
  return(final_age)
  
}

# @Description: Checks different possibilities for age of birth and transforms it into numeric age
handle_age_diagnosis <- function(age_x){
  
  suppressWarnings(
    age_corrected <- ifelse(
      grepl("birth|born", tolower(age_x)),
      0,
      ifelse(!is.na(as.numeric(age_x)),
             as.numeric(age_x),
             extract_age_from_y_m(age_x)
      )
    )
  )
  return(age_corrected)
}

# @Description: Try to transform age of diagnosis. If error occur give out 99999
fix_age_diagnosis <- function(d) {
  
  if(!is.na(d)){
    
    
    d <- try(handle_age_diagnosis(d), silent = TRUE)
    if (class(d) == "try-error") {
      d <- 99999
    }
    
  } else {d <- NA}
  
  return(d)
}


# hba1c_prc ####
# ______________________________________________

# TODO Operationalization: Show where NA values exist DONE!

# Set realistic hb1c values [%]
par_lower_hb1c <- 2
par_upper_hb1c <- 30

# Functions for replacement
exclude_unrealistic_hba1c <- function(x,
                                      lower_hb1c,
                                      upper_hb1c){
  # cases where there is a comma and not a dot
  x <- str_replace(x , ",", ".")
  # cases with signs
  x <- ifelse(grepl(c("<|>"), x), "999999", x)
  x <- as.numeric(x)
  d <- ifelse((x < lower_hb1c | x > upper_hb1c), 999999, x)
  
  return(d)
}

fix_hba1c <- function(d) {
  
  if(!is.na(d)){
    
    d <- try(exclude_unrealistic_hba1c(d, par_lower_hb1c, par_upper_hb1c), 
             silent = TRUE)
    if (class(d) == "try-error") {
      d  <- 999999 }
    
  }
  
  else {
    d <- NA
  }
  
  return(d)
}


# [12, 13] "fbg_mldl" ####  
# ______________________________________________
# Apply to baseline and updated values

#### FBG 
# TODO Operationalization: Show where NA values exist


# @TODO when loading real data:
# Before running the code you need to assign the hospitals and countries
# which use mmol or mg.
# Go through the data and check the column headers of the respective columns.
# Since we don't have all the real data yet, this needs to be configured.


# Assign countries & hospitals to unit of fbg measurement
# These info are done for the fake data only.
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
    is.na(hospital_id) & is.na(country_id) ~ NA_character_,
    hospital_id %in% mg_hos ~ "mg/dL",
    country_id %in% mg_ct ~ "mg/dL",
    hospital_id %in% mmol_hos ~ "mmol/L",
    country_id %in% mmol_ct ~ "mmol/L",
    TRUE ~ NA_character_
  )
  
  if(is.na(returned_unit)){
    warning("FBG unit used by hospital could not be matched ~ Assumed to be mmol/L. Check if allocation of real hospitals to mg/mmol unit was performed within the code (See 02_a4d_patient_tracker_format.R fbg_mgdl.")
  }
  return(returned_unit)
}

# @Description: Makes sure FBG values are in mmol/L
# @fbg: Any fbg value in mg/dL or mmol/L
# @country_id: ID of the country where patient values were taken
# @hospital_id: ID of the hospital where patient values were taken
# @Output: FBG value in mmol/L or NA if not matched
transform_fbg_in_mmol <- function(fbg, country_id, hospital_id) {
  
  fbg_num <- as.numeric(fbg) 
  factor_mmol_in_mg <- 18.02
  measure_unit <- assign_fbg_unit_per_hospital(country_id = country_id, 
                                               hospital_id = hospital_id)
  
  # If not unit "mmol/L" is assumed
  fbg_mmol <- case_when(
    measure_unit == "mg/dL" ~ fbg_num / factor_mmol_in_mg,
    measure_unit == "mmol/L" ~ fbg_num,
    is.na(measure_unit) ~ fbg_num,
    TRUE ~ NA_real_
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
    TRUE ~ NA_real_
  )
  
  if(is.na(fbg_result)){
    stop("ERROR: FBG value outside realistic scale")
  }
  
  return(fbg_result)
}

# @Description: FBG input is often a range (200-300) but functions only
#               work with unique values. This wrapper hence loops the range through 
#               the functions.
# @hid: Hospital ID
# @cid: country ID
fbg_wrapper <- function(fbg_range, hid, cid){
  
  # Source for levels: https://www.cdc.gov/diabetes/basics/getting-tested.html
  fbg_range <- case_when(
    grepl("high|bad|hi", tolower(fbg_range)) ~ "200",
    grepl("med|medium", tolower(fbg_range)) ~ "140-199",
    grepl("low|good|okay", tolower(fbg_range)) ~ "140",
    TRUE ~ fbg_range
  ) %>%
    gsub(pattern="(DKA)", replacement="")
  
  
  lower_upper_fbg <- fbg_range %>% str_split("-") %>%
    unlist() %>%
    as.numeric()
  
  for(i in 1:length(lower_upper_fbg)){
    lower_upper_fbg[i] <- sanity_check_fbg_mmol(
      transform_fbg_in_mmol(
        lower_upper_fbg[i], 
        country_id = cid, 
        hospital_id = hid
      )
    )
  }
  
  final <- paste(lower_upper_fbg, collapse = "-")
  
}


fbg_fix <- function(fbg, country, hospital) {
  d <- try(
    fbg_wrapper(
      fbg, cid = country, hid = hospital),
    silent = TRUE)
  if (class(d) == "try-error") {
    d  <- "999999" }
  return(d)
}


#  "support_from_a4d" ####   
# ______________________________________________
#### SUPPORT A4D
supporta4d_fix <- function(d) {
  d <- try(as.character(d), silent = TRUE)
  if (!d %in% c("Partial", "SAC", "Full",
                "Insulin, SMBG, HbA1c & Transportation", 
                "Insulin, SMBG & HbA1c", "Sponsor A Child" ,"Standard")) {
    d  <- "999999" }
  return(d)
}


#  "testing_fqr_pday" ####   
# ______________________________________________
#### TESTING FQR 

# If ranges, take mean
replace_testfqr_strings_mean <- function(x){
  y <- unlist(map(str_split(x, pattern = "-"), 
                  function(z) mean(as.numeric(z))))
}

fix_testfqr <- function(d) {
  d <- try(replace_testfqr_strings_mean(d), silent = TRUE)
  if (class(d) == "try-error") {
    d  <- 999999 }
  return(d)
}


# "est_strips_pmoth" ####  
# ______________________________________________
#### STRIPS NEEDED

fix_strips <- function(d) {
  d <- try(as.numeric(d), silent = TRUE)
  if (class(d) == "try-error") {
    d  <- 999999 }
  return(d)
}


#  "status" #### 
# ______________________________________________
#### STATUS
fix_status <- function(d) {
  
  if (!is.na(d)) {
    
    d <- try(as.character(d), silent = TRUE)
    if (!d %in% c("Active", "Deceased", "Discontinued",
                  "Inactive",  "Query", "Active - Remote",
                  "Lost Follow Up" )) {
      d  <- "999999" }
    
  } else {d <- NA}
  return(d)
}


# "updated_fbg_sample" ####
# ______________________________________________
#### UPDATED FBG SAMPLE
fix_fbg_sample <- function(d) {
  
  
  if (!is.na(d)) {
    
    d <- try(replace_empty_string_with_NA(as.character(d)), silent = TRUE)
    if (!d %in% c("SMBG", "CBG")) {
      d  <- "999999" }
    
  } else {d <- NA}
  
  return(d)
}


# "tracker_year" ####   
# TODO: Read out single year, check that all years are the same and match the input of the year function?
# Currently uses fix_chr_without_NAs 

# "clinic_code" ####                       
# TODO: When we have data, using list of clinics to double check?
# TODO Operationalization: Read out clinic table in database to check data with
# Currently uses fix_chr_without_NAs 

# "country_code" ####                      
# TODO: When we have data, using list of countries to double check?
# TODO Operationalization: Read out clinic table in database to check data with
# Currently uses fix_chr_without_NAs 


# "sheet_name" ####                        
# TODO: Include in final function
# Currently uses fix_chr_without_NAs 

# parse_sheet_name <- function(x){
#   y <- unlist(map(as.character(x), function(z)
#     (format(readr::parse_date(z,"%b'%y"), format = "%Y-%m"))))
#   # TODO: real trackers use format like "Feb18" not "Feb'18" -> remove the apostrophe upstream?
# }
# 
# transform_sheet_name_to_tracker_month <- function(x) {
#   x <- try(parse_sheet_name(x), silent = TRUE)
#   if (class(x) == "try-error") {
#     x <- "999999" }
#   return(x)
# }
# 

# "insulin_regimen" #### 
# ______________________________________________

fix_insulin_reg <- function(d) {
  
  
  if (!is.na(d)) {
    
    d <- try(as.character(d), silent = TRUE)
    if (!tolower(d) %in% tolower(c("basal-bolus", "premixed 30/70 bd", "insulin pump", "Premixed BD",
                                   "Premixed 30/70 BD", "Modified conventional TID", "NPH ",
                                   "Others"))) {
      d  <- "999999" }
    
  } else {d <- NA}
  
  return(d)
}


# "insulin_dosage" #### 
# ______________________________________________

fix_insulin_dos <- function(d) {
  
  
  if (!is.na(d)) {
    
    d <- try(as.character(d), silent = TRUE)
    if (class(d) == "try-error") {
      d  <- "999999" }
    
  } else {d <- NA}
  
  return(d)
}


# "required_insulin" #### 
# ______________________________________________

fix_required_insulin <- function(d) {
  
  
  if (!is.na(d)) {
    
    d <- try(as.numeric(d), silent = TRUE)
    if (class(d) == "try-error") {
      d  <- 999999 }
    
  } else {d <- NA}
  
  return(d)
}

# "required_insulin_product_name" #### 
# ______________________________________________

fix_required_insulin_name <- function(d) {
  
  
  if (!is.na(d)) {
    
    d <- try(as.character(d), silent = TRUE)
    if (class(d) == "try-error") {
      d  <- "999999" }
    
  } else {d <- NA}
  
  return(d)
}

# "est_strips_pmoth" #### 
# ______________________________________________

fix_est_strips_pmoth <- function(d) {
  
  
  if (!is.na(d)) {
    
    d <- try(as.numeric(d), silent = TRUE)
    if (class(d) == "try-error") {
      d  <- 999999 }
    
  } else {d <- NA}
  
  return(d)
}

# "blood_pressure_sys_mmhg" #### 
# ______________________________________________   
par_highest_blood_pressure_sys <- 250
par_lowest_blood_pressure_sys <- 20

fix_blood_pressure_sys <- function(d) {
  
  if (!is.na(d)) {
    
    d <- try(check_numeric_borders(
      d,
      par_highest_blood_pressure_sys,
      par_lowest_blood_pressure_sys), silent = TRUE)
    if (class(d) == "try-error") {
      d <- "999999" }
    
    
  } else {d <- NA}
  
  
  return(d)
}

#"blood_pressure_dias_mmhg" ####    
# ______________________________________________      

par_highest_blood_pressure_dias <- 220
par_lowest_blood_pressure_dias <- 20

fix_blood_pressure_dias <- function(d) {
  
  
  if (!is.na(d)) {
    
    d <- try(check_numeric_borders(
      d,
      par_highest_blood_pressure_dias,
      par_lowest_blood_pressure_dias), silent = TRUE)
    if (class(d) == "try-error") {
      d <- "999999" }
    
    
  } else {d <- NA}
  
  return(d)
}

#  "weight" ####             
# ______________________________________________               
par_max_weight_kg <- 200
par_min_weight_kg <- 0

fix_weight <- function(d) {
  
  
  if (!is.na(d)) {
    
    d <- try(check_numeric_borders(d, par_max_weight_kg, par_min_weight_kg), 
             silent = TRUE)
    if (class(d) == "try-error") {
      d <- 999999 }
    
  } else {d <- NA}
  
  
  return(d)
}


# "height" ####    
# ______________________________________________
par_max_height <- 200
par_min_height <- 0

transform_cm_to_m <- function(height){
  height <- as.numeric(height)
  height <- ifelse(height > 50,
                   height/100,
                   height)
}

fix_height <- function(d) {
  
  if (!is.na(d)) {
    
    d <- try(
      check_numeric_borders(
        transform_cm_to_m(d), 
        par_max_height, par_min_height), 
      silent = TRUE)
    if (class(d) == "try-error") {
      d <- 999999 }
    
    
  } else {d <- NA}
  
  return(d)
}

# "bmi" ####      
# ______________________________________________
par_max_bmi <- 60
par_min_bmi <- 4


fix_bmi <- function(d, par_max_bmi, par_min_bmi) {
  
  if (!is.na(d)) {
    
    d <- try(check_numeric_borders(d, par_max_bmi, par_min_bmi), 
             silent = TRUE)
    if (class(d) == "try-error") {
      d <- 999999 }
    
    
  } else {d <- NA}
  
  return(d)
}


# "edu_occ" AN CODES ####         
# ______________________________________________

# TODO need to check with valid codes from the AN sheet
#  tyla could give us a list to check if all codes exist such that these can later be replaced
fix_edu_occ <- function(d) {
  d <- d
  return(d)
}


#
# "hospitalisation" ####     
# ______________________________________________    
# TODO: If possible transform all texted dates into real dates. Complex manual function necessary

extract_hospitalisation_date <- function(hosp_str){
  
  str_out <- hosp_str %>%
    replace(hosp_str == "NA", NA)
  
}

fix_hospitalisation <- function(d) {
  d <- try(extract_hospitalisation_date(d), 
           silent = TRUE)
  if (class(d) == "try-error") {
    d <- "999999" }
  return(d)
}


# "additional_support"    
# ______________________________________________

fix_additional_support <- function(d) {
  
  if (!is.na(d)) {
    
    d <- as.character(d)  } else {d <- NA}
  return(d)
}
# "id" #### 
# ______________________________________________                 
fix_id <- function(d) {
  
  if (!is.na(d)) {
    
    d <- as.character(d)  } else {d <- NA}
  return(d)
}

# "latest_complication_screening_type" ####
# ______________________________________________
fix_latest_complic <- function(d) {
  
  if (!is.na(d)) {
    
    d <- as.character(d)  } else {d <- NA}
  return(d)
}

# "remarks" ####
# ______________________________________________
fix_remarks <- function(d) {
  
  if (!is.na(d)) {
    
    d <- as.character(d)  } else {d <- NA}
  return(d)
}



# TODO: Add checks & new columns for DC_V2_Anon Example csv DONE!


# "dm_complication_comment" ####
# ______________________________________________
fix_complication_comment <- function(d) {
  
  if (!is.na(d)) {
    
    d <- as.character(d)  } else {d <- NA}
  return(d)
}
# "dm_complication_*" ####
# ______________________________________________
fix_complication <- function(d) {
  
  if (!is.na(d)) {
    
    d <- ifelse(tolower(d) %in% c("y", "n", "0", "1"), d, "999999") 
    
    d <- as.data.frame(d)
    d <- d %>%  mutate(d = case_when(d == "0" ~ "N",
                                     d == "1" ~ "Y",
                                     TRUE ~ "999999"))
    
    d <- d$d
    
  } else {d <- NA}
  return(d)
}

# "num_admin_hosp_*" ####
# ______________________________________________

fix_num_hosp <- function(d) {
  if (!is.na(d)) {
    
    d <- try(as.numeric(d), 
             silent = TRUE)
    if (class(d) == "try-error") {
      d <- 999999 }
    
    
  } else {d <- NA}
  
  return(d)
}


# "num_admin_hosp_other_reason" ####
# ______________________________________________
fix_admin_hosp <- function(d) {
  
  if (!is.na(d)) {
    
    d <- as.character(d)  } else {d <- NA}
  return(d)
}

# "inactive_reason" ####
# ______________________________________________
#### INSULIN REGIMEN
fix_inactive_reason <- function(d) {
  
  
  if (!is.na(d)) {
    
    d <- try(as.character(d), silent = TRUE)
    if (class(d) == "try-error") {
      d <- 999999 }
    if (!tolower(d) %in% tolower(c("Deceased","Lost Follow Up"))) {
      d  <- "999999" }
    
  } else {d <- NA}
  
  return(d)
}

# "lost_date" ####
#  SEE DATE FIX FUNCTION
# "lost_age" ####
# ______________________________________________
fix_lost_age <- function(d) {
  if (!is.na(d)) {
    
    d <- try(as.numeric(d), 
             silent = TRUE)
    if (class(d) == "try-error") {
      d <- 999999 }
    
    
  } else {d <- NA}
  
  return(d)
}
# "diag_date" ####
#  SEE DATE FIX FUNCTION
# "dka_diag" ####
# ______________________________________________
fix_dka_diag <- function(d) {
  
  if (!is.na(d)) {
    
    d <- ifelse(tolower(d) %in% c("y", "n", "0", "1"), d, "999999") 
    
    d <- as.data.frame(d)
    d <- d %>%  mutate(d = case_when(d == "0" ~ "N",
                                     d == "1" ~ "Y",
                                     TRUE ~ "999999"))
    
    d <- d$d
    
  } else {d <- NA}
  return(d)
}

# "family_support_scale" ####
# NO IDEA WHAT THAT IS 






# MAIN/WRAPPER FUNCTION ---------------------------------------------------


# Output 1: Cleaned data table
# TODO Output 2: Table containing data checklist

cleaning_a4d_tracker <- function(data) {
  
  
  data_c <- data %>% 
    rowwise() %>% 
    mutate(id = fix_id(id),
           gender = fix_gender(gender),
           age = fix_age(age),
           age_diagnosis = fix_age_diagnosis(age_diagnosis),
           recruitment_date = fix_date_cols(recruitment_date),
           baseline_hba1c_prc = fix_hba1c(baseline_hba1c_prc),
           updated_hba1c_date = fix_date_cols(updated_hba1c_date),
           updated_hba1c_prc = fix_hba1c(updated_hba1c_prc),
           updated_fbg_date = fix_date_cols(updated_fbg_date),
           baseline_fbg_mgdl = fbg_fix(baseline_fbg_mgdl, 
                                       country = unique(data$country_code),
                                       hospital = unique(data$clinic_code)), # NOT FIXING FOR NOW
           updated_fbg_mgdl = fbg_fix(updated_fbg_mgdl, 
                                      country = unique(data$country_code),
                                      hospital = unique(data$clinic_code)) , # NOT FIXING FOR NOW
           support_from_a4d = fix_additional_support(support_from_a4d),
           insulin_regimen = fix_insulin_reg(insulin_regimen),
           insulin_dosage = fix_insulin_dos(insulin_dosage),
           testing_fqr_pday = fix_testfqr(testing_fqr_pday),
           required_insulin = fix_required_insulin(required_insulin),
           required_insulin_product_name = fix_required_insulin_name(required_insulin_product_name),
           est_strips_pmoth = fix_est_strips_pmoth(est_strips_pmoth),
           status = fix_status(status),
           patient_name = patient_name, 
           province = province,
           dob = dob,
           edu_occ = edu_occ, 
           sheet_name = sheet_name, 
           tracker_mo = tracker_mo,
           tracker_year = tracker_year, 
           country_code = country_code,
           clinic_code = clinic_code, 
           testing_fqr = as.numeric(testing_fqr), # NEED TO REMOVE
           updated_fbg_sample = fix_fbg_sample(updated_fbg_sample), # POTENTIALLY NEED TO CHECK AT EXTRACTION PHASE
           blood_pressure_sys_mmhg = fix_blood_pressure_sys(blood_pressure_sys_mmhg),
           blood_pressure_dias_mmhg = fix_blood_pressure_dias(blood_pressure_dias_mmhg),
           weight = fix_weight(weight),
           height = fix_height(height),
           bmi = fix_bmi(bmi),
           bmi_date = fix_date_cols(bmi_date),
           hospitalisation = fix_hospitalisation(hospitalisation),
           last_clinic_visit_date = fix_date_cols(last_clinic_visit_date),
           additional_support = fix_additional_support(additional_support),
           latest_complication_screening_type = fix_latest_complic(latest_complication_screening_type),
           remarks = fix_remarks(remarks),
           dm_complication_comment = fix_complication_comment(dm_complication_comment),
           dm_complication_other = fix_complication(dm_complication_other),
           dm_complication_kidney = fix_complication(dm_complication_kidney),
           dm_complication_eye = fix_complication(dm_complication_eye),
           num_admin_hosp_total = fix_num_hosp(num_admin_hosp_total),
           num_admin_hosp_dka = fix_num_hosp(num_admin_hosp_dka),
           num_admin_hosp_hypo = fix_num_hosp(num_admin_hosp_hypo),
           num_admin_hosp_other = fix_num_hosp(num_admin_hosp_other),
           num_admin_hosp_other_reason = fix_admin_hosp(num_admin_hosp_other_reason),
           inactive_reason = fix_inactive_reason(inactive_reason),
           lost_date = fix_date_cols(lost_date),
           lost_age = fix_lost_age(lost_age),
           diag_date = fix_date_cols(diag_date),
           dka_diag = fix_dka_diag(dka_diag)
           # family_support_scale = family_support_scale  # NOT FIXING FOR NOW
           ) %>% 
    ungroup() 
  # %>% select(id:family_support_scale)
  
  data_c <- as.data.frame(data_c)
  
  
  return(data_c)
  
}

# TEST --------------------------------------------------------------------

# testing <- cleaning_a4d_tracker(data = dat)

# create a summary of the variables (helpful to do sanity checks!)
# library(summarytools)
# summarytools::view(dfSummary(testing), file = "a4doverview_test28022022.html")


# TODOs:
## 1. Variables until [16] testing_fqr are part of the final wrapper. EVerything afterwards
#      (and buggy ones lige age_diagnosis) need to be finalized and added to the wrapper
#  2. Dates are transformed as dates but only show raw date number (e.g. "123123")
#      instead of actual date ("2020-02-02")
#  3. Edu_Occ can't be matched by thai vocabulary. Functions should work but it seems
#     that some R language encryption issues arise when saving thai strings.
#  4. Columns that are not correctly extracted yet, input needed:
#        latest_complication_screening_type, latest_complication_screening_date,
#        remarks, additional_support, est_strips_pmonth
## 4. Final check if all variables have been transformed correctly

# testing on correct data:
# 1. Currently the fbg ranges will be excluded due to values out of realistic range.
#    This is on purpose since we only know the units for specific hospitals/countries.
#    In the fake data the hospital and country codes are fake. We need to test the
#    function on the original data to ensure that the functions correctly transform
#    the unit of the fbg values (and hence include them) in the data.
#    See [12, 13] fbg and make adjustments if needed


