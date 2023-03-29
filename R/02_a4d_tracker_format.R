# DESCRIPTION -------------------------------------------------------------

#This function cleans the output of the "tidy" function by reformatting certain columns.
# Basic functions are created and then applied to each piece of data avaialbe in one row.


# Base Functions ####

check_numeric_borders <- function(vector,
                                  max,
                                  min){
  vector <- as.numeric(vector)
  vector <- ifelse(vector > max,
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

# @Description: Try to tranfsorm age of diagnosis. If error occur give out 99999
fix_age_diagnosis <- function(d) {

  d <- try(handle_age_diagnosis(d))
  if (class(d) == "try-error") {
        d <- 99999
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
    is.na(hospital_id) & is.na(country_id) ~ NA_character_,
    hospital_id %in% mg_hos ~ "mg/dL",
    country_id %in% mg_ct ~ "mg/dL",
    hospital_id %in% mmol_hos ~ "mmol/L",
    country_id %in% mmol_ct ~ "mmol/L",
    TRUE ~ NA_character_
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
    fbg_mmol <= min_fbg & fbg_mmol >= max_fbg ~ fbg_mmol,
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
    grepl("high|bad", tolower(fbg_range)) ~ "200",
    grepl("med|medium", tolower(fbg_range)) ~ "140-199",
    grepl("low|good|okay", tolower(fbg_range)) ~ "140",
    TRUE ~ fbg_range
  )


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
# Currently uses fix_chr_without_NAs

# [21] "clinic_code" ####
# TODO: When we have data, using list of clinics to double check?
# TODO Operationalization: Read out clinic table in database to check data with
# Currently uses fix_chr_without_NAs

# [22] "country_code" ####
# TODO: When we have data, using list of countries to double check?
# TODO Operationalization: Read out clinic table in database to check data with
# Currently uses fix_chr_without_NAs


# [23] "sheet_name" ####
# TODO: Include in final function
# Currently uses fix_chr_without_NAs

parse_sheet_name <- function(x){
  y <- unlist(map(as.character(x), function(z)
    (format(readr::parse_date(z,"%b'%y"), format = "%Y-%m"))))
  # TODO: real trackers use format like "Feb18" not "Feb'18" -> remove the apostrophe upstream?
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
# TODO: Instead of determining "elementary", identify year of education &
#       Ensure saving of thai strings works as planned
# ประถมศึกษาปีที่= elementary
# อนุบาล=kindergarden
# Take years behind in consideration, e.g."ประถมศึกษาปีที่ 4"


# @Description: Match thai education words and return level of education
match_education_strings <- function(str){

  str_out <- case_when(
    grepl(paste0("ประถมศึกษาปี", "|elementary"), str) ~ "elementary",
    grepl(paste0("อนุบา", "|kindergarden"), str) ~ "kindergarden",
    grepl(paste0("มหาวิทยาลั","|university"), str) ~ "university",
    TRUE ~ str
  )
  return(str_out)
  }


fix_edu_occ <- function(x) {
  x <- try(match_education_strings(x),
           silent = TRUE)
  if (class(x) == "try-error") {
    x <- "999999" }
  return(x)
}


# [32] "hospitalisation" ####
# ______________________________________________
# TODO: If possible transform all texted dates into real dates. Complex manual function necessary

extract_hospitalisation_date <- function(hosp_str){

  str_out <- hosp_str %>%
    replace(hosp_str == "NA", NA)

}

fix_hospitalisation <- function(x) {
  x <- try(extract_hospitalisation_date(x),
           silent = TRUE)
  if (class(x) == "try-error") {
    x <- "999999" }
  return(x)
}


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

    # Dates
    data_c$dob[i] <- fix_date_cols(data$dob[i])
    data_c$updated_fbg_date[i] = fix_date_cols(data$updated_fbg_date[i])
    data_c$updated_hba1c_date[i] = fix_date_cols(data$updated_hba1c_date[i])
    data_c$bmi_date[i] = fix_date_cols(paste(data$bmi_date[i], "01", sep = "-"))
    data_c$recruitment_date[i] = fix_date_cols(data$recruitment_date[i])
    data_c$last_clinic_visit_date[i] = fix_date_cols(data$last_clinic_visit_date[i])
    data_c$latest_complication_screening_date[i] = fix_date_cols(
      data$latest_complication_screening_date[i])

    # Static patient information
    data_c$id[i] <- fix_chr_without_NAs(data$id[i])
    data_c$patient_name[i] <- fix_chr_without_NAs(data$patient_name[i])
    data_c$province[i] <- fix_chr_without_NAs(data$province[i])
    data_c$edu_occ[i] <- fix_edu_occ(data$edu_occ[i])

    data_c$tracker_year[i] <- fix_chr_without_NAs(data$tracker_year[i])
    data_c$clinic_code[i] <- fix_chr_without_NAs(data$clinic_code[i])
    data_c$country_code[i] <- fix_chr_without_NAs(data$country_code[i])
    data_c$sheet_name[i] <- fix_chr_without_NAs(data$sheet_name[i])


    data_c$gender[i] <- fix_gender(data$gender[i])
    data_c$age[i]    <- fix_age(data$age[i])
    data_c$age_diagnosis[i] <- fix_age_diagnosis(data$age_diagnosis[i])
    data_c$status[i] <- status_fix(data$status[i])

    # Dynamic body information
    data_c$height[i] <- fix_height(data$height[i])
    data_c$weight[i] <- fix_weight(data$weight[i])
    data_c$bmi[i]    <- fix_bmi(data$bmi[i], data_c$weight[i], data_c$height[i] )

    # Blood values
    data_c$updated_hba1c_prc[i]  <- fix_hba1c(data$updated_hba1c_prc[i])
    data_c$baseline_hba1c_prc[i] <- fix_hba1c(data$baseline_hba1c_prc[i])

    data_c$insulin_regimen[i]    <- fix_insulin_reg(data$insulin_regimen[i])

    # FBG
    data_c$updated_fbg_mmoll <- fbg_fix(data_c$updated_fbg_mgdl,
                                        country  = data_c$country_code,
                                        hospital = data_c$clinic_code)
    data_c$baseline_fbg_mmoll <- fbg_fix(data_c$baseline_fbg_mgdl,
                                        country  = data_c$country_code,
                                        hospital = data_c$clinic_code)
    data_c$updated_fbg_sample[i] <- fix_fbg_sample(data$updated_fbg_sample[i])

    data_c$blood_pressure_dias_mmhg[i] <- fix_blood_pressure_dias(
      data$blood_pressure_dias_mmhg[i])

    data_c$blood_pressure_sys_mmhg[i] <- fix_blood_pressure_sys(
      data$blood_pressure_sys_mmhg[i])

    # Other
    data_c$support_from_a4d[i] <- supporta4d_fix(data$support_from_a4d[i])
    data_c$testing_fqr[i] <- testfqr_fix(data$testing_fqr[i])
    data_c$hospitalisation[i] <- fix_hospitalisation(data$hospitalisation[i])
  }

  # Keep FBG in mmol/L unit
  data_c <- data_c %>%
    dplyr::select(
      -c("updated_fbg_mgdl", "baseline_fbg_mgdl")
    )

  return(data_c)
}




# TEST --------------------------------------------------------------------


# Run test only if script is not sourced
# if (!interactive()) {
#   # reading the data frame
#   spec_cols <- cols(
#     no = col_character(),
#     patient_name = col_character(),
#     province = col_character(),
#     gender = col_character(),
#     dob = col_character(),
#     age = col_character(),
#     age_diagnosis = col_character(),
#     recruitment_date = col_character(),
#     baseline_hba1c_prc = col_character(),
#     updated_hba1c_prc = col_character(),
#     updated_hba1c_date = col_character(),
#     baseline_fbg_mgdl = col_character(),
#     updated_fbg_mgdl = col_character(),
#     updated_fbg_date = col_character(),
#     support_from_a4d = col_character(),
#     testing_fqr = col_character(),
#     est_strips_pmoth = col_character(),
#     status = col_character(),
#     updated_fbg_sample = col_character(),
#     tracker_year = col_character(),
#     clinic_code = col_character(),
#     country_code = col_character(),
#     sheet_name = col_character(),
#     insulin_regimen = col_character(),
#     blood_pressure_sys_mmhg = col_character(),
#     blood_pressure_dias_mmhg = col_character(),
#     weight = col_character(),
#     height = col_character(),
#     bmi = col_character(),
#     bmi_date = col_character(),
#     edu_occ = col_character(),
#     hospitalisation = col_character(),
#     last_clinic_visit_date = col_character(),
#     additional_support = col_character(),
#     id = col_character(),
#     latest_complication_screening_type = col_character(),
#     latest_complication_screening_date = col_character(),
#     remarks = col_character()
#   )
#
#
#   data <- read_csv("~/Desktop/A4DTracker_Overview.csv", col_types = spec_cols)
#
#
#   testing <- cleaning_a4d_tracker(data = data)
# }



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


