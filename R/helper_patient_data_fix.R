# DESCRIPTION -------------------------------------------------------------

# This function cleans the output of the "tidy" function by reformatting certain columns.
# Basic functions are created and then applied to each piece of data available in one row.

#### Base Functions ####

#' @title Try to convert character x into a value of target type.
#'
#' @description
#' Use cast_fnc to cast x to target type.
#' If that cast fails, output error_val instead.
#' If x is already NA, output NA.
#'
#' @param x character value.
#' @param cast_fnc function used to convert character values into target type.
#' @param error_val error value to use when cast_fnc fails.
#' @param col_name column name if used with across.
#'
#' @return converted value
#' @export
convert_to <- function(x, cast_fnc, error_val, col_name = "") {
    x <- tryCatch(
        cast_fnc(x),
        error = function(e) {
            logError("Could not convert value ", x, " in column ", col_name)
            x <- error_val
        },
        warning = function(w) {
            logWarn("Could not convert value ", x, " in column ", col_name)
            x <- error_val
        }
    )

    x
}


#' @title If x is outside its allowed range, return error value.
#'
#' @param x numeric value to check.
#' @param min lower bound.
#' @param max upper bound.
#' @param col_name column name if used with mutate/across.
#'
#' @return either x or error value.
#' @export
cut_numeric_value <- function(x,
                              min,
                              max,
                              col_name = "") {
    old_error_count <- length(which(x == ERROR_VAL_NUMERIC))
    x <- ifelse(x > max, ERROR_VAL_NUMERIC, x)
    x <- ifelse(x < min, ERROR_VAL_NUMERIC, x)

    if (ERROR_VAL_NUMERIC %in% x) {
        new_error_count <- length(which(x == ERROR_VAL_NUMERIC)) - old_error_count
        logWarn(
            "Column ", col_name, " contains invalid values outside [", min, ", ", max, "]. ",
            new_error_count, " values were replaced with ", ERROR_VAL_NUMERIC, "."
        )
    }

    x
}


#' @title replace empty strings with NA.
#'
#' @param string_vector character vector containing empty strings.
#'
#' @return
#' @export
replace_empty_string_with_NA <- function(string_vector) {
    ifelse(string_vector == "", NA_character_, string_vector)
}


#' @title Correct decimal sign.
#'
#' @param x character representation of a number.
#'
#' @return character representation with a correct decimal sign.
#' @export
correct_decimal_sign <- function(x) {
    str_replace(x, ",", ".")
}

#### age ####

#' @title Fix age column.
#'
#' @description
#' Based on date of birth calculate the current age of the patient.
#' If this age is different than the given patient age, we take the calculated one.
#'
#'
#' @param age patient age.
#' @param dob patient date of birth.
#' @param tracker_year year the tracker was filled out.
#' @param tracker_month month the tracker was filled out.
#' @param id patient id.
#'
#' @return fixed age.
#' @export
fix_age <- function(age, dob, tracker_year, tracker_month, id) {
    calc_age <- age

    if (!is.na(dob)) {
        dob <- as.Date(dob)
        calc_age <- tracker_year - lubridate::year(dob)
        delta_month <- tracker_month - lubridate::month(dob)
        if (delta_month < 0) {
            calc_age <- calc_age - 1
        }

        if (calc_age != age) {
            logWarn(
                "Patient ", id, ": age ", age, " is different from calculated age ", calc_age,
                ". Using calculated age instead of original age."
            )
        }

        if (calc_age < 0) {
            logWarn("Patient ", id, ": calculated age is negative. Something went wrong.")
            calc_age <- ERROR_VAL_NUMERIC
        }
    }

    calc_age
}

#### bmi ####

#' @title Only accept value for bmi if there is also a weight and height.
#'
#' @description
#' Set bmi to error val if either weight or height is NA.
#'
#' @param bmi numeric bmi value.
#' @param weight numeric weight value.
#' @param height numeric height value.
#' @param id patient id.
#'
#' @return corrected bmi value
#' @export
fix_bmi <- function(bmi, weight, height, id) {
    if (is.na(bmi)) {
        return(NA_real_)
    }

    if (is.na(weight) || is.na(height)) {
        logWarn("Patient ", id, ": height or weight value is missing. Setting bmi to error value.")
        bmi <- ERROR_VAL_NUMERIC
    }

    bmi
}


#### gender ####

#' @title Replace gender synonyms with either M, F or Other.
#'
#' @param gender patient gender.
#' @param id patient id.
#'
#' @return Either M, F or Other.
#' @export
fix_gender <- function(gender, id) {
    synonyms_female <- c("female", "girl", "woman", "fem", "feminine", "f")
    synonyms_male <- c("male", "boy", "man", "masculine", "m")

    fixed_gender <- case_when(
        is.na(gender) | gender == "" ~ NA_character_,
        tolower(gender) %in% synonyms_female ~ "F",
        tolower(gender) %in% synonyms_male ~ "M",
        .default = "Other"
    )

    if (!is.na(fixed_gender) && fixed_gender == "Other") {
        logWarn("Patient ", id, ": gender ", gender, " is not in the list of synonyms. Replacing it with ", fixed_gender, ".")
    }
    fixed_gender
}



#### t1d_diagnosis_age ####


#' @title Extract year from age descriptions like "10y5m"
#'
#' @description
#' Example: "1y5m" -> "1"
#'
#' @param age text representation of diagnosis age
#'
#' @return year of given age as character.
#' @export
extract_year_from_age <- function(age) {
    if (is.na(age)) {
        return(age)
    }
    unlist(strsplit(age, "y", fixed = T))[1]
}

#' @title Try to replace text descriptions for age with proper numerical values.
#'
#' @param t1d_diagnosis_age patient age when diagnosed with T1D.
#' @param t1d_diagnosis_date date when diagnosed with T1D.
#' @param id patient id
#'
#' @return
#' @export
fix_t1d_diagnosis_age <- function(t1d_diagnosis_age, t1d_diagnosis_date, id) {
    age_corrected <- case_when(
        is.na(t1d_diagnosis_age) ~ NA_character_,
        grepl("birth|born|month", tolower(t1d_diagnosis_age)) ~ "0",
        grepl("y", tolower(t1d_diagnosis_age)) ~ extract_year_from_age(t1d_diagnosis_age),
        .default = t1d_diagnosis_age
    )

    age_corrected
}


#### fbg ####


# @Description: Makes sure FBG values are in mmol/L
# @fbg: Any fbg value in mg/dL or mmol/L
# @country_id: ID of the country where patient values were taken
# @hospital_id: ID of the hospital where patient values were taken
# @Output: FBG value in mmol/L or NA if not matched
transform_fbg_in_mmol <- function(fbg, country_id, hospital_id) {
    factor_mmol_in_mg <- 18.02
    measure_unit <-
        assign_fbg_unit_per_hospital(
            country_id = country_id,
            hospital_id = hospital_id
        )

    # If not unit "mmol/L" is assumed
    fbg_mmol <- case_when(
        measure_unit == "mg/dL" ~ fbg / factor_mmol_in_mg,
        measure_unit == "mmol/L" ~ fbg,
        is.na(measure_unit) ~ fbg,
        TRUE ~ NA_real_
    )

    return(as.numeric(fbg_mmol))
}


#' @title Replace common textual descriptions with numeric values
#'
#' @param fbg character value.
#'
#' @return fbg transformed value.
#' @export
fix_fbg <- function(fbg) {
    # Source for levels: https://www.cdc.gov/diabetes/basics/getting-tested.html
    fbg <- case_when(
        grepl("high|bad|hi", tolower(fbg)) ~ "200",
        grepl("med|medium", tolower(fbg)) ~ "170",
        grepl("low|good|okay", tolower(fbg)) ~ "140",
        TRUE ~ fbg
    ) %>%
        gsub(pattern = "(DKA)", replacement = "", fixed = T) %>%
        trimws()

    fbg
}


##### support_from_a4d ####

support_a4d_fix <- function(d) {
    d <- try(as.character(d), silent = TRUE)
    if (!d %in% c(
        "Partial",
        "SAC",
        "Full",
        "Insulin, SMBG, HbA1c & Transportation",
        "Insulin, SMBG & HbA1c",
        "Sponsor A Child",
        "Standard"
    )) {
        d <- "999999"
    }
    return(d)
}


# "testing_fqr_pday" ####
# ______________________________________________
#### TESTING FQR

# If ranges, take mean
replace_testfqr_strings_mean <- function(x) {
    y <- unlist(map(
        str_split(x, pattern = "-"),
        function(z) {
            mean(as.numeric(z))
        }
    ))
}

fix_testfqr <- function(d) {
    d <- try(replace_testfqr_strings_mean(d), silent = TRUE)
    if (class(d) == "try-error") {
        d <- 999999
    }
    return(d)
}


# "est_strips_pmoth" ####
# ______________________________________________
#### STRIPS NEEDED

fix_strips <- function(d) {
    d <- try(as.numeric(d), silent = TRUE)
    if (class(d) == "try-error") {
        d <- 999999
    }
    return(d)
}


# "status" ####
# ______________________________________________
#### STATUS
fix_status <- function(d) {
    if (!is.na(d)) {
        d <- try(as.character(d), silent = TRUE)
        if (!d %in% c(
            "Active",
            "Deceased",
            "Discontinued",
            "Inactive",
            "Query",
            "Active - Remote",
            "Lost Follow Up"
        )) {
            d <- "999999"
        }
    } else {
        d <- NA
    }
    return(d)
}


# "updated_fbg_sample" ####
# ______________________________________________
#### UPDATED FBG SAMPLE
fix_fbg_sample <- function(d) {
    if (!is.na(d)) {
        d <-
            try(replace_empty_string_with_NA(as.character(d)),
                silent = TRUE
            )
        if (!d %in% c("SMBG", "CBG")) {
            d <- "999999"
        }
    } else {
        d <- NA
    }

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
# y <- unlist(map(as.character(x), function(z)
# (format(readr::parse_date(z,"%b'%y"), format = "%Y-%m"))))
# # TODO: real trackers use format like "Feb18" not "Feb'18" -> remove the apostrophe upstream?
# }
#
# transform_sheet_name_to_tracker_month <- function(x) {
# x <- try(parse_sheet_name(x), silent = TRUE)
# if (class(x) == "try-error") {
# x <- "999999" }
# return(x)
# }
#

# "insulin_regimen" ####
# ______________________________________________

fix_insulin_reg <- function(d) {
    if (!is.na(d)) {
        d <- try(as.character(d), silent = TRUE)
        if (!tolower(d) %in% tolower(
            c(
                "basal-bolus",
                "premixed 30/70 bd",
                "insulin pump",
                "Premixed BD",
                "Premixed 30/70 BD",
                "Modified conventional TID",
                "NPH ",
                "Others",
                "basal",
                "premixed bd + bolus",
                "premixed bd + glaridus",
                "glargine",
                "basal bolus",
                "self-mixed bd",
                "modified conventional",
                "premixed 30/70",
                "basal-bolus mdi (an)",
                "other"
            )
        )) {
            d <- "999999"
        }
    } else {
        d <- NA
    }

    return(d)
}


# "insulin_dosage" ####
# ______________________________________________

fix_insulin_dos <- function(d) {
    if (!is.na(d)) {
        d <- try(as.character(d), silent = TRUE)
        if (class(d) == "try-error") {
            d <- "999999"
        }
    } else {
        d <- NA
    }

    return(d)
}


# "required_insulin" ####
# ______________________________________________

fix_required_insulin <- function(d) {
    if (!is.na(d)) {
        d <- try(as.numeric(d), silent = TRUE)
        if (class(d) == "try-error") {
            d <- 999999
        }
    } else {
        d <- NA
    }

    return(d)
}

# "required_insulin_product_name" ####
# ______________________________________________

fix_required_insulin_name <- function(d) {
    if (!is.na(d)) {
        d <- try(as.character(d), silent = TRUE)
        if (class(d) == "try-error") {
            d <- "999999"
        }
    } else {
        d <- NA
    }

    return(d)
}

# "est_strips_pmoth" ####
# ______________________________________________

fix_est_strips_pmoth <- function(d) {
    if (!is.na(d)) {
        d <- try(as.numeric(d), silent = TRUE)
        if (class(d) == "try-error") {
            d <- 999999
        }
    } else {
        d <- NA
    }

    return(d)
}

# "blood_pressure_sys_mmhg" ####
# ______________________________________________
par_highest_blood_pressure_sys <- 250
par_lowest_blood_pressure_sys <- 20

fix_blood_pressure_sys <- function(d) {
    if (!is.na(d)) {
        d <- try(
            cut_numeric_value(
                d,
                par_highest_blood_pressure_sys,
                par_lowest_blood_pressure_sys
            ),
            silent = TRUE
        )
        if (class(d) == "try-error") {
            d <- "999999"
        }
    } else {
        d <- NA
    }


    return(d)
}

# "blood_pressure_dias_mmhg" ####
# ______________________________________________

par_highest_blood_pressure_dias <- 220
par_lowest_blood_pressure_dias <- 20

fix_blood_pressure_dias <- function(d) {
    if (!is.na(d)) {
        d <- try(
            cut_numeric_value(
                d,
                par_highest_blood_pressure_dias,
                par_lowest_blood_pressure_dias
            ),
            silent = TRUE
        )
        if (class(d) == "try-error") {
            d <- "999999"
        }
    } else {
        d <- NA
    }

    return(d)
}

# "weight" ####
# ______________________________________________
par_max_weight_kg <- 200
par_min_weight_kg <- 0

fix_weight <- function(d) {
    if (!is.na(d)) {
        d <-
            try(cut_numeric_value(d, par_max_weight_kg, par_min_weight_kg),
                silent = TRUE
            )
        if (class(d) == "try-error") {
            d <- 999999
        }
    } else {
        d <- NA
    }


    return(d)
}


# "height" ####
# ______________________________________________
par_max_height <- 200
par_min_height <- 0

transform_cm_to_m <- function(height) {
    height <- as.numeric(height)
    height <- ifelse(height > 50,
        height / 100,
        height
    )
}

fix_height <- function(d) {
    if (!is.na(d)) {
        d <- try(
            cut_numeric_value(
                transform_cm_to_m(d),
                par_max_height, par_min_height
            ),
            silent = TRUE
        )
        if (class(d) == "try-error") {
            d <- 999999
        }
    } else {
        d <- NA
    }

    return(d)
}


# "edu_occ" AN CODES ####
# ______________________________________________

# TODO need to check with valid codes from the AN sheet
# tyla could give us a list to check if all codes exist such that these can later be replaced
fix_edu_occ <- function(d) {
    d <- d
    return(d)
}


#
# "hospitalisation" ####
# ______________________________________________
# TODO: If possible transform all texted dates into real dates. Complex manual function necessary

extract_hospitalisation_date <- function(hosp_str) {
    str_out <- hosp_str %>%
        replace(hosp_str == "NA", NA)
}

fix_hospitalisation <- function(d) {
    d <- try(extract_hospitalisation_date(d),
        silent = TRUE
    )
    if (class(d) == "try-error") {
        d <- "999999"
    }
    return(d)
}


# "id" ####
# ______________________________________________
fix_id <- function(d) {
    if (!is.na(d)) {
        d <- as.character(d)
    } else {
        d <- NA
    }
    return(d)
}

# "latest_complication_screening_type" ####
# ______________________________________________
fix_latest_complic <- function(d) {
    if (!is.na(d)) {
        d <- as.character(d)
    } else {
        d <- NA
    }
    return(d)
}

# "remarks" ####
# ______________________________________________
fix_remarks <- function(d) {
    if (!is.na(d)) {
        d <- as.character(d)
    } else {
        d <- NA
    }
    return(d)
}



# TODO: Add checks & new columns for DC_V2_Anon Example csv DONE!


# "dm_complication_comment" ####
# ______________________________________________
fix_complication_comment <- function(d) {
    if (!is.na(d)) {
        d <- as.character(d)
    } else {
        d <- NA
    }
    return(d)
}
# "dm_complication_*" ####
# ______________________________________________
fix_complication <- function(d) {
    if (!is.na(d)) {
        d <- ifelse(tolower(d) %in% c("y", "n", "0", "1"), d, "999999")

        d <- as.data.frame(d)
        d <- d %>% mutate(d = case_when(
            d == "0" ~ "N",
            d == "1" ~ "Y",
            TRUE ~ "999999"
        ))

        d <- d$d
    } else {
        d <- NA
    }
    return(d)
}

# "num_admin_hosp_*" ####
# ______________________________________________

fix_num_hosp <- function(d) {
    if (!is.na(d)) {
        d <- try(as.numeric(d),
            silent = TRUE
        )
        if (class(d) == "try-error") {
            d <- 999999
        }
    } else {
        d <- NA
    }

    return(d)
}


# "num_admin_hosp_other_reason" ####
# ______________________________________________
fix_admin_hosp <- function(d) {
    if (!is.na(d)) {
        d <- as.character(d)
    } else {
        d <- NA
    }
    return(d)
}

# "inactive_reason" ####
# ______________________________________________
#### INSULIN REGIMEN
fix_inactive_reason <- function(d) {
    if (!is.na(d)) {
        d <- try(as.character(d), silent = TRUE)
        if (class(d) == "try-error") {
            d <- 999999
        }
        if (!tolower(d) %in% tolower(c("Deceased", "Lost Follow Up"))) {
            d <- "999999"
        }
    } else {
        d <- NA
    }

    return(d)
}

# "lost_date" ####
# SEE DATE FIX FUNCTION
# "lost_age" ####
# ______________________________________________
fix_lost_age <- function(d) {
    if (!is.na(d)) {
        d <- try(as.numeric(d),
            silent = TRUE
        )
        if (class(d) == "try-error") {
            d <- 999999
        }
    } else {
        d <- NA
    }

    return(d)
}
# "diag_date" ####
# SEE DATE FIX FUNCTION
# "dka_diag" ####
# ______________________________________________
fix_dka_diag <- function(d) {
    if (!is.na(d)) {
        d <- ifelse(tolower(d) %in% c("y", "n", "0", "1"), d, "999999")

        d <- as.data.frame(d)
        d <- d %>% mutate(d = case_when(
            d == "0" ~ "N",
            d == "1" ~ "Y",
            TRUE ~ "999999"
        ))

        d <- d$d
    } else {
        d <- NA
    }
    return(d)
}


#' @title Split a column value into value and date.
#'
#' @description
#' Assumes that the measurement entry is in the format "8.53 (28/8/2017)".
#' Uses regex to split this string into the value part and the date part,
#' for example in "8.53 " and "28/8/2017".
#'
#' @param df data frame with patient data.
#' @param colname column to split into value and date.
#'
#' @return data frame with the original column and a new colname_date column.
extract_date_from_measurement <-
    function(df, colname) {
        df <- df %>% separate_wider_regex(
            !!colname,
            c(
                set_names(".*", colname),
                "[(]",
                set_names(".*?", paste0(colname, "_date")),
                "[)]?"
            ),
            too_few = "align_start"
        )

        if (colname == "fbg_updated_mmol" || colname == "fbg_updated_mg") {
            df <- df %>% rename_with(~ str_replace(.x, "_mmol_date|_mg_date", "_date"))
        }

        df
    }




date_fix <-
    function(df) {
        format <- "%Y/%m/%d"
        year <- df$tracker_year[1]


        if ("recruitment_date" %in% colnames(df) & year > 2018) {
            df <- df %>%
                mutate(
                    recruitment_date = as.Date(
                        as.numeric(recruitment_date) * (60 * 60 * 24),
                        origin = "1899-12-30",
                        format = format,
                        tz = "GMT"
                    )
                )
        }

        if ("last_clinic_visit_date" %in% colnames(df)) {
            df <- df %>%
                mutate(
                    last_clinic_visit_date = as.POSIXct(
                        as.numeric(last_clinic_visit_date) * (60 * 60 * 24),
                        format = format,
                        origin = "1899-12-30",
                        tz = "GMT"
                    )
                )
        }

        if ("bmi_date" %in% colnames(df)) {
            df <- df %>%
                mutate(
                    bmi_date = as.POSIXct(
                        as.numeric(bmi_date) * (60 * 60 * 24),
                        format = format,
                        origin = "1899-12-30",
                        tz = "GMT"
                    ),
                    bmi_date = format(as.Date(bmi_date), "%Y-%m")
                )
        }

        if ("updated_fbg_date" %in% colnames(df) & year > 2018) {
            df <- df %>%
                mutate(
                    updated_fbg_date = as.POSIXct(
                        as.numeric(updated_fbg_date) * (60 * 60 * 24),
                        format = format,
                        origin = "1899-12-30",
                        tz = "GMT"
                    ),
                    updated_fbg_date = case_when(
                        year(updated_fbg_date) < 100 ~ updated_fbg_date %m+% years(2000),
                        TRUE ~ updated_fbg_date
                    )
                )
        }

        if ("updated_hba1c_date" %in% colnames(df) & year > 2018) {
            df <- df %>%
                mutate(
                    updated_hba1c_date = as.POSIXct(
                        as.numeric(updated_hba1c_date) * (60 * 60 * 24),
                        format = format,
                        origin = "1899-12-30",
                        tz = "GMT"
                    ),
                    updated_hba1c_date = case_when(
                        year(updated_hba1c_date) < 100 ~ updated_hba1c_date %m+% years(2000),
                        TRUE ~ updated_hba1c_date
                    )
                )
        }

        return(df)
    }



#' @title Separate single blood pressure value into sys and dias values.
#'
#' @param df data frame with patient data.
#'
#' @return data frame with two new columns: blood_pressure_sys_mmhg and blood_pressure_dias_mmhg.
split_bp_in_sys_and_dias <- function(df) {
    logInfo("Splitting blood_pressure_mmhg into blood_pressure_sys_mmhg and blood_pressure_dias_mmhg.")
    df <- df %>%
        separate_wider_delim(
            cols = blood_pressure_mmhg,
            delim = "/",
            names = c("blood_pressure_sys_mmhg", "blood_pressure_dias_mmhg"),
        )
    df
}

# TEST --------------------------------------------------------------------

# testing <- cleaning_a4d_tracker(data = dat)

# create a summary of the variables (helpful to do sanity checks!)
# library(summarytools)
# summarytools::view(dfSummary(testing), file = "a4doverview_test28022022.html")


# TODOs:
## 1. Variables until [16] testing_fqr are part of the final wrapper. EVerything afterwards
# (and buggy ones lige age_diagnosis) need to be finalized and added to the wrapper
# 2. Dates are transformed as dates but only show raw date number (e.g. "123123")
# instead of actual date ("2020-02-02")
# 3. Edu_Occ can't be matched by thai vocabulary. Functions should work but it seems
# that some R language encryption issues arise when saving thai strings.
# 4. Columns that are not correctly extracted yet, input needed:
# latest_complication_screening_type, latest_complication_screening_date,
# remarks, additional_support, est_strips_pmonth
## 4. Final check if all variables have been transformed correctly

# testing on correct data:
# 1. Currently the fbg ranges will be excluded due to values out of realistic range.
# This is on purpose since we only know the units for specific hospitals/countries.
# In the fake data the hospital and country codes are fake. We need to test the
# function on the original data to ensure that the functions correctly transform
# the unit of the fbg values (and hence include them) in the data.
# See [12, 13] fbg and make adjustments if needed
