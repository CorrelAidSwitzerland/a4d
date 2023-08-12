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
    if (is.na(x)) {
        return(x)
    }

    x <- ifelse(x > max, ERROR_VAL_NUMERIC, x)
    x <- ifelse(x < min, ERROR_VAL_NUMERIC, x)

    if (x == ERROR_VAL_NUMERIC) {
        logWarn(
            "Found invalid value ", x, " for column ", col_name, " outside [", min, ", ", max, "]. ",
            "Value was replaced with ", ERROR_VAL_NUMERIC, "."
        )
    }

    x
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

        if (colname %in% c("fbg_updated_mmol", "fbg_updated_mg")) {
            df <- df %>% rename_with(~ str_replace(.x, "_mmol_date|_mg_date", "_date"))
        }

        df
    }


#' @title WIP: Convert dates that are stored as 5-digit number from Excel to date format
#'
#' @param date date as five number digit.
#'
#' @seealso https://stackoverflow.com/a/50274507: depending on whether read in
#' file was created on Mac or Windows, the origin date might need to be adjusted.
#'
#' @return Standardized date in a character format.
#'
#' @examples
#' fix_digit_date("44939") # res in "2023-01-13"
#'
fix_digit_date <-
    function(date) {
        if (is.na(date)) {
            return(NA_Date_)
        }

        if (str_detect(string = date, pattern = "^[:digit:]{5}$")) {
            date <- as.character(openxlsx::convertToDate(date))
        }
        date
    }


#' @title Try to handle invalid dates.
#'
#' @description
#' For each value of the character vector, this function uses the
#' lubridate parse functions to find common formats
#'
#' @param date character date value.
#'
#' @return Either a correctly parsed date or NA.
parse_dates <- function(date) {
    parsed_date <- suppressWarnings(lubridate::as_date(date))

    if (is.na(parsed_date)) {
        logWarn(
            "Could not parse date value ", date, ". ",
            "Trying to parse with lubridate::parse_date_time and orders = c('dmy', 'dmY', 'by', 'bY')."
        )
        orders <- c("dmy", "dmY", "dbY", "by", "bY")
        parsed_date <- lubridate::parse_date_time(date, orders)
    }

    parsed_date
}


#' @title Check value against list of allowed values.
#'
#' @param x character value to check.
#' @param valid_values list of valid options.
#' @param id patient id.
#' @param col column name when used with mutate/across.
#'
#' @return Either x or "Other".
check_allowed_values <- function(x, valid_values, id, col = "") {
    if (is.na(x) || x == "") {
        return(NA_character_)
    }

    if (!tolower(x) %in% tolower(valid_values)) {
        logWarn(
            "Patient ", id, ": Value ", x, "for column ", col, " is not in the list of allowed values. ",
            "Replacing it with ", ERROR_VAL_CHARACTER, "."
        )
        x <- ERROR_VAL_CHARACTER
    }

    x
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

#' @title Calculate bmi value based on weight and height.
#'
#' @description
#' Formula: weight (kg) / height^2 (meters^2)
#' bmi is set to NA if either weight or height are NA.
#' bmi is set to error val if either weight or height are error val.
#'
#' @param weight numeric weight value.
#' @param height numeric height value.
#' @param id patient id.
#'
#' @return calculated bmi value.
#' @export
fix_bmi <- function(weight, height, id) {
    bmi <- case_when(
        is.na(weight) || is.na(height) ~ NA_real_,
        weight == ERROR_VAL_NUMERIC || height == ERROR_VAL_NUMERIC ~ ERROR_VAL_NUMERIC,
        .default = weight / height^2
    )
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
        .default = ERROR_VAL_CHARACTER
    )

    if (!is.na(fixed_gender) && fixed_gender == ERROR_VAL_CHARACTER) {
        logWarn("Patient ", id, ": gender ", gender, " is not in the list of synonyms. Replacing it with ", fixed_gender, ".")
    }
    fixed_gender
}


#### t1d_diagnosis_age ####

#' @title Try to replace text descriptions for age with proper numerical values.
#'
#' @param t1d_diagnosis_age patient age when diagnosed with T1D.
#' @param t1d_diagnosis_date date when diagnosed with T1D.
#' @param id patient id
#'
#' @return Corrected value with text replacement.
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
    if (is.na(fbg) || fbg == "") {
        return(NA_character_)
    }

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


#### testing_fqr_pday ####

#' @title Fix problems with ranges and decimal numbers in testing_frequency.
#'
#' @param test_frq character value for testing frequency
#'
#' @return validated test_frq value
#' @export
fix_testing_frequency <- function(test_frq) {
    if (is.na(test_frq) || test_frq == "") {
        return(NA_character_)
    }

    if (grepl("-", test_frq, fixed = TRUE)) {
        logInfo("Found a range for testing_frequency. Replacing it with the mean.")
        test_frq <- try(as.character(replace_range_with_mean(test_frq), silent = TRUE))
    }

    test_frq
}


#' @title Replace a range with mean.
#'
#' @description
#' A range like "0-2" will be replaced with the mean(c(0,2)).
#'
#' @param x range as text..
#'
#' @return mean of that range
replace_range_with_mean <- function(x) {
    mean(as.numeric(unlist(str_split(x, "-"))))
}


#### insulin_regimen ####

#' Title
#'
#' @param d
#'
#' @return
#' @export
#'
#' @examples
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


#### blood_pressure_mmhg ####

#' @title Separate single blood pressure value into sys and dias values.
#'
#' @param df data frame with patient data.
#'
#' @return data frame with two new columns: blood_pressure_sys_mmhg and blood_pressure_dias_mmhg.
split_bp_in_sys_and_dias <- function(df) {
    logInfo("Splitting blood_pressure_mmhg into blood_pressure_sys_mmhg and blood_pressure_dias_mmhg.")
    df <- df %>% mutate(
        blood_pressure_mmhg = case_when(
            str_detect(blood_pressure_mmhg, "/", negate = T) ~ paste(ERROR_VAL_NUMERIC, ERROR_VAL_NUMERIC, sep = "/"),
            TRUE ~ blood_pressure_mmhg
        )
    )

    if (paste(ERROR_VAL_NUMERIC, ERROR_VAL_NUMERIC, sep = "/") %in% df$blood_pressure_mmhg) {
        logWarn(
            "Found invalid values for column blood_pressure_mmhg that do not follow the format X/Y. ",
            "Values were replaced with ", ERROR_VAL_NUMERIC, "."
        )
    }

    df <- df %>%
        separate_wider_delim(
            cols = blood_pressure_mmhg,
            delim = "/",
            names = c("blood_pressure_sys_mmhg", "blood_pressure_dias_mmhg"),
        )
    logDebug("Finished splitting blood_pressure_mmhg into blood_pressure_sys_mmhg and blood_pressure_dias_mmhg.")
    df
}


##### weight ####

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


#### height ####

transform_cm_to_m <- function(height) {
    height <- as.numeric(height)
    height <- ifelse(height > 50,
        height / 100,
        height
    )
    height
}


#### hospitalisation ####

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


#### dm_complication_* ####

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


##### dka_diag ####

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
# remarks, est_strips_pmonth
## 4. Final check if all variables have been transformed correctly

# testing on correct data:
# 1. Currently the fbg ranges will be excluded due to values out of realistic range.
# This is on purpose since we only know the units for specific hospitals/countries.
# In the fake data the hospital and country codes are fake. We need to test the
# function on the original data to ensure that the functions correctly transform
# the unit of the fbg values (and hence include them) in the data.
# See [12, 13] fbg and make adjustments if needed
