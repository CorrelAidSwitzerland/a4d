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
#' @param col_name column name if used with across. Optional, defaults to "".
#' @param id patient id, optional, defaults to "".
#'
#' @return converted value
#' @export
convert_to <- function(x, cast_fnc, error_val, col_name = "", id = "") {
    x <- tryCatch(
        cast_fnc(x),
        error = function(e) {
            logError(
                log_to_json(
                    message = "Error while trying to convert value {values['x']} in column {values['col_name']} for patient: {values['id']}. Replacing with error val {values['err_val']}.",
                    values = list(x = x, col_name = col_name, id = id, err_val = error_val),
                    script = "script2",
                    file = "helper_patient_data_fix.R",
                    functionName = "convert_to",
                    errorCode = "function_call"
                )
            )
            x <- error_val
        },
        warning = function(w) {
            logWarn(
                log_to_json(
                    message = "Warning while trying to convert value {values['x']} in column {values['col_name']} for patient: {values['id']}. Replacing with error val {values['err_val']}.",
                    values = list(x = x, col_name = col_name, id = id, err_val = error_val),
                    script = "script2",
                    file = "helper_patient_data_fix.R",
                    functionName = "convert_to",
                    errorCode = "function_call"
                )
            )
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
    if (is.na(x) || x == ERROR_VAL_NUMERIC) {
        return(x)
    }

    if (x < min || x > max) {
        logWarn(
            log_to_json(
                message = "Found invalid value {values['x']} for column {values['col_name']} outside [{values['min']}, {values['max']}]. Value was replaced with {values['error_val']}.",
                values = list(x = x, col_name = col_name, min = min, max = max, error_val = ERROR_VAL_NUMERIC),
                script = "script2",
                file = "helper_patient_data_fix.R",
                functionName = "cut_numeric_value",
                warningCode = "invalid_value"
            )
        )
        x <- ERROR_VAL_NUMERIC
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
    stringr::str_replace(x, ",", ".")
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
        df <- df %>%
            tidyr::separate_wider_regex(
                !!colname,
                c(
                    magrittr::set_names(".*", colname),
                    "[(]",
                    magrittr::set_names(".*?", paste0(colname, "_date")),
                    "[)]?"
                ),
                too_few = "align_start"
            )

        if (colname %in% c("fbg_updated_mmol", "fbg_updated_mg")) {
            df <- df %>%
                dplyr::rename_with(~ stringr::str_replace(.x, "_mmol_date|_mg_date", "_date"))
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
#' \dontrun{
#' fix_digit_date("44939") # should return "2023-01-13"
#' }
fix_digit_date <-
    function(date) {
        if (is.na(date)) {
            return(lubridate::NA_Date_)
        }

        if (stringr::str_detect(string = date, pattern = "^[:digit:]{5}$")) {
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
    if (is.na(date)) {
        return(NA_Date_)
    }

    parsed_date <- suppressWarnings(lubridate::as_date(date))

    if (is.na(parsed_date)) {
        orders <- c("dmy", "dmY", "dbY", "by", "bY", "mY", "my", "y")
        logWarn(
            log_to_json(
                message = "Could not parse date value {values['date']}. Trying to parse with lubridate::parse_date_time and orders = {values['orders']}.",
                values = list(date = date, orders = orders),
                script = "script2",
                file = "helper_patient_data_fix.R",
                functionName = "parse_dates",
                warningCode = "invalid_value"
            )
        )

        if (grepl("[[:alpha:]]{4}", date)) {
            date <- sub("([[:alpha:]]{3})[[:alpha:]]", "\\1", date)
        }
        parsed_date <- lubridate::parse_date_time(date, orders)
        parsed_date <- as.Date(parsed_date)
    }

    parsed_date
}


#' @title Check value against list of allowed values.
#'
#' @description
#' Invalid values are logged and depending on the replace invalid flag
#' also replaced with an error value. When checking for the validity of values
#' the case of the string is not considered nor are spaces and non-alphanumeric
#' characters. If the value matches an entry in the allowed_values list
#' that match is returned as the result.
#'
#' @param x character value to check.
#' @param valid_values list of valid options.
#' @param id patient id.
#' @param replace_invalid A flag controlling whether to also replace invalid .
#' @param error_val value that replaces invalid values.
#' @param col column name when used with mutate/across.
#'
#' @return Either x or "Other".
check_allowed_values <- function(x, valid_values, id, replace_invalid = TRUE, error_val = "Undefined", col = "") {
    if (is.na(x) || x == "") {
        return(NA_character_)
    }

    valid_value_mapping <- setNames(as.list(valid_values), sanitize_str(valid_values))

    if (!sanitize_str(x) %in% names(valid_value_mapping)) {
        logWarn(
            log_to_json(
                message = "Patient {values['id']}: Value {values['x']} for column {values['col']} is not in the list of allowed values. ",
                values = list(id = id, x = x, col = col),
                script = "script2",
                file = "helper_patient_data_fix.R",
                functionName = "check_allowed_values",
                warningCode = "invalid_value"
            )
        )

        if (replace_invalid) {
            logWarn(
                log_to_json(
                    message = "Replacing {values['x']} with {values['error_val']}.",
                    values = list(x = x, error_val = error_val),
                    script = "script2",
                    file = "helper_patient_data_fix.R",
                    functionName = "check_allowed_values",
                    warningCode = "invalid_value"
                )
            )

            x <- error_val
        }

        return(x)
    }

    valid_value_mapping[[sanitize_str(x)]]
}

parse_character_cleaning_pipeline <- function(column_name, column_config) {
    pipeline <- rlang::expr(
        !!rlang::parse_expr(column_name)
    )
    for (step in column_config$steps) {
        pipeline <- rlang::expr(!!pipeline %>% !!parse_step(column_name, step))
    }

    pipeline
}

parse_step <- function(column_name, step) {
    switch(step$type,
        allowed_values = parse_allowed_value_check(column_name, step),
        basic_function = rlang::expr((!!rlang::parse_expr(step$function_name))())
    )
}

parse_allowed_value_check <- function(column_name, check_details) {
    args <- rlang::exprs(
        valid_values = !!check_details$allowed_values,
        id = id,
        replace_invalid = !!check_details$replace_invalid,
        col = !!column_name
    )
    if ("error_value" %in% check_details) {
        args$error_val <- rlang::parse_expr(check_details$error_value)
    }
    rlang::expr(check_allowed_values(!!!args))
}

parse_character_cleaning_config <- function(config) {
    allowed_value_expr <- list()
    for (column in names(config)) {
        allowed_value_expr[[column]] <- parse_character_cleaning_pipeline(column, config[[column]])
    }

    allowed_value_expr
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

        if (is.na(age)) {
            logWarn(
                log_to_json(
                    message = "Patient {values['id']}: age is missing. Using calculated age {values['calc_age']} instead of original age.",
                    values = list(id = id, calc_age = calc_age),
                    script = "script2",
                    file = "helper_patient_data_fix.R",
                    functionName = "fix_age",
                    warningCode = "missing_value"
                )
            )
        } else {
            if (calc_age != age) {
                logWarn(
                    log_to_json(
                        message = "Patient {values['id']}: age {values['age']} is different from calculated age {values['calc_age']}. Using calculated age instead of original age.",
                        values = list(id = id, age = age, calc_age = calc_age),
                        script = "script2",
                        file = "helper_patient_data_fix.R",
                        functionName = "fix_age",
                        warningCode = "invalid_value"
                    )
                )
            }
        }

        if (calc_age < 0) {
            logWarn(
                log_to_json(
                    message = "Patient {values['id']}: calculated age is negative. Please check this manually. Using error value instead.",
                    values = list(id = id),
                    script = "script2",
                    file = "helper_patient_data_fix.R",
                    functionName = "fix_age",
                    warningCode = "invalid_value"
                )
            )

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
    bmi <- dplyr::case_when(
        is.na(weight) || is.na(height) ~ NA_real_,
        weight == ERROR_VAL_NUMERIC || height == ERROR_VAL_NUMERIC ~ ERROR_VAL_NUMERIC,
        .default = weight / height^2
    )


    if (!is.na(weight) && weight == ERROR_VAL_CHARACTER) {
        logWarn(
            log_to_json(
                message = "Patient {values['id']}: the weight is out of bounds.",
                values = list(id = id),
                script = "script2",
                file = "helper_patient_data_fix.R",
                functionName = "fix_bmi",
                warningCode = "invalid_value"
            )
        )
    }

    if (!is.na(height) && height == ERROR_VAL_CHARACTER) {
        logWarn(
            log_to_json(
                message = "Patient {values['id']}: the height is out of bounds.",
                values = list(id = id),
                script = "script2",
                file = "helper_patient_data_fix.R",
                functionName = "fix_bmi",
                warningCode = "invalid_value"
            )
        )
    }

    bmi
}


#### sex ####

#' @title Replace sex synonyms with either M, F or Other.
#'
#' @param sex patient gender.
#' @param id patient id.
#'
#' @return Either M, F or Other.
#' @export
fix_sex <- function(sex, id) {
    synonyms_female <- c("female", "girl", "woman", "fem", "feminine", "f")
    synonyms_male <- c("male", "boy", "man", "masculine", "m")

    fixed_sex <- dplyr::case_when(
        is.na(sex) | sex == "" ~ NA_character_,
        tolower(sex) %in% synonyms_female ~ "F",
        tolower(sex) %in% synonyms_male ~ "M",
        .default = ERROR_VAL_CHARACTER
    )

    if (!is.na(fixed_sex) && fixed_sex == ERROR_VAL_CHARACTER) {
        logWarn(
            log_to_json(
                message = "Patient {values['id']}: sex {values['sex']} is not in the list of synonyms. Replacing it with {values['fixed_sex']}.",
                values = list(id = id, sex = sex, fixed_sex = fixed_sex),
                script = "script2",
                file = "helper_patient_data_fix.R",
                functionName = "fix_sex",
                warningCode = "invalid_value"
            )
        )
    }

    fixed_sex
}


#### t1d_diagnosis_age ####

#' @title Try to replace text descriptions for age with proper numerical values.
#'
#' @param t1d_diagnosis_age patient age when diagnosed with T1D.
#' @param id patient id
#'
#' @return Corrected value with text replacement.
#' @export
fix_t1d_diagnosis_age <- function(t1d_diagnosis_age, id) {
    if (is.na(t1d_diagnosis_age)) {
        return(t1d_diagnosis_age)
    }

    age_corrected <- dplyr::case_when(
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
    fbg_mmol <- dplyr::case_when(
        measure_unit == "mg/dL" ~ fbg / factor_mmol_in_mg,
        measure_unit == "mmol/L" ~ fbg,
        is.na(measure_unit) ~ fbg,
        TRUE ~ NA_real_
    )

    as.numeric(fbg_mmol)
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
    fbg <- dplyr::case_when(
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
#' @param id patient id
#'
#' @return validated test_frq value
#' @export
fix_testing_frequency <- function(test_frq, id) {
    if (is.na(test_frq) || test_frq == "") {
        return(NA_character_)
    }

    if (grepl("-", test_frq, fixed = TRUE)) {
        logWarn(
            log_to_json(
                message = "Patient {values['id']}: Found a range for testing_frequency. Replacing it with the mean.",
                values = list(id = id),
                script = "script2",
                file = "helper_patient_data_fix.R",
                functionName = "fix_testing_frequency",
                warningCode = "invalid_value"
            )
        )

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
    mean(as.numeric(unlist(stringr::str_split(x, "-"))))
}


#### blood_pressure_mmhg ####

#' @title Separate single blood pressure value into sys and dias values.
#'
#' @param df data frame with patient data.
#'
#' @return data frame with two new columns: blood_pressure_sys_mmhg and blood_pressure_dias_mmhg.
split_bp_in_sys_and_dias <- function(df) {
    df <- df %>%
        dplyr::mutate(
            blood_pressure_mmhg = dplyr::case_when(
                stringr::str_detect(blood_pressure_mmhg, "/", negate = T) ~ paste(ERROR_VAL_NUMERIC, ERROR_VAL_NUMERIC, sep = "/"),
                TRUE ~ blood_pressure_mmhg
            )
        )

    if (paste(ERROR_VAL_NUMERIC, ERROR_VAL_NUMERIC, sep = "/") %in% df$blood_pressure_mmhg) {
        logWarn(
            log_to_json(
                message = "Found invalid values for column blood_pressure_mmhg that do not follow the format X/Y. Values were replaced with {values['err_val']}.",
                values = list(error_val = ERROR_VAL_NUMERIC),
                script = "script2",
                file = "helper_patient_data_fix.R",
                functionName = "split_bp_in_sys_and_dias",
                warningCode = "invalid_value"
            )
        )
    }

    df <- df %>%
        tidyr::separate_wider_delim(
            cols = blood_pressure_mmhg,
            delim = "/",
            names = c("blood_pressure_sys_mmhg", "blood_pressure_dias_mmhg"),
        )


    df
}


#### height ####

transform_cm_to_m <- function(height) {
    if (is.na(height) || height == ERROR_VAL_NUMERIC) {
        return(height)
    }

    height <- as.numeric(height)
    height <- ifelse(height > 50,
        height / 100,
        height
    )

    height
}


#### id ####
#' @title check patient id and truncate it if necessary.
#'
#' @param id patient id.
#'
#' @return Either original id, truncated id, or error value.
#' @export
fix_id <- function(id) {
    if (is.na(id)) {
        return(NA_character_)
    }

    id <- stringr::str_replace(id, "-", "_")

    if (!grepl("^[[:upper:]]{2}_[[:upper:]]{2}[[:digit:]]{3}$", id)) {
        logWarn(
            log_to_json(
                message = "Patient {values['id']}: id cannot be matched to a 8 letter alpha numeric code like XX_YY001.",
                values = list(id = id),
                script = "script2",
                file = "helper_patient_data_fix.R",
                functionName = "fix_id",
                warningCode = "invalid_value"
            )
        )

        if (stringr::str_length(id) > 8) {
            logWarn(
                log_to_json(
                    message = "Patient {values['id']}: id was truncated because it is longer than 8 characters.",
                    values = list(id = id),
                    script = "script2",
                    file = "helper_patient_data_fix.R",
                    functionName = "fix_id",
                    warningCode = "invalid_value"
                )
            )

            id <- stringr::str_sub(id, 1, 8)
        } else {
            logError(
                log_to_json(
                    message = "Patient {values['id']}: id is not valid. Replacing with error value {values['err_val']}.",
                    values = list(id = id, err_val = ERROR_VAL_CHARACTER),
                    script = "script2",
                    file = "helper_patient_data_fix.R",
                    functionName = "fix_id",
                    errorCode = "invalid_value"
                )
            )

            id <- ERROR_VAL_CHARACTER
        }
    }

    id
}


#### insulin regimen ####

#' @title Extract the part of a string before a comma
#'
#' @param string The input string
#'
#' @return The extracted part.
extract_first_raw_regimen <- function(string) {
    stringr::str_extract(string, "[^,]*")
}

#' @title Maps Insulin regiment with allowed values.
#'
#' @description
#' The mapping is done using regular expressions.
#'
#'
#' @param raw_input The string that needs to be mapped
#'
#' @return The mapping result
extract_regimen <- function(raw_input) {
    output <- sub("^.*basal.*$", "Basal-bolus (MDI)", raw_input, ignore.case = TRUE)
    output <- sub("^.*premixed.*$", "Premixed 30/70 BD", output, ignore.case = TRUE)
    output <- sub("^.*self-mixed.*$", "Self-mixed BD", output, ignore.case = TRUE)
    output <- sub("^.*conventional.*$", "Modified conventional TID", output, ignore.case = TRUE)

    output
}
