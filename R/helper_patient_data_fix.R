# DESCRIPTION -------------------------------------------------------------

# This function cleans the output of the "tidy" function by reformatting certain columns.
# Basic functions are created and then applied to each piece of data available in one row.

# Base Functions ####

#' @title Try to convert x into a target type.
#'
#' @description
#' Use cast_fnc to cast x to target type.
#' If that cast fails, output error_val instead.
#' If x is already NA, output NA
#'
#' @param x value to convert to numeric
#' @param cast_fnc function to use to cast x
#' @param error_val error value to use when as.numeric fails
#'
#' @return converted x
#' @export
convert_to <- function(x, cast_fnc, error_val) {
    x <- tryCatch(
        cast_fnc(x),
        error = function(e) x <- error_val,
        warning = function(w) x <- error_val
    )

    if (length(x) == 0) {
        x <- error_val
    }

    x
}


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
#'
#' @return fixed age.
#' @export
fix_age <- function(age, dob, tracker_year, tracker_month) {
    calc_age <- age

    if (!is.na(dob)) {
        dob <- as.Date(dob)
        calc_age <- tracker_year - lubridate::year(dob)
        delta_month <- tracker_month - lubridate::month(dob)
        if (delta_month < 0) {
            calc_age <- calc_age - 1
        }

        if (calc_age != age) {
            logInfo("Age ", age, " is different from calculated age ", calc_age, ". Using calculated age instead of original age.")
        }

        if (calc_age < 0) {
            logInfo("Calculated age is negative. Something went wrong.")
            calc_age <- 999999
        }
    }

    calc_age
}


check_numeric_borders <- function(x,
                                  min,
                                  max) {
    x <- as.numeric(x)
    x <- ifelse(x > max,
        NA_real_,
        x
    )
    x <- ifelse(x < min,
        NA_real_,
        x
    )
}

replace_empty_string_with_NA <- function(string_vector) {
    output <- ifelse(string_vector == "", NA, string_vector)
}

# gender ####

## Synonyms for gender
par_synonyms_lower_female <-
    c("female", "girl", "woman", "fem", "feminine", "f")
par_synonyms_lower_male <- c("male", "boy", "man", "masculine", "m")

replace_gender_synonyms <- function(d,
                                    synonyms_f = par_synonyms_lower_female,
                                    synonyms_m = par_synonyms_lower_male) {
    y <- case_when(
        tolower(d) %in% synonyms_f ~ "F",
        tolower(d) %in% synonyms_m ~ "M",
        TRUE ~ "Other"
    )
}

fix_gender <- function(d) {
    cleaned_gender <- try(replace_gender_synonyms(d), silent = TRUE)
    if (class(cleaned_gender) == "try-error") {
        cleaned_gender <- 999999
    }
    return(cleaned_gender)
}




# age_diagnosis ####
# ______________________________________________


# Description: Transforms "1 y 6m" to "1.5" age, if not possible return NA
extract_age_from_y_m <- function(age) {
    final_age <- NA_real_
    suppressWarnings(if (is.na(as.numeric(age))) {
        age_str <- str_split(age, " |y|m") %>%
            unlist()
        age_str <- subset(age_str, age_str != "")

        if (length(age_str) > 1) {
            years <- as.numeric(age_str[1])
            months <- mean(as.numeric(age_str[-1]))
        } else {
            years <- age_str
            months <- 0
        }

        final_age <- years + (round(months / 12, 1))
    })
    return(final_age)
}

# @Description: Checks different possibilities for age of birth and transforms it into numeric age
handle_age_diagnosis <- function(age_x) {
    suppressWarnings(age_corrected <- ifelse(
        grepl("birth|born", tolower(age_x)),
        0,
        ifelse(
            !is.na(as.numeric(age_x)),
            as.numeric(age_x),
            extract_age_from_y_m(age_x)
        )
    ))
    return(age_corrected)
}

# @Description: Try to transform age of diagnosis. If error occur give out 99999
fix_age_diagnosis <- function(d) {
    if (!is.na(d)) {
        d <- try(handle_age_diagnosis(d), silent = TRUE)
        if (class(d) == "try-error") {
            d <- 99999
        }
    } else {
        d <- NA
    }

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
                                      upper_hb1c) {
    # cases where there is a comma and not a dot
    x <- str_replace(x, ",", ".")
    # cases with signs
    x <- ifelse(grepl(c("<|>"), x), "999999", x)
    x <- as.numeric(x)
    d <- ifelse((x < lower_hb1c | x > upper_hb1c), 999999, x)

    return(d)
}

fix_hba1c <- function(d) {
    if (!is.na(d)) {
        d <-
            try(exclude_unrealistic_hba1c(d, par_lower_hb1c, par_upper_hb1c),
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
mmol_hospitals <-
    c("Clinic_LU", "Clinic_PE", "Clinic_YA", "Clinic_PU")
mg_countries <- c("Country_1", "Country_2", "Country_3")
mg_hospitals <- c(
    "Clinic_DW",
    "Clinic_TN",
    "Clinic_EO",
    "Clinic_VF",
    "Clinic_BR",
    "Clinic_KH",
    "Clinic_XD",
    "Clinic_QG",
    "Clinic_YB",
    "Clinic_FY",
    "Clinic_CJ",
    "Clinic_VW",
    "Clinic_IX",
    "Clinic_YA",
    "Clinic_ZB",
    "Clinic_EU",
    "Clinic_IH"
)

# @Description: For a given country/hospital the fbg unit is returned
# @country_id: ID of the country where patient values were taken
# @hospital_id: ID of the hospital where patient values were taken
# @Output: String with fbg unit "mg/dL", "mmol/L" or NA
assign_fbg_unit_per_hospital <- function(hospital_id,
                                         country_id,
                                         mmol_ct = mmol_countries,
                                         mmol_hos = mmol_hospitals,
                                         mg_ct = mg_countries,
                                         mg_hos = mg_hospitals) {
    returned_unit <- case_when(
        is.na(hospital_id) & is.na(country_id) ~ NA_character_,
        hospital_id %in% mg_hos ~ "mg/dL",
        country_id %in% mg_ct ~ "mg/dL",
        hospital_id %in% mmol_hos ~ "mmol/L",
        country_id %in% mmol_ct ~ "mmol/L",
        TRUE ~ NA_character_
    )

    if (is.na(returned_unit)) {
        warning(
            "FBG unit used by hospital could not be matched ~ Assumed to be mmol/L. Check if allocation of real hospitals to mg/mmol unit was performed within the code (See 02_a4d_patient_tracker_format.R fbg_mgdl."
        )
    }
    return(returned_unit)
}

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


fbg_mmol_lower_bound <- 0
fbg_mmol_upper_bound <-
    136.5 # https://www.cleveland19.com/story/1425584/ohio-man-holds-world-record-of-highest-blood-sugar/

# @Description: Check if FBG value is realistic
# @fbg_mmol: FBG value in mmol/L
# @fbg_min, fbg_max: Lower and upper bound of realistic fbg values
# @Output: FBG mmol value if no error. Otherwise raised error & NA
sanity_check_fbg_mmol <-
    function(fbg_mmol,
             min_fbg = fbg_mmol_lower_bound,
             max_fbg = fbg_mmol_upper_bound) {
        fbg_result <- case_when(
            fbg_mmol >= min_fbg &
                fbg_mmol <= max_fbg ~ fbg_mmol,
            TRUE ~ NA_real_
        )

        if (is.na(fbg_result)) {
            stop("ERROR: FBG value outside realistic scale")
        }

        return(fbg_result)
    }

# @Description: FBG input is often a range (200-300) but functions only
# work with unique values. This wrapper hence loops the range through
# the functions.
# @hid: Hospital ID
# @cid: country ID
fbg_wrapper <- function(fbg_range, hid, cid) {
    # Source for levels: https://www.cdc.gov/diabetes/basics/getting-tested.html
    fbg_range <- case_when(
        grepl("high|bad|hi", tolower(fbg_range)) ~ "200",
        grepl("med|medium", tolower(fbg_range)) ~ "140-199",
        grepl("low|good|okay", tolower(fbg_range)) ~ "140",
        TRUE ~ fbg_range
    ) %>%
        gsub(pattern = "(DKA)", replacement = "", fixed = T)


    lower_upper_fbg <- fbg_range %>%
        str_split("-") %>%
        unlist() %>%
        as.numeric()

    for (i in 1:length(lower_upper_fbg)) {
        lower_upper_fbg[i] <- sanity_check_fbg_mmol(transform_fbg_in_mmol(
            lower_upper_fbg[i],
            country_id = cid,
            hospital_id = hid
        ))
    }

    final <- paste(lower_upper_fbg, collapse = "-")
}


fbg_fix <- function(fbg, country, hospital) {
    d <- try(fbg_wrapper(fbg, cid = country, hid = hospital),
        silent = TRUE
    )
    if (class(d) == "try-error") {
        d <- "999999"
    }
    return(d)
}


# "support_from_a4d" ####
# ______________________________________________
#### SUPPORT A4D
supporta4d_fix <- function(d) {
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
                "Others"
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
            check_numeric_borders(
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
            check_numeric_borders(
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
            try(check_numeric_borders(d, par_max_weight_kg, par_min_weight_kg),
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
            check_numeric_borders(
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

# "bmi" ####
# ______________________________________________
bmi_upper_limit <- 60
bmi_lower_limit <- 4


#' @title Check valid values for bmi.
#'
#' @description
#' Check if bmi is between 60 and 4, set NA otherwise.
#' Set bmi to NA if either weight or height is NA.
#'
#' @param bmi bmi value.
#' @param height height value.
#' @param weight weight value.
#'
#' @return data frame with corrected bmi column.
fix_bmi <- function(bmi, height, weight) {
    if (is.na(weight) | is.na(height)) {
        logInfo("Found rows with either height or weight values missing. Setting bmi to NA.")
        bmi <- NA_real_
    }

    bmi <- check_numeric_borders(bmi, min = bmi_lower_limit, max = bmi_upper_limit)
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
