#' Process and clean patient raw data
#'
#' @description
#' Read in the output of the first script as parquet (all character).
#' Merge extracted patient data with the metadata schema that contains all
#' necessary columns. Process each variable in three steps:
#' 1. Fix any problems with character representation.
#' 2. Convert column to target data type, for example a date or number.
#' 3. Apply any post-processing logic for the target data type.
#'
#'
#' @param patient_file Path to patient raw parquet file from first script.
#' @param paths List with paths to patient_data_cleaned and product_data_cleaned folders.
#' @param allowed_provinces A named character vector with names of allowed provinces.
#'
#' @export
process_patient_file <- function(patient_file, paths, allowed_provinces) {
    ERROR_VAL_NUMERIC <<- 999999
    ERROR_VAL_CHARACTER <<- "Undefined"
    ERROR_VAL_DATE <<- "9999-09-09"

    patient_file_name <- tools::file_path_sans_ext(basename(patient_file))
    patient_file_path <-
        file.path(paths$tracker_root, patient_file)
    output_root <- paths$patient_data_cleaned

    logDebug("Start process_patient_file.")
    logInfo(
        "Current file: ",
        patient_file_name
    )

    logfile <- paste0(patient_file_name)

    with_file_logger(logfile,
        {
            tryCatch(
                process_patient_file_worker(
                    patient_file_path = patient_file_path,
                    patient_file_name = patient_file_name,
                    output_root = output_root,
                    allowed_provinces = allowed_provinces
                ),
                error = function(e) {
                    logError("Could not process raw patient data. Error = ", e$message, ".")
                },
                warning = function(w) {
                    logWarn("Could not process raw patient data. Warning = ", w$message, ".")
                }
            )
        },
        output_root = paths$output_root
    )

    logInfo("Finish process_patient_file.")
}


process_patient_file_worker <- function(patient_file_path, patient_file_name, output_root, allowed_provinces) {
    df_patient_raw <- arrow::read_parquet(patient_file_path)

    # filter all rows with no patient id or patient name
    df_patient_raw <- df_patient_raw %>%
        dplyr::filter(!(is.na(id) & is.na(name))) %>%
        dplyr::filter(!(id == "0" & name == "0"))

    # --- TRANSFORMATIONS ---
    # data before 2019 had only one column for updated hba1c and fbg
    # with date as part of the value
    if (!"hba1c_updated_date" %in% colnames(df_patient_raw) && "hba1c_updated" %in% colnames(df_patient_raw)) {
        logInfo("Column updated_hba1c_date not found. Trying to parse from hba1c_updated.")
        df_patient_raw <-
            extract_date_from_measurement(df_patient_raw, "hba1c_updated")
        logDebug("Finished parsing dates from hba1c_updated.")
    }

    if (!"fbg_updated_date" %in% colnames(df_patient_raw) && "fbg_updated_mg" %in% colnames(df_patient_raw)) {
        logInfo("Column updated_fbg_date not found. Trying to parse from fbg_updated_mg.")
        df_patient_raw <-
            extract_date_from_measurement(df_patient_raw, "fbg_updated_mg")
        logDebug("Finished parsing dates from fbg_updated_mg.")
    }

    if (!"fbg_updated_date" %in% colnames(df_patient_raw) && "fbg_updated_mmol" %in% colnames(df_patient_raw)) {
        logInfo("Column fbg_updated_date not found. Trying to parse from fbg_updated_mmol.")
        df_patient_raw <-
            extract_date_from_measurement(df_patient_raw, "fbg_updated_mmol")
        logDebug("Finished parsing dates from fbg_updated_mmol.")
    }

    # blood pressure is given as sys/dias value pair,
    # so we split this column in two separate columns
    if ("blood_pressure_mmhg" %in% colnames(df_patient_raw)) {
        df_patient_raw <- split_bp_in_sys_and_dias(df_patient_raw)
    }

    # The maximum value available for hba1c will be around 14% - 18%,
    # depending on the equipment being used.
    # If the reading is above the maximum available value the > sign is used -
    # we would prefer to retain this character in the database as it is important for data analysis.
    logInfo("Adding columns hba1c_baseline_exceeds and hba1c_updated_exceeds.")
    df_patient_raw <- df_patient_raw %>%
        dplyr::mutate(
            hba1c_baseline_exceeds = ifelse(grepl(">|<", hba1c_baseline), TRUE, FALSE),
            hba1c_updated_exceeds = ifelse(grepl(">|<", hba1c_updated), TRUE, FALSE)
        )

    # --- META SCHEMA ---
    # meta schema has all final columns for the database
    # along with their corresponding data types
    logInfo("Creating meta schema.")
    # short type string for read_csv:
    # iiinDccDcnnDnncnlnlDncDccDDDccccDccccciDciiiDn
    schema <- tibble::tibble(
        # clinic_visit = logical(),
        # complication_screening = character(),
        # complication_screening_date = lubridate::as_date(1),
        # complication_screening_results = character(),
        # dm_complication_comment = character(), # TODO
        # dm_complication_eye = character(), # TODO
        # dm_complication_kidney = character(), # TODO
        # dm_complication_other = character(), # TODO
        # est_strips_pmonth = integer(),
        # family_support_scale = character(), # TODO
        # inactive_reason = character(),
        # insulin_dosage = character(),
        # meter_received_date = lubridate::as_date(1), # TODO
        # remarks = character(),
        # remote_followup = logical(),
        # additional_support = character(),
        age = integer(),
        blood_pressure_dias_mmhg = integer(),
        blood_pressure_sys_mmhg = integer(),
        bmi = numeric(),
        bmi_date = lubridate::as_date(1),
        clinic_code = character(),
        country_code = character(),
        dob = lubridate::as_date(1),
        edu_occ = character(),
        fbg_baseline_mg = numeric(),
        fbg_baseline_mmol = numeric(),
        fbg_updated_date = lubridate::as_date(1),
        fbg_updated_mg = numeric(),
        fbg_updated_mmol = numeric(),
        file_name = character(),
        hba1c_baseline = numeric(),
        hba1c_baseline_exceeds = logical(),
        hba1c_updated = numeric(),
        hba1c_updated_exceeds = logical(),
        hba1c_updated_date = lubridate::as_date(1),
        height = numeric(),
        hospitalisation_cause = character(),
        hospitalisation_date = lubridate::as_date(1),
        id = character(),
        insulin_regimen = character(),
        last_clinic_visit_date = lubridate::as_date(1),
        last_remote_followup_date = lubridate::as_date(1),
        lost_date = lubridate::as_date(1),
        name = character(),
        observations = character(),
        observations_category = character(),
        province = character(),
        recruitment_date = lubridate::as_date(1),
        sex = character(),
        sheet_name = character(),
        status = character(),
        status_out = character(),
        support_from_a4d = character(),
        t1d_diagnosis_age = integer(),
        t1d_diagnosis_date = lubridate::as_date(1),
        t1d_diagnosis_with_dka = character(),
        testing_frequency = integer(),
        tracker_date = lubridate::as_date(1),
        tracker_month = integer(),
        tracker_year = integer(),
        updated_2022_date = lubridate::as_date(1),
        weight = numeric()
    )

    cols_extra <- colnames(df_patient_raw)[!colnames(df_patient_raw) %in% colnames(schema)]
    logWarn("Extra columns in patient data: ", paste(cols_extra, collapse = ", "))

    cols_missing <-
        colnames(schema)[!colnames(schema) %in% colnames(df_patient_raw)]
    logWarn("Missing columns in patient data: ", paste(cols_missing, collapse = ", "))

    # add all columns of schema to df_patient_raw
    # keep all rows, only append missing cols
    logInfo("Merging df_patient with meta schema and selecting all columns of meta schema.")
    df_patient <- merge.default(df_patient_raw, schema, all.x = T)
    df_patient <- df_patient[colnames(schema)]

    # the cleaning, fixing and validating happens in three major steps:
    # 1. make sure we fix any known problems in the raw character columns
    df_patient <-
        df_patient %>%
        dplyr::rowwise() %>%
        # 1. handle known problems before converting to target type
        dplyr::mutate(
            t1d_diagnosis_age = fix_t1d_diagnosis_age(t1d_diagnosis_age, id),
            hba1c_baseline = stringr::str_replace(hba1c_baseline, "<|>", ""),
            hba1c_updated = stringr::str_replace(hba1c_updated, "<|>", ""),
            fbg_baseline_mg = fix_fbg(fbg_baseline_mg),
            fbg_baseline_mmol = fix_fbg(fbg_baseline_mmol),
            fbg_updated_mg = fix_fbg(fbg_updated_mg),
            fbg_updated_mmol = fix_fbg(fbg_updated_mmol),
            testing_frequency = fix_testing_frequency(testing_frequency)
        ) %>%
        # 2. convert the refined character columns into the target data type
        dplyr::mutate(
            dplyr::across(
                schema %>% dplyr::select(tidyselect::where(is.numeric)) %>% names(),
                \(x) convert_to(correct_decimal_sign(x), as.numeric, ERROR_VAL_NUMERIC, dplyr::cur_column(), id = id)
            ),
            dplyr::across(
                schema %>% dplyr::select(tidyselect::where(is.logical)) %>% names(),
                \(x) convert_to(x, as.logical, FALSE, dplyr::cur_column(), id = id)
            ),
            dplyr::across(
                schema %>% dplyr::select(tidyselect::where(lubridate::is.Date)) %>% names(),
                \(x) convert_to(fix_digit_date(x), parse_dates, as.Date(ERROR_VAL_DATE), dplyr::cur_column(), id = id)
            ),
            dplyr::across(
                schema %>% dplyr::select(tidyselect::where(is.integer)) %>% names(),
                \(x) convert_to(x, function(x) as.integer(round(as.double(x))), ERROR_VAL_NUMERIC, dplyr::cur_column(), id = id)
            )
        ) %>%
        # 3. fix remaining problems in the target data type
        dplyr::mutate(
            # height and weight are needed to calculate bmi
            height = transform_cm_to_m(height) %>%
                cut_numeric_value(min = 0, max = 2.3, col_name = "height"),
            weight = cut_numeric_value(weight, min = 0, max = 200, col_name = "weight"),
            bmi = fix_bmi(weight, height, id) %>%
                cut_numeric_value(min = 4, max = 60, "bmi"),
            age = fix_age(age, dob, tracker_year, tracker_month, id) %>%
                cut_numeric_value(min = 0, max = 25, "age"),
            sex = fix_sex(sex, id),
            hba1c_baseline = cut_numeric_value(hba1c_baseline, min = 4, max = 18, "hba1c_baseline"),
            # https://www.cleveland19.com/story/1425584/ohio-man-holds-world-record-of-highest-blood-sugar/
            fbg_baseline_mmol = cut_numeric_value(fbg_baseline_mmol, min = 0, max = 136.5, "fbg_baseline_mmol"),
            # https://www.cleveland19.com/story/1425584/ohio-man-holds-world-record-of-highest-blood-sugar/
            fbg_updated_mmol = cut_numeric_value(fbg_updated_mmol, min = 0, max = 136.5, "fbg_updated_mmol"),
            blood_pressure_sys_mmhg = cut_numeric_value(blood_pressure_sys_mmhg, min = 20, max = 250, "blood_pressure_sys_mmhg"),
            blood_pressure_dias_mmhg = cut_numeric_value(blood_pressure_dias_mmhg, min = 20, max = 220, "blood_pressure_dias_mmhg"),
            tracker_date = lubridate::ym(paste(tracker_year, tracker_month, sep = "-")),
            !!!parse_character_cleaning_config(a4d:::config$cleaning),
            # should be fixed last as other fix functions use id to log invalid rows!
            id = fix_id(id)
        ) %>%
        dplyr::ungroup()

    # add clinic and country code after having fixed all issues with patient id
    cc_codes <- extract_country_clinic_code(df_patient)
    df_patient["clinic_code"] <- cc_codes$clinic_code
    df_patient["country_code"] <- cc_codes$country_code

    # Formula to calculate mmol/l from mg/dl: mmol/l = mg/dl / 18
    if (all(is.na(df_patient$fbg_baseline_mmol))) {
        df_patient <- df_patient %>%
            dplyr::mutate(fbg_baseline_mmol = dplyr::case_when(
                fbg_baseline_mg != ERROR_VAL_NUMERIC ~ fbg_baseline_mg / 18
            ))
    }
    if (all(is.na(df_patient$fbg_updated_mmol))) {
        df_patient <- df_patient %>%
            dplyr::mutate(fbg_updated_mmol = dplyr::case_when(
                fbg_updated_mg != ERROR_VAL_NUMERIC ~ fbg_updated_mg / 18
            ))
    }

    # Formula to calculate mg/dl from mmol/l: mg/dl = 18 × mmol/l
    if (all(is.na(df_patient$fbg_baseline_mg))) {
        df_patient <- df_patient %>%
            dplyr::mutate(fbg_baseline_mg = dplyr::case_when(
                fbg_baseline_mmol != ERROR_VAL_NUMERIC ~ fbg_baseline_mmol * 18
            ))
    }
    if (all(is.na(df_patient$fbg_updated_mg))) {
        df_patient <- df_patient %>%
            dplyr::mutate(fbg_updated_mg = dplyr::case_when(
                fbg_updated_mmol != ERROR_VAL_NUMERIC ~ fbg_updated_mmol * 18
            ))
    }

    # sort by year and month like it is in the tracker files
    df_patient <- df_patient %>%
        dplyr::arrange(tracker_date, id)

    logDebug(
        "df_patient dim: ",
        dim(df_patient) %>% as.data.frame(),
        "."
    )

    export_data_as_parquet(
        data = df_patient,
        filename = stringr::str_replace(patient_file_name, "_patient_raw", ""),
        output_root = output_root,
        suffix = "_patient_cleaned"
    )
}


#' Process and clean product raw data
#'
#' @description
#' A short description...
#'
#'
#' @param product_file Path to product raw parquet file from first script.
#' @param paths List with paths to patient_data_cleaned and product_data_cleaned folders.
#' @param synonyms_product Synonyms for product data header names.
#'
#' @export
process_product_file <- function(product_file, paths, synonyms_product) {
    product_file_name <- tools::file_path_sans_ext(basename(product_file))
    product_file_path <-
        file.path(paths$tracker_root, product_file)

    logDebug("Start process_product_file.")
    logInfo(
        "Current file: ",
        product_file_name
    )
    df_product_raw <- arrow::read_parquet(product_file_path)

    logfile <- paste0(product_file_name)
    with_file_logger(logfile,
        {
            tryCatch(
                df_product_raw <- reading_product_data_step2(df_product_raw, synonyms_product),
                error = function(e) {
                    logError("Could not process raw product data. Error = ", e$message, ".")
                },
                warning = function(w) {
                    logWarn("Could not process raw product data. Warning = ", w$message, ".")
                }
            )

            logDebug(
                "df_product_raw dim: ",
                dim(df_product_raw) %>% as.data.frame(),
                "."
            )

            export_data_as_parquet(
                data = df_product_raw,
                filename = stringr::str_replace(product_file_name, "_product_raw", ""),
                output_root = paths$product_data_cleaned,
                suffix = "_product_cleaned"
            )
        },
        output_root = paths$output_root
    )

    logInfo("Finish process_product_file.")
}
