options(readxl.show_progress = FALSE)
options(future.rng.onMisuse = "ignore")
ERROR_VAL_NUMERIC <<- 999999
ERROR_VAL_CHARACTER <<- "Other"
ERROR_VAL_DATE <<- "9999-09-09"

`%dopar%` <- foreach::`%dopar%`

source("R/helper_main.R")
source("R/read_patient_data.R")
source("R/helper_read_patient_data.R")
source("R/helper_patient_data_fix.R")
source("R/read_product_data.R")
source("R/helper_clean_data.R")
source("R/helper_product_data.R")
source("R/get_tracker_year.R")
source("R/logger.R")

main <- function() {
    paths <- init_paths(c("patient_data_cleaned", "product_data_cleaned"), delete = TRUE)
    setup_logger(paths$output_root)
    patient_data_files <- get_files(paths$tracker_root, pattern = "patient_raw.csv$")
    product_data_files <- get_files(paths$tracker_root, pattern = "product_raw.csv$")
    logInfo(
        "Found ",
        length(patient_data_files),
        " patient csv files under ",
        paths$tracker_root,
        "."
    )
    logInfo(
        "Found ",
        length(product_data_files),
        " product csv files under ",
        paths$tracker_root,
        "."
    )

    logDebug("Start processing patient csv files.")

    foreach::foreach(patient_file = patient_data_files) %dopar% {
        patient_file_name <- tools::file_path_sans_ext(basename(patient_file))
        logfile <- paste0(patient_file_name)
        setup_file_logger(paths$output_root, logfile)
        tryCatch(
            process_patient_file(paths, patient_file, patient_file_name, paths$patient_data_cleaned),
            error = function(e) {
                logError("Could not process raw patient data. Error = ", e, ".")
            },
            warning = function(w) {
                logWarn("Could not process raw patient data. Warning = ", w, ".")
            },
            finally = unregisterLogger(logfile)
        )
    }

    logInfo("Finish processing all patient csv files.")

    logDebug("Start processing product csv files.")
    synonyms <- get_synonyms()
    synonyms_product <- synonyms$product

    foreach::foreach(product_file = product_data_files) %dopar% {
        product_file_name <- tools::file_path_sans_ext(basename(product_file))
        logfile <- paste0(product_file_name)
        setup_file_logger(paths$output_root, logfile)
        tryCatch(
            process_product_file(paths, product_file, product_file_name, synonyms_product, paths$product_data_cleaned),
            error = function(e) {
                logError("Could not process raw product data. Error = ", e, ".")
            },
            warning = function(w) {
                logWarn("Could not process raw product data. Warning = ", w, ".")
            },
            finally = unregisterLogger(logfile)
        )
    }

    logInfo("Finish processing all csv files.")
}


process_patient_file <- function(paths, patient_file, patient_file_name, output_root) {
    patient_file_path <-
        file.path(paths$tracker_root, patient_file)
    logDebug("Start process_patient_file.")
    logInfo(
        "Current file: ",
        patient_file_name
    )

    df_patient_raw <- read_raw_csv(patient_file_path)

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
        mutate(
            hba1c_baseline_exceeds = ifelse(grepl(">|<", hba1c_baseline), TRUE, FALSE),
            hba1c_updated_exceeds = ifelse(grepl(">|<", hba1c_updated), TRUE, FALSE)
        )

    # --- META SCHEMA ---
    # meta schema has all final columns for the database
    # along with their corresponding data types
    logInfo("Creating meta schema.")
    schema <- tibble::tibble(
        # clinic_visit = logical(),
        # complication_screening = character(),
        # complication_screening_date = as.Date(1),
        # complication_screening_results = character(),
        # dm_complication_comment = character(), # TODO
        # dm_complication_eye = character(), # TODO
        # dm_complication_kidney = character(), # TODO
        # dm_complication_other = character(), # TODO
        # est_strips_pmonth = integer(),
        # family_support_scale = character(), # TODO
        # inactive_reason = character(),
        # insulin_dosage = character(),
        # meter_received_date = as.Date(1), # TODO
        # remarks = character(),
        # additional_support = character(),
        age = integer(),
        blood_pressure_dias_mmhg = integer(),
        blood_pressure_sys_mmhg = integer(),
        bmi = numeric(),
        bmi_date = as.Date(1),
        clinic_code = character(),
        country_code = character(),
        dob = as.Date(1),
        edu_occ = character(),
        fbg_baseline_mg = numeric(),
        fbg_baseline_mmol = numeric(),
        fbg_updated_date = as.Date(1),
        fbg_updated_mg = numeric(),
        fbg_updated_mmol = numeric(),
        file_name = character(),
        gender = character(),
        hba1c_baseline = numeric(),
        hba1c_baseline_exceeds = logical(),
        hba1c_updated = numeric(),
        hba1c_updated_exceeds = logical(),
        hba1c_updated_date = as.Date(1),
        height = numeric(),
        hospitalisation_cause = character(),
        hospitalisation_date = as.Date(1),
        id = character(),
        insulin_regimen = character(),
        last_clinic_visit_date = as.Date(1),
        last_remote_followup_date = as.Date(1),
        lost_date = as.Date(1),
        name = character(),
        observations = character(),
        observations_category = character(),
        province = character(),
        recruitment_date = as.Date(1),
        remote_followup = as.Date(1),
        sheet_name = character(),
        status = character(),
        status_out = character(),
        support_from_a4d = character(),
        t1d_diagnosis_age = integer(),
        t1d_diagnosis_date = as.Date(1),
        t1d_diagnosis_with_dka = character(),
        testing_frequency = integer(),
        tracker_month = integer(),
        tracker_year = integer(),
        updated_2022_date = as.Date(1),
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
    logInfo("Applying fix functions phase 1: mutate df_patient row-wise to fix all problems with character columns.")
    df_patient <-
        df_patient %>%
        rowwise() %>%
        # 1. handle known problems before converting to target type
        mutate(
            t1d_diagnosis_age = fix_t1d_diagnosis_age(t1d_diagnosis_age, t1d_diagnosis_date, id),
            hba1c_baseline = str_replace(hba1c_baseline, "<|>", ""),
            hba1c_updated = str_replace(hba1c_updated, "<|>", ""),
            fbg_baseline_mg = fix_fbg(fbg_baseline_mg),
            fbg_baseline_mmol = fix_fbg(fbg_baseline_mmol),
            fbg_updated_mg = fix_fbg(fbg_updated_mg),
            fbg_updated_mmol = fix_fbg(fbg_updated_mmol),
            testing_frequency = fix_testing_frequency(testing_frequency)
        ) %>%
        ungroup()

    # 2. convert the refined character columns into the target data type
    logInfo("Applying fix functions phase 2: mutate df_patient row-wise to convert character columns into target type.")
    df_patient <-
        df_patient %>%
        rowwise() %>%
        mutate(
            across(
                schema %>% select(where(is.numeric)) %>% names(),
                \(x) convert_to(correct_decimal_sign(x), as.numeric, ERROR_VAL_NUMERIC, cur_column())
            ),
            across(
                schema %>% select(where(is.logical)) %>% names(),
                \(x) convert_to(x, as.logical, FALSE, cur_column())
            ),
            across(
                schema %>% select(where(is.Date)) %>% names(),
                \(x) convert_to(fix_digit_date(x), parse_dates, as.Date(ERROR_VAL_DATE), cur_column())
            ),
            across(
                schema %>% select(where(is.integer)) %>% names(),
                \(x) convert_to(x, function(x) as.integer(round(as.double(x))), ERROR_VAL_NUMERIC, cur_column())
            )
        ) %>%
        ungroup()

    # 3. fix any remaining issues in the target data type
    logInfo("Applying fix functions phase 3: mutate df_patient row-wise to fix all problems in the target data type.")
    df_patient <-
        df_patient %>%
        rowwise() %>%
        # 3. fix remaining problems in the target data type
        mutate(
            # height and weight are needed to calculate bmi
            height = transform_cm_to_m(height) %>% cut_numeric_value(min = 0, max = 2.3),
            weight = cut_numeric_value(weight, min = 0, max = 200),
            bmi = fix_bmi(weight, height, id) %>% cut_numeric_value(min = 4, max = 60, "bmi"),
            age = fix_age(age, dob, tracker_year, tracker_month, id) %>% cut_numeric_value(min = 0, max = 25, "age"),
            sex = fix_sex(sex, id),
            hba1c_baseline = cut_numeric_value(hba1c_baseline, min = 4, max = 18, "hba1c_baseline"),
            fbg_baseline_mmol = cut_numeric_value(fbg_baseline_mmol, min = 0, max = 136.5, "fbg_baseline_mmol"), # https://www.cleveland19.com/story/1425584/ohio-man-holds-world-record-of-highest-blood-sugar/
            fbg_updated_mmol = cut_numeric_value(fbg_baseline_mmol, min = 0, max = 136.5, "fbg_updated_mmol"), # https://www.cleveland19.com/story/1425584/ohio-man-holds-world-record-of-highest-blood-sugar/
            blood_pressure_sys_mmhg = cut_numeric_value(blood_pressure_sys_mmhg, min = 20, max = 250, "blood_pressure_sys_mmhg"),
            blood_pressure_dias_mmhg = cut_numeric_value(blood_pressure_dias_mmhg, min = 20, max = 220, "blood_pressure_dias_mmhg"),
            support_from_a4d = check_allowed_values(support_from_a4d, c("Standard", "Partial", "Semi-Partial", "SAC", "Monitoring"), id, "support_from_a4d"),
            status = check_allowed_values(status, c("Active", "Active - Remote", "Query", "Inactive", "Lost Follow Up", "Deceased", "Discontinued"), id, "status"),
            insulin_regimen = check_allowed_values(
                insulin_regimen, c(
                    "Basal-bolus",
                    "Basal-bolus MDI (AN)",
                    "Basal-bolus MDI (HI)",
                    "Premixed 30/70 BD",
                    "Self-mixed BD",
                    "Modified conventional TID"
                ),
                id,
                "insulin_regimen"
            ),
            t1d_diagnosis_with_dka = check_allowed_values(t1d_diagnosis_with_dka, c("N", "Y"), id, "t1d_diagnosis_with_dka")
        ) %>%
        ungroup()

    logDebug(
        "df_patient dim: ",
        dim(df_patient) %>% as.data.frame(),
        "."
    )

    export_data(
        data = df_patient,
        filename = str_replace(patient_file_name, "_patient_raw", ""),
        output_root = output_root,
        suffix = "_patient_cleaned"
    )

    logInfo("Finish process_patient_file.")
}


process_product_file <- function(paths, product_file, product_file_name, synonyms_product, output_root) {
    product_file_path <-
        file.path(paths$tracker_root, product_file)
    logDebug("Start process_product_file.")
    logInfo(
        "Current file: ",
        product_file_name
    )

    df_product_raw <- read_raw_csv(product_file_path)

    df_product_raw <- reading_product_data_step2(df_product_raw, synonyms_product)

    logDebug(
        "df_product_raw dim: ",
        dim(df_product_raw) %>% as.data.frame(),
        "."
    )

    export_data(
        data = df_product_raw,
        filename = str_replace(product_file_name, "_product_raw", ""),
        output_root = output_root,
        suffix = "_product_cleaned"
    )

    logInfo("Finish process_product_file.")
}


# Calculate the number of cores
no_cores <- future::availableCores() - 1
doFuture::registerDoFuture()

future::plan(future::multisession, workers = no_cores)
# future::plan(future::sequential)

main()

clearLoggers()
