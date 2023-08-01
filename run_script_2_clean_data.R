options(readxl.show_progress = FALSE)
options(future.rng.onMisuse = "ignore")

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
    patient_data_files <- get_files(paths$tracker_root, pattern = "patient_data.csv$")
    product_data_files <- get_files(paths$tracker_root, pattern = "product_data.csv$")
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
        tryCatch(
            process_patient_file(paths, patient_file, patient_file_name),
            error = function(e) {
                logError("Could not process ", patient_file_name, ". Error = ", e, ".")
            },
            warning = function(w) {
                logWarn("Could not process ", patient_file_name, ". Warning = ", w, ".")
            }
        )
    }

    logInfo("Finish processing all patient csv files.")

    logDebug("Start processing product csv files.")
    synonyms <- get_synonyms()
    synonyms_product <- synonyms$product

    foreach::foreach(product_file = product_data_files) %dopar% {
        product_file_name <- tools::file_path_sans_ext(basename(product_file))
        tryCatch(
            process_product_file(paths, product_file, product_file_name, synonyms_product),
            error = function(e) {
                logError("Could not process ", product_file_name, ". Error = ", e, ".")
            },
            warning = function(w) {
                logWarn("Could not process ", product_file_name, ". Warning = ", w, ".")
            }
        )
    }

    logInfo("Finish processing all csv files.")
}


process_patient_file <- function(paths, patient_file, patient_file_name) {
    patient_file_path <-
        file.path(paths$tracker_root, patient_file)
    logDebug("Start process_patient_file.")
    logInfo(
        "Current file: ",
        patient_file_name
    )

    logfile <- paste0(patient_file_name)
    setup_file_logger(paths$output_root, logfile)

    df_patient_raw <- read_raw_csv(patient_file_path)

    # --- TRANSFORMATIONS ---
    # data before 2019 had only one column for updated hba1c and fbg
    # with date as part of the value
    if (!"updated_hba1c_date" %in% colnames(df_patient_raw)) {
        logInfo("Column updated_hba1c_date not found. Trying to parse from updated_hba1c.")
        df_patient_raw <-
            extract_date_from_measurement(df_patient_raw, "updated_hba1c")
        df_patient_raw <-
            parse_invalid_dates(df_patient_raw, "updated_hba1c_date")
        logInfo("Finished parsing dates from updated_hba1c.")
    }

    if (!"updated_fbg_date" %in% colnames(df_patient_raw)) {
        logInfo("Column updated_fbg_date not found. Trying to parse from updated_hba1c.")
        df_patient_raw <-
            extract_date_from_measurement(df_patient_raw, "updated_fbg")
        df_patient_raw <-
            parse_invalid_dates(df_patient_raw, "updated_fbg_date")
        logInfo("Finished parsing dates from updated_fbg.")
    }

    df_patient_raw <- bmi_fix(df_patient_raw)
    if ("blood_pressure_mmhg" %in% colnames(df_patient_raw)) {
        df_patient_raw <- split_bp_in_sys_and_dias(df_patient_raw)
    }

    # --- META SCHEMA ---
    schema <- tibble::tibble(
        # clinic_visit = logical(),
        # complication_screening = character(),
        # complication_screening_date = date(),
        # complication_screening_results = character(),
        # dm_complication_comment = character(), # TODO
        # dm_complication_eye = character(), # TODO
        # dm_complication_kidney = character(), # TODO
        # dm_complication_other = character(), # TODO
        # est_strips_pmonth = integer(),
        # family_support_scale = character(), # TODO
        # inactive_reason = character(),
        # insulin_dosage = character(),
        # meter_received_date = date(), # TODO
        # remarks = character(),
        additional_support = character(),
        age = integer(),
        blood_pressure_dias_mmhg = integer(),
        blood_pressure_sys_mmhg = integer(),
        bmi = numeric(),
        bmi_date = date(),
        clinic_code = character(),
        country_code = character(),
        dob = date(),
        edu_occ = character(),
        fbg_baseline_mg = numeric(),
        fbg_baseline_mmol = numeric(),
        fbg_updated = numeric(),
        fbg_updated_date = date(),
        fbg_updated_mg = numeric(),
        fbg_updated_mmol = numeric(),
        file_name = character(),
        gender = character(),
        hb1ac_baseline = numeric(),
        hb1ac_updated = numeric(),
        hba1c_updated_date = date(),
        height = numeric(),
        hospitalisation_cause = character(),
        hospitalisation_date = date(),
        id = character(),
        insulin_regimen = character(),
        last_clinic_visit_date = date(),
        last_remote_followup_date = date(),
        lost_date = date(),
        name = character(),
        observations = character(),
        observations_category = character(),
        province = character(),
        recruitment_date = date(),
        remote_followup = date(),
        sheet_name = character(),
        status = character(),
        status_out = character(),
        support_from_a4d = character(),
        t1d_diagnosis_age = integer(),
        t1d_diagnosis_date = date(),
        t1d_diagnosis_with_dka = logical(),
        testing_fqr_pday = integer(),
        tracker_month = character(),
        tracker_year = integer(),
        updated_2022_date = date(),
        weight = numeric()
    )

    cols_extra <- colnames(df_patient_raw)[!colnames(df_patient_raw) %in% colnames(schema)]
    logInfo("Extra columns in patient data: ", cols_extra)

    cols_missing <-
        colnames(schema)[!colnames(schema) %in% colnames(df_patient_raw)]
    logInfo("Missing columns in patient data: ", cols_missing)

    # add all columns of schema to df_patient_raw
    # # keep all rows, only append missing cols
    df_patient <- merge.default(df_patient_raw, schema, all.x = T)
    df_patient <- df_patient[colnames(schema)]

    unregisterLogger(logfile)

    logInfo("Finish process_patient_file.")
}


process_product_file <- function(paths, product_file, product_file_name, synonyms_product) {
    product_file_path <-
        file.path(paths$tracker_root, product_file)
    logDebug("Start process_product_file.")
    logInfo(
        "Current file: ",
        product_file_name
    )

    logfile <- paste0(product_file_name)
    setup_file_logger(paths$output_root, logfile)

    df_product_raw <- read_raw_csv(product_file_path)

    df_product_raw <- reading_product_data_step2(df_product_raw, synonyms_product)

    unregisterLogger(logfile)

    logInfo("Finish process_product_file.")
}


# Calculate the number of cores
no_cores <- future::availableCores() - 1
doFuture::registerDoFuture()

future::plan(future::multisession, workers = no_cores)
# future::plan(future::sequential)

main()

clearLoggers()
