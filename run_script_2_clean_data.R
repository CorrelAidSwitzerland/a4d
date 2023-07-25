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
    synonyms_product = synonyms$product

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
    # df_patient_raw <- date_fix(df_patient_raw)
    if ("blood_pressure_mmhg" %in% colnames(df_patient_raw)) {
        df_patient_raw <- split_bp_in_sys_and_dias(df_patient_raw)
    }

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
