options(readxl.show_progress = FALSE)
options(future.rng.onMisuse = "ignore")

`%dopar%` <- foreach::`%dopar%`

source("R/read_patient_data.R")
source("R/helper_read_patient_data.R")
source("R/read_product_data.R")
source("R/helper_clean_data.R")
source("R/helper_product_data.R")
source("R/get_tracker_year.R")
source("R/logger.R")

main <- function() {
    paths <- init_paths()
    setup_logger(paths$output_root)
    tracker_files <- get_tracker_files(paths$tracker_root)
    logInfo(
        "Found ",
        length(tracker_files),
        " xlsx files under ",
        paths$tracker_root,
        "."
    )

    synonyms <- get_synonyms()

    logDebug("Start processing tracker files.")

    foreach::foreach(tracker_file = tracker_files) %dopar% {
        tryCatch(
            process_tracker_file(paths, tracker_file, synonyms),
            error = function(e) {
                logError("Could not process ", tracker_file, ". Error = ", e, ".")
            },
            warning = function(w) {
                logWarn(
                    "Could not process ",
                    tracker_file,
                    ". Warning = ",
                    w,
                    "."
                )
            }
        )
    }
    logInfo("Finish processing all tracker files.")
}


init_paths <- function() {
    tracker_root_path <- select_A4D_directory(T)
    output_root <- file.path(
        tracker_root_path,
        "output",
        "sensitive_data_removed"
    )

    if (!file.exists(output_root)) {
        dir.create(output_root, recursive = TRUE)
    }

    list(tracker_root = tracker_root_path, output_root = output_root)
}


get_synonyms <- function() {
    ## Extract synonyms for products and patients
    ## If you encounter new columns, just add the synonyms to these YAML files
    synonyms_patient <-
        read_column_synonyms(synonym_file = "synonyms_patient.yaml")
    synonyms_product <-
        read_column_synonyms(synonym_file = "synonyms_product.yaml")

    list(patient = synonyms_patient, product = synonyms_product)
}


get_tracker_files <- function(tracker_root) {

    tracker_files <- list.files(path = tracker_root, recursive = T, pattern = "\\.xlsx$")

    # only choose files in folders containing the following names
    regex_tracker_country <- "01_THAILAND|02_MYANMAR|03_LAOS|04_VIETNAM|05_CAMBODIA|06_MALAYSIA"
    tracker_files <- tracker_files[grepl(x = tracker_files, pattern = regex_tracker_country)]

    # only choose files within folders called "ARCHIVE"
    tracker_files <- tracker_files[grepl(x = tracker_files, pattern = "ARCHIVE")]

    tracker_files <-
        tracker_files[str_detect(tracker_files, "~", negate = T)]
}


process_tracker_file <- function(paths, tracker_file, synonyms) {
    addDefaultFileLogger(file.path(paths$output_root, "logs",
                                   paste0(tools::file_path_sans_ext(basename(tracker_file)),
                                          ".log")))
    logDebug("Start process_tracker_file.")
    logInfo("Current file: ", tracker_file, ".")
    tracker_data_file <-
        file.path(paths$tracker_root, tracker_file)

    process_patient_data(
        tracker_file = tracker_file,
        tracker_data_file = tracker_data_file,
        output_root = paths$output_root,
        synonyms_patient = synonyms$patient
    )

    process_product_data(
        tracker_file = tracker_file,
        tracker_data_file = tracker_data_file,
        output_root = paths$output_root,
        synonyms_product = synonyms$product
    )

    logInfo("Finish process_tracker_file.")
}


process_patient_data <-
    function(tracker_file,
             tracker_data_file,
             output_root,
             synonyms_patient) {
        logDebug("Start process_patient_data.")

        df_raw_patient <-
            reading_patient_data_2(
                tracker_data_file = tracker_data_file,
                columns_synonyms = synonyms_patient
            )
        logDebug(
            "df_raw_patient dim: ",
            dim(df_raw_patient) %>% as.data.frame(),
            "."
        )

        # INCOMPLETE - Set sensitive rows to NA -------------------------------------
        # level of education is in the patient list - we need to get data from there as well
        df_raw_patient <-
            remove_sensitive_data(
                data = df_raw_patient,
                tracker_file = tracker_file,
                cols = c(
                    "patient_id",
                    "patient_name",
                    "province",
                    "dob",
                    "country_code",
                    "clinic_code",
                    "gender",
                    "edu_occ"
                )
            )

        export_data(
            data = df_raw_patient,
            tracker_file = tracker_file,
            output_root = output_root,
            suffix = "_patient_data"
        )

        logInfo("Finish process_patient_data.")
    }


process_product_data <-
    function(tracker_file,
             tracker_data_file,
             output_root,
             synonyms_product) {
        logDebug("Start process_product_data.")

        df_raw_product <-
            reading_product_data_step1(
                tracker_data_file = tracker_data_file,
                columns_synonyms = synonyms_product
            )
        logDebug(
            "df_raw_product dim: ",
            dim(df_raw_product) %>% as.data.frame(),
            "."
        )

        # product set sensitive column to NA and add tracker file name as a column
        if (!is.null(df_raw_product)) {
            df_raw_product <-
                remove_sensitive_data(
                    data = df_raw_product,
                    tracker_file = tracker_file,
                    cols = c("product_released_to")
                )

            export_data(
                data = df_raw_product,
                tracker_file = tracker_file,
                output_root = output_root,
                suffix = "_product_data"
            )
        } else {
            logInfo("No product data in the file - ", tracker_file)
        }
        logDebug("Finish process_product_data.")
    }


remove_sensitive_data <- function(data, tracker_file, cols) {
    data <-
        data %>%
        dplyr::mutate(across(
            tidyr::any_of(cols),
            ~NA
        )) %>%
        dplyr::mutate(file_name = fs::path_ext_remove(tracker_file))
}


export_data <- function(data, tracker_file, output_root, suffix) {
    logDebug("Start export_data. Suffix = ", suffix, ".")
    data %>%
        write.csv(
            file =
                file.path(
                    output_root,
                    paste0(
                        tools::file_path_sans_ext(basename(tracker_file)),
                        suffix,
                        ".csv"
                    )
                ),
            row.names = F
        )
    logInfo("Finish export_data. Suffix = ", suffix, ".")
}

# Calculate the number of cores
no_cores <- future::availableCores() - 1
doFuture::registerDoFuture()

future::plan(future::multisession, workers = no_cores)
# future::plan(future::sequential)

main()

clearLoggers()
