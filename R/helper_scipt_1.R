#' @title Process a single tracker file and extract patient and product data.
#'
#' @param tracker_file Filename of the tracler.
#' @param paths a list with the paths to the tracker root dir, the patient and product output dir and the root output dir.
#' @param p progressor from progressr package.
#'
#' @export
process_tracker_file <- function(tracker_file, paths, p) {
    p()
    tracker_name <- tools::file_path_sans_ext(basename(tracker_file))
    synonyms <- get_synonyms()
    tracker_data_file <-
        file.path(paths$tracker_root, tracker_file)

    ParallelLogger::logDebug("Start process_tracker_file.")
    ParallelLogger::logInfo(
        "Current file: ",
        tracker_name
    )

    logfile <- paste0(tracker_name, "_", "patient")
    with_file_logger(logfile,
        {
            tryCatch(
                process_patient_data(
                    tracker_name = tracker_name,
                    tracker_data_file = tracker_data_file,
                    output_root = paths$patient_data_raw,
                    synonyms_patient = synonyms$patient
                ),
                error = function(e) {
                    ParallelLogger::logError("Could not process patient data. Error = ", e$message, ".")
                },
                warning = function(w) {
                    ParallelLogger::logWarn("Could not process patient data. Warning = ", w$message, ".")
                }
            )
        },
        output_root = paths$output_root
    )

    logfile <- paste0(tracker_name, "_", "product")

    with_file_logger(logfile,
        {
            tryCatch(
                process_product_data(
                    tracker_name = tracker_name,
                    tracker_data_file = tracker_data_file,
                    output_root = paths$product_data_raw,
                    synonyms_product = synonyms$product
                ),
                error = function(e) {
                    ParallelLogger::logError("Could not process product data. Error = ", e$message, ".")
                },
                warning = function(w) {
                    ParallelLogger::logWarn("Could not process product data. Warning = ", w$message, ".")
                }
            )
        },
        output_root = paths$output_root
    )

    ParallelLogger::logDebug("Finish process_tracker_file.")
}


#' @title Extract patient data.
#'
#' @param tracker_name Filename without extension.
#' @param tracker_data_file Filename of the tracker.
#' @param output_root Directory for storing extracted patient data.
#' @param synonyms_patient Synonyms for patient data header names.
#'
#' @export
process_patient_data <-
    function(tracker_name,
             tracker_data_file,
             output_root,
             synonyms_patient) {
        ParallelLogger::logDebug("Start process_patient_data.")

        df_raw_patient <-
            reading_patient_data(
                tracker_data_file = tracker_data_file,
                columns_synonyms = synonyms_patient
            )

        df_raw_patient <- df_raw_patient %>% dplyr::mutate(file_name = tracker_name)

        ParallelLogger::logDebug(
            "df_raw_patient dim: ",
            dim(df_raw_patient) %>% as.data.frame(),
            "."
        )

        export_data_as_parquet(
            data = df_raw_patient,
            filename = tracker_name,
            output_root = output_root,
            suffix = "_patient_raw"
        )

        ParallelLogger::logDebug("Finish process_patient_data.")
    }


#' @title Extract product data.
#'
#' @param tracker_name Filename without extension.
#' @param tracker_data_file Filename of the tracker.
#' @param output_root Directory for storing extracted product data.
#' @param synonyms_product Synonyms for product data header names.
#'
#' @export
process_product_data <-
    function(tracker_name,
             tracker_data_file,
             output_root,
             synonyms_product) {
        ParallelLogger::logDebug("Start process_product_data.")

        df_raw_product <-
            reading_product_data_step1(
                tracker_data_file = tracker_data_file,
                columns_synonyms = synonyms_product
            )

        if (!is.null(df_raw_product)) {
            df_raw_product <- df_raw_product %>% dplyr::mutate(file_name = tracker_name)
        } else {
            ParallelLogger::logDebug("Empty product data")
        }

        ParallelLogger::logDebug(
            "df_raw_product dim: ",
            dim(df_raw_product) %>% as.data.frame(),
            "."
        )

        # product set sensitive column to NA and add tracker file name as a column
        if (!is.null(df_raw_product)) {
            export_data_as_parquet(
                data = df_raw_product,
                filename = tracker_name,
                output_root = output_root,
                suffix = "_product_raw"
            )
        } else {
            ParallelLogger::logWarn("No product data in the file")
        }
        ParallelLogger::logDebug("Finish process_product_data.")
    }
