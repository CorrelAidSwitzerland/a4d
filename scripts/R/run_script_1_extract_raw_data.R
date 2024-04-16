options(readxl.show_progress = FALSE)

main <- function() {
    paths <- init_paths(c("patient_data_raw", "product_data_raw"), delete = TRUE)
    setup_logger(paths$output_root, "script1")
    tracker_files <- get_files(paths$tracker_root)
    logInfo(
        "Found ",
        length(tracker_files),
        " xlsx files under ",
        paths$tracker_root,
        "."
    )

    synonyms <- get_synonyms()

    for (i in seq_along(tracker_files)) {
        tracker_file <- tracker_files[i]
        tracker_name <- tools::file_path_sans_ext(basename(tracker_file))
        tictoc::tic(paste("Processing tracker file:", tracker_name))
        tryCatch(
            process_tracker_file(paths, tracker_file, tracker_name, synonyms),
            error = function(e) {
                logError(log_to_json("Could not process {values['tracker_name']}. Error = {values['e']}.", values = list(tracker_name = tracker_name, e = e$message), file = "scripts/run_script_1_extract_raw_data.R", line = 24, errorCode = "XDTWDGA"))
            },
            warning = function(w) {
                logWarn("Could not process ", tracker_name, ". Warning = ", w$message, ".")
            }
        )
        tictoc::toc()
        cat(paste("Processed ", i, " of ", length(tracker_files), " (", round(i / length(tracker_files) * 100, 0), "%) tracker files.\n"))
    }
}


process_tracker_file <- function(paths, tracker_file, tracker_name, synonyms) {
    tracker_data_file <-
        file.path(paths$tracker_root, tracker_file)
    logDebug(log_to_json("Start process_tracker_file."))
    logInfo(log_to_json(
        "Current file: {values['tracker_name]}",
        values = list(tracker_name = tracker_name), file = "scripts/run_script_1_extract_raw_data.R", line = 42, errorCode = "TEST"
    ))

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
                    logError(log_to_json("Could not process patient data. Error = {values['e']}.", values = list(e = e$message), file = "scripts/run_script_1_extract_raw_data.R", line = 57, errorCode = "ABCPilot"))
                },
                warning = function(w) {
                    logWarn("Could not process patient data. Warning = ", w$message, ".")
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
                    logError("Could not process product data. Error = ", e$message, ".")
                },
                warning = function(w) {
                    logWarn("Could not process product data. Warning = ", w$message, ".")
                }
            )
        },
        output_root = paths$output_root
    )
}


process_patient_data <-
    function(tracker_name,
             tracker_data_file,
             output_root,
             synonyms_patient) {
        df_raw_patient <-
            reading_patient_data(
                tracker_data_file = tracker_data_file,
                columns_synonyms = synonyms_patient
            )

        df_raw_patient <- df_raw_patient %>% dplyr::mutate(file_name = tracker_name)

        logDebug(
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
    }


process_product_data <-
    function(tracker_name,
             tracker_data_file,
             output_root,
             synonyms_product) {
        df_raw_product <-
            reading_product_data_step1(
                tracker_data_file = tracker_data_file,
                columns_synonyms = synonyms_product
            )

        if (!is.null(df_raw_product)) {
            df_raw_product <- df_raw_product %>% dplyr::mutate(file_name = tracker_name)
        } else {
            logDebug("Empty product data")
        }

        logDebug(
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
            logWarn("No product data in the file")
        }
    }

# profvis(main())
main()

clearLoggers()
