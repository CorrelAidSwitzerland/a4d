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

    logInfo("Start processing tracker files.")

    for (i in seq_along(tracker_files)) {
        tracker_file <- tracker_files[i]
        tracker_name <- tools::file_path_sans_ext(basename(tracker_file))
        tryCatch(
            process_tracker_file(paths, tracker_file, tracker_name, synonyms),
            error = function(e) {
                logError("Could not process ", tracker_name, ". Error = ", e$message, ".")
            },
            warning = function(w) {
                logWarn("Could not process ", tracker_name, ". Warning = ", w$message, ".")
            }
        )
        cat(paste("Processed ", i, " of ", length(tracker_files), " (", round(i / length(tracker_files)*100, 0), "%) tracker files.\n"))
    }
    logInfo("Finish processing all tracker files.")
}


process_tracker_file <- function(paths, tracker_file, tracker_name, synonyms) {
    tracker_data_file <-
        file.path(paths$tracker_root, tracker_file)
    logDebug("Start process_tracker_file.")
    logInfo(
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
                    logError("Could not process patient data. Error = ", e$message, ".")
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

    logInfo("Finish process_tracker_file.")
}


process_patient_data <-
    function(tracker_name,
             tracker_data_file,
             output_root,
             synonyms_patient) {
        logDebug("Start process_patient_data.")

        df_raw_patient <-
            reading_patient_data(
                tracker_data_file = tracker_data_file,
                columns_synonyms = synonyms_patient
            )

        df_raw_patient <- df_raw_patient %>% mutate(file_name = tracker_name)

        logDebug(
            "df_raw_patient dim: ",
            dim(df_raw_patient) %>% as.data.frame(),
            "."
        )

        export_data(
            data = df_raw_patient,
            filename = tracker_name,
            output_root = output_root,
            suffix = "_patient_raw"
        )

        logInfo("Finish process_patient_data.")
    }


process_product_data <-
    function(tracker_name,
             tracker_data_file,
             output_root,
             synonyms_product) {
        logDebug("Start process_product_data.")

        df_raw_product <-
            reading_product_data_step1(
                tracker_data_file = tracker_data_file,
                columns_synonyms = synonyms_product
            )

        if (!is.null(df_raw_product)) {
            df_raw_product <- df_raw_product %>% mutate(file_name = tracker_name)
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
            export_data(
                data = df_raw_product,
                filename = tracker_name,
                output_root = output_root,
                suffix = "_product_raw"
            )
        } else {
            logWarn("No product data in the file")
        }
        logDebug("Finish process_product_data.")
    }

# profvis(main())
main()

clearLoggers()
