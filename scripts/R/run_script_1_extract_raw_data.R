options(readxl.show_progress = FALSE)

main <- function() {
    paths <- init_paths(c("patient_data_raw", "product_data_raw"), delete = TRUE)
    setup_logger(paths$output_root, "script1")
    tracker_files <- get_files(paths$tracker_root)
    logInfo(
        log_to_json(
            "Found {values['len']} xlsx files under {values['root']}.",
            values = list(len = length(tracker_files), root = paths$tracker_root),
            script = "script1",
            file = "run_script_1_extract_raw_data.R",
            functionName = "main"
        )
    )

    synonyms <- get_synonyms()

    for (i in seq_along(tracker_files)) {
        tracker_file <- tracker_files[i]
        tracker_name <- tools::file_path_sans_ext(basename(tracker_file))
        tictoc::tic(paste("Processing tracker file:", tracker_name))
        process_tracker_file(paths, tracker_file, tracker_name, synonyms)
        tictoc::toc()
        cat(paste("Processed ", i, " of ", length(tracker_files), " (", round(i / length(tracker_files) * 100, 0), "%) tracker files.\n"))
    }
}


process_tracker_file <- function(paths, tracker_file, tracker_name, synonyms) {
    tracker_data_file <-
        file.path(paths$tracker_root, tracker_file)

    logInfo(
        log_to_json(
            "Current file: {values['file']}.",
            values = list(file = tracker_name),
            script = "script1",
            file = "run_script_1_extract_raw_data.R",
            functionName = "process_tracker_file"
        )
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
                    logError(
                        log_to_json(
                            "Could not process patient data. Error = {values['e']}.",
                            values = list(e = e$message),
                            script = "script1",
                            file = "run_script_1_extract_raw_data.R",
                            errorCode = "critical_abort",
                            functionName = "process_patient_data"
                        )
                    )
                },
                warning = function(w) {
                    logWarn(
                        log_to_json(
                            "Could not process patient data. Warning = {values['w']}.",
                            values = list(w = w$message),
                            script = "script1",
                            file = "run_script_1_extract_raw_data.R",
                            warningCode = "critical_abort",
                            functionName = "process_patient_data"
                        )
                    )
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
                    logError(
                        log_to_json(
                            "Could not process product data. Error = {values['e']}.",
                            values = list(e = e$message),
                            script = "script1",
                            file = "run_script_1_extract_raw_data.R",
                            errorCode = "critical_abort",
                            functionName = "process_product_data"
                        )
                    )
                },
                warning = function(w) {
                    logWarn(
                        log_to_json(
                            "Could not process product data. Warning = {values['w']}.",
                            values = list(w = w$message),
                            script = "script1",
                            file = "run_script_1_extract_raw_data.R",
                            warningCode = "critical_abort",
                            functionName = "process_product_data"
                        )
                    )
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

        # instead of clinic_code and country_code, we extract clinic_id from parent folder
        # and join with static clinic data later in the database
        df_raw_patient$clinic_id <- basename(dirname(tracker_data_file))

        logInfo(
            log_to_json(
                message = "df_raw_patient dim: {values['dim']}.",
                values = list(dim = dim(df_raw_patient)),
                script = "script1",
                file = "run_script_1_extract_raw_data.R",
                functionName = "process_patient_data"
            )
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

            df_raw_product$clinic_id <- basename(dirname(tracker_data_file))

            logInfo(
                log_to_json(
                    message = "df_raw_product dim: {values['dim']}.",
                    values = list(dim = dim(df_raw_product)),
                    script = "script1",
                    file = "run_script_1_extract_raw_data.R",
                    functionName = "process_product_data"
                )
            )

            # product set sensitive column to NA and add tracker file name as a column
            export_data_as_parquet(
                data = df_raw_product,
                filename = tracker_name,
                output_root = output_root,
                suffix = "_product_raw"
            )
        } else {
            logWarn(
                log_to_json(
                    message = "Empty product data!",
                    script = "script1",
                    file = "run_script_1_extract_raw_data.R",
                    functionName = "process_product_data",
                    warningCode = "empty_product_data"
                )
            )
        }
    }

# profvis(main())
main()

clearLoggers()
