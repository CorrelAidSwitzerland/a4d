options(readxl.show_progress = FALSE)

main <- function() {
    paths <- init_paths(c("tables"), delete = TRUE)
    setup_logger(paths$output_root, "script3")
    patient_data_files <- get_files(file.path(paths$output_root, "patient_data_cleaned"), pattern = "\\.csv$")
    product_data_files <- get_files(file.path(paths$output_root, "product_data_cleaned"), pattern = "\\.csv$")
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

    logInfo("Start creating table csv files.")

    logfile <- "table_patient_data_static"
    setup_file_logger(paths$output_root, logfile)
    tryCatch(
        {
            create_table_patient_data_static(patient_data_files, file.path(paths$output_root, "patient_data_cleaned"), paths$tables)
        },
        error = function(e) {
            logError("Could not create table csv for static patient data. Error: ", e$message)
        },
        warning = function(w) {
            logWarn("Could not create table csv for static patient data. Error: ", w$message)
        },
        finally = unregisterLogger(logfile)
    )

    logInfo("Finish creating table csv files.")
}

main()
