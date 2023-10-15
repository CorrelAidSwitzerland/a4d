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

    with_file_logger(logfile,
        {
            tryCatch(
                {
                    create_table_patient_data_static(patient_data_files, file.path(paths$output_root, "patient_data_cleaned"), paths$tables)
                },
                error = function(e) {
                    logError("Could not create table csv for static patient data. Error: ", e$message)
                },
                warning = function(w) {
                    logWarn("Could not create table csv for static patient data. Error: ", w$message)
                }
            )
        },
        output_root = paths$output_root
    )

    logfile <- "table_patient_data"

    with_file_logger(logfile,
        {
            tryCatch(
                {
                    create_table_patient_data(patient_data_files, file.path(paths$output_root, "patient_data_cleaned"), paths$tables)
                },
                error = function(e) {
                    logError("Could not create table csv for dynamic patient data. Error: ", e$message)
                },
                warning = function(w) {
                    logWarn("Could not create table csv for dynamic patient data. Error: ", w$message)
                }
            )
        },
        output_root = paths$output_root
    )

    logfile <- "table_product_data"

    with_file_logger(logfile,
        {
            tryCatch(
                {
                    create_table_product_data(file.path(paths$output_root, "product_data_cleaned"), paths$tables)
                },
                error = function(e) {
                    logError("Could not create table csv for product data. Error: ", e$message)
                },
                warning = function(w) {
                    logWarn("Could not create table csv for product patient data. Error: ", w$message)
                }
            )
        },
        output_root = paths$output_root
    )

    logInfo("Finish creating table csv files.")
}

main()
