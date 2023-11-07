options(readxl.show_progress = FALSE)

ERROR_VAL_NUMERIC <<- 999999
ERROR_VAL_CHARACTER <<- "Undefined"
ERROR_VAL_DATE <<- "9999-09-09"

main <- function() {
    paths <- init_paths(c("tables"), delete = TRUE)
    setup_logger(paths$output_root, "script3")
    patient_data_files <- get_files(file.path(paths$output_root, "patient_data_cleaned"), pattern = "\\.parquet$")
    product_data_files <- get_files(file.path(paths$output_root, "product_data_cleaned"), pattern = "\\.parquet$")
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

    logfile <- "table_patient_data_monthly"
    with_file_logger(logfile,
        {
            tryCatch(
                {
                    create_table_patient_data_monthly(patient_data_files, file.path(paths$output_root, "patient_data_cleaned"), paths$tables)
                },
                error = function(e) {
                    logError("Could not create table csv for monthly patient data. Error: ", e$message)
                },
                warning = function(w) {
                    logWarn("Could not create table csv for monthly patient data. Error: ", w$message)
                }
            )
        },
        output_root = paths$output_root
    )

    logfile <- "table_longitudinal_data_hba1c"
    with_file_logger(logfile,
                     {
                         tryCatch(
                             {
                                 create_table_longitudinal_data(
                                     patient_data_files,
                                     file.path(paths$output_root, "patient_data_cleaned"),
                                     paths$tables,
                                     "hba1c_updated",
                                     "hba1c"
                                 )
                             },
                             error = function(e) {
                                 logError("Could not create table csv for longitudinal patient data. Error: ", e$message)
                             },
                             warning = function(w) {
                                 logWarn("Could not create table csv for longitudinal patient data. Error: ", w$message)
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
                    logWarn("Could not create table csv for product data. Warning: ", w$message)
                }
            )
        },
        output_root = paths$output_root
    )

    logInfo("Finish creating table csv files.")

    logInfo("Trying to link csv files for product and patient data.")

    logfile <- "link_product_patient_data"

    with_file_logger(logfile,
        {
            tryCatch(
                {
                    link_product_patient(
                        file.path(paths$tables, "product_data.csv"),
                        file.path(paths$tables, "patient_data_monthly.csv")
                    )
                },
                error = function(e) {
                    logError("Could not link csv files for product and patient data. Error: ", e$message)
                },
                warning = function(w) {
                    logWarn("Could not link csv files for product and patient data. Warning: ", w$message)
                }
            )
        },
        output_root = paths$output_root
    )

    logInfo("Finished linking csv files for product and patient data.")
}

main()
