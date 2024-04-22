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
        log_to_json(
            "Found {values['len']} patient csv files under {values['root']}. ",
            values = list(len = length(patient_data_files), root = paths$tracker_root),
            script = "script3",
            file = "run_script3_create_tables.R",
            functionName = "main"
        )
    )
    logInfo(
        log_to_json(
            "Found {values['len']} product csv files under {values['root']}. ",
            values = list(len = length(product_data_files), root = paths$tracker_root),
            script = "script3",
            file = "run_script3_create_tables.R",
            functionName = "main"
        )
    )

    logfile <- "table_patient_data_static"
    with_file_logger(logfile,
        {
            tryCatch(
                {
                    create_table_patient_data_static(patient_data_files, file.path(paths$output_root, "patient_data_cleaned"), paths$tables)
                },
                error = function(e) {
                    logError(
                        log_to_json(
                            "Could not create table for static patient data. Error = {values['e']}.",
                            values = list(e = e$message),
                            script = "script3",
                            file = "run_script_3_create_tables.R",
                            errorCode = "critical_abort",
                            functionName = "create_table_patient_data_static"
                        )
                    )
                },
                warning = function(w) {
                    logWarn(
                        log_to_json(
                            "Could not create table for static patient data. Warning = {values['w']}.",
                            values = list(w = w$message),
                            script = "script3",
                            file = "run_script_3_create_tables.R",
                            warningCode = "critical_abort",
                            functionName = "create_table_patient_data_static"
                        )
                    )
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
                    logError(
                        log_to_json(
                            "Could not create table for monthly patient data. Error = {values['e']}.",
                            values = list(e = e$message),
                            script = "script3",
                            file = "run_script_3_create_tables.R",
                            errorCode = "critical_abort",
                            functionName = "create_table_patient_data_monthly"
                        )
                    )
                },
                warning = function(w) {
                    logWarn(
                        log_to_json(
                            "Could not create table for monthly patient data. Warning = {values['w']}.",
                            values = list(w = w$message),
                            script = "script3",
                            file = "run_script_3_create_tables.R",
                            warningCode = "critical_abort",
                            functionName = "create_table_patient_data_monthly"
                        )
                    )
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
                    logError(
                        log_to_json(
                            "Could not create table for longitudinal patient data. Error = {values['e']}.",
                            values = list(e = e$message),
                            script = "script3",
                            file = "run_script_3_create_tables.R",
                            errorCode = "critical_abort",
                            functionName = "create_table_longitudinal_data"
                        )
                    )
                },
                warning = function(w) {
                    logWarn(
                        log_to_json(
                            "Could not create table for longitudinal patient data. Warning = {values['w']}.",
                            values = list(w = w$message),
                            script = "script3",
                            file = "run_script_3_create_tables.R",
                            warningCode = "critical_abort",
                            functionName = "create_table_longitudinal_data"
                        )
                    )
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
                    logError(
                        log_to_json(
                            "Could not create table for product data. Error = {values['e']}.",
                            values = list(e = e$message),
                            script = "script3",
                            file = "run_script_3_create_tables.R",
                            errorCode = "critical_abort",
                            functionName = "create_table_product_data"
                        )
                    )
                },
                warning = function(w) {
                    logWarn(
                        log_to_json(
                            "Could not create table for product data. Warning = {values['w']}.",
                            values = list(w = w$message),
                            script = "script3",
                            file = "run_script_3_create_tables.R",
                            warningCode = "critical_abort",
                            functionName = "create_table_product_data"
                        )
                    )
                }
            )
        },
        output_root = paths$output_root
    )

    logfile <- "clinic_data_static"
    with_file_logger(logfile,
        {
            tryCatch(
                {
                    export_data_as_parquet(data = read.csv("reference_data/clinic_data_static.csv"), filename = "clinic_data_static", output_root = paths$tables, suffix = "")
                },
                error = function(e) {
                    logError(
                        log_to_json(
                            "Could not create table for clinic static data. Error = {values['e']}.",
                            values = list(e = e$message),
                            script = "script3",
                            file = "run_script_3_create_tables.R",
                            errorCode = "critical_abort",
                            functionName = "export_data_as_parquet"
                        )
                    )
                },
                warning = function(w) {
                    logWarn(
                        log_to_json(
                            "Could not create table for clinic static data. Warning = {values['w']}.",
                            values = list(w = w$message),
                            script = "script3",
                            file = "run_script_3_create_tables.R",
                            warningCode = "critical_abort",
                            functionName = "export_data_as_parquet"
                        )
                    )
                }
            )
        },
        output_root = paths$output_root
    )

    logfile <- "link_product_patient_data"

    with_file_logger(logfile,
        {
            tryCatch(
                {
                    link_product_patient(
                        file.path(paths$tables, "product_data.parquet"),
                        file.path(paths$tables, "patient_data_monthly.parquet")
                    )
                },
                error = function(e) {
                    logError(
                        log_to_json(
                            "Could not link files for product and patient data. Error = {values['e']}.",
                            values = list(e = e$message),
                            script = "script3",
                            file = "run_script_3_create_tables.R",
                            errorCode = "tryCatch",
                            functionName = "link_product_patient"
                        )
                    )
                },
                warning = function(w) {
                    logWarn(
                        log_to_json(
                            "Could not link files for product and patient data. Warning = {values['w']}.",
                            values = list(w = w$message),
                            script = "script3",
                            file = "run_script_3_create_tables.R",
                            warningCode = "tryCatch",
                            functionName = "link_product_patient"
                        )
                    )
                }
            )
        },
        output_root = paths$output_root
    )
}

main()
