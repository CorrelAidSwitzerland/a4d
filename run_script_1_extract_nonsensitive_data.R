doParallel::registerDoParallel()
options(readxl.show_progress = FALSE)

main <- function() {
    paths <- init_paths()
    setup_logger(paths$output_root)
    tracker_files <- get_tracker_files(paths$tracker_root)
    log_info("Found {length(tracker_files)} xlsx files under {paths$tracker_root}.")

    synonyms <- get_synonyms()

    log_debug("Start processing tracker files.")
    log_appender(appender_stdout)
    foreach::`%dopar%`(
        foreach::foreach(tracker_file = tracker_files),
        foreach_process(tracker_file, paths, synonyms)
    )
    log_appender(appender_console)

    log_success("Finish processing all tracker files.")
}


foreach_process <- function(tracker_file, paths, synonyms) {
    setup_sink(paths$output_root, tracker_file)
    tryCatch(
        process_tracker_file(paths, tracker_file, synonyms),
        error = function(e) {
            log_error("Could not process {tracker_file}. Error = {e}.")
        },
        warnning = function(w) {
            log_warn("Could not process {tracker_file}. Warning = {w}.")
        }
    )
    sink()
}


init_paths <- function() {
    # tracker_file <-
    # rstudioapi::selectFile(path = tracker_root_path, filter = "Excel Workbook (*.xlsx)")
    tracker_root_path <- select_A4D_directory()
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
    tracker_files <- list.files(tracker_root, "*.xlsx")
    tracker_files <-
        tracker_files[str_detect(tracker_files, "~", negate = T)]
}


process_tracker_file <- function(paths, tracker_file, synonyms) {
    log_debug("Start process_tracker_file.")
    log_info("Current file: {tracker_file}.")
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

    log_success("Finish process_tracker_file.")
}


process_patient_data <-
    function(tracker_file,
             tracker_data_file,
             output_root,
             synonyms_patient) {
        log_debug("Start process_patient_data.")

        df_raw_patient <-
            reading_patient_data_2(
                tracker_data_file = tracker_data_file,
                columns_synonyms = synonyms_patient
            )
        log_debug("df_raw_patient dim: {dim(df_raw_patient)}.")

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

        log_success("Finish process_patient_data.")
    }


process_product_data <-
    function(tracker_file,
             tracker_data_file,
             output_root,
             synonyms_product) {
        log_info("Start process_product_data.")

        df_raw_product <-
            reading_product_data_step1(
                tracker_data_file = tracker_data_file,
                columns_synonyms = synonyms_product
            )
        log_debug("df_raw_product dim: {dim(df_raw_product)}.")

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
        }
        log_success("Finish process_product_data.")
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
    log_debug("Start export_data. Suffix = {suffix}.")
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
    log_success("Finish export_data. Suffix = {suffix}")
}

main()
