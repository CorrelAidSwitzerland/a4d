options(readxl.show_progress = FALSE)
future::plan("multisession")

paths <- a4d::init_paths(c("patient_data_cleaned", "product_data_cleaned"), delete = TRUE)
a4d::setup_logger(paths$output_root, "script2")
patient_data_files <- a4d::get_files(paths$tracker_root, pattern = "patient_raw.parquet$")
product_data_files <- a4d::get_files(paths$tracker_root, pattern = "product_raw.parquet$")
ParallelLogger::logInfo(
    "Found ",
    length(patient_data_files),
    " patient csv files under ",
    paths$tracker_root,
    "."
)
ParallelLogger::logInfo(
    "Found ",
    length(product_data_files),
    " product csv files under ",
    paths$tracker_root,
    "."
)

ParallelLogger::logInfo("Start processing patient csv files.")

progressr::with_progress({
    p <- progressr::progressor(steps = length(patient_data_files))

    result <- furrr::future_map(
        patient_data_files,
        a4d::process_patient_file,
        paths = paths,
        p = p
    )
})

ParallelLogger::logInfo("Finish processing all patient csv files.")

ParallelLogger::logDebug("Start processing product csv files.")

progressr::with_progress({
    p <- progressr::progressor(steps = length(product_data_files))

    result <- furrr::future_map(
        product_data_files,
        a4d::process_product_file,
        paths = paths,
        p = p
    )
})

ParallelLogger::logInfo("Finish processing all csv files.")

ParallelLogger::clearLoggers()
