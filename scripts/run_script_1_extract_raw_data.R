options(readxl.show_progress = FALSE)
future::plan("multisession")

paths <- a4d::init_paths(c("patient_data_raw", "product_data_raw"), delete = TRUE)
a4d::setup_logger(paths$output_root, "script1")
tracker_files <- a4d::get_files(paths$tracker_root)

ParallelLogger::logInfo(
    "Found ",
    length(tracker_files),
    " xlsx files under ",
    paths$tracker_root,
    "."
)

ParallelLogger::logInfo("Start processing tracker files.")

progressr::with_progress({
    p <- progressr::progressor(steps = length(tracker_files))

    result <- furrr::future_map(
        tracker_files,
        a4d::process_tracker_file,
        paths = paths,
        p = p
    )
})

ParallelLogger::logInfo("Finish processing all tracker files.")

ParallelLogger::clearLoggers()
