library(furrr)
library(progressr)
library(tictoc)

options(readxl.show_progress = FALSE)
plan("multisession")


paths <- a4d::init_paths(c("patient_data_raw", "product_data_raw"), delete = TRUE)
a4d::setup_logger(paths$output_root, "script1")
tracker_files <- a4d::get_files(paths$tracker_root)
synonyms <- a4d::get_synonyms()

logInfo(
    "Found ",
    length(tracker_files),
    " xlsx files under ",
    paths$tracker_root,
    "."
)

logInfo("Start processing tracker files.")

tic()
with_progress({
    p <- progressor(steps = length(tracker_files), label = "Tracker files")

    future_map(
        tracker_files,
        function(tracker_file) {
            a4d::process_tracker_file(tracker_file, paths, synonyms)
            p()
            return()
        },
        .options = furrr_options(seed = NULL, packages = c("a4d", "ParallelLogger"), scheduling = FALSE)
    )
})
toc()

logInfo("Finish processing all tracker files.")

clearLoggers()

plan("sequential")
