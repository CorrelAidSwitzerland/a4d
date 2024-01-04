library(furrr)
library(progressr)
library(tictoc)

plan("multisession")


paths <- a4d::init_paths(c("patient_data_cleaned", "product_data_cleaned"), delete = TRUE)
a4d::setup_logger(paths$output_root, "script2")
patient_data_files <- a4d::get_files(paths$tracker_root, pattern = "patient_raw.parquet$")
product_data_files <- a4d::get_files(paths$tracker_root, pattern = "product_raw.parquet$")
allowed_provinces <- a4d::get_allowed_provinces()
synonyms <- a4d::get_synonyms()

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

logInfo("Start processing patient csv files.")

tic()
with_progress({
    p <- progressor(steps = length(patient_data_files))

    result <- future_map(
        patient_data_files,
        function(patient_file) {
            a4d::process_patient_file(patient_file, paths, allowed_provinces)
            p()
            return()
        },
        .options = furrr_options(seed = NULL, packages = c("a4d", "ParallelLogger"), scheduling = FALSE)
    )
})
toc()

logInfo("Finish processing all patient csv files.")

logDebug("Start processing product csv files.")

tic()
with_progress({
    p <- progressor(steps = length(product_data_files))

    result <- future_map(
        product_data_files,
        function(patient_file) {
            a4d::process_product_file(patient_file, paths, synonyms_product = synonyms$product)
            p()
            return()
        },
        .options = furrr_options(seed = NULL, packages = c("a4d", "ParallelLogger"), scheduling = FALSE)
    )
})
toc()

logInfo("Finish processing all csv files.")

clearLoggers()

plan("sequential")
