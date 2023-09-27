

download_data <- function() {
    print("Start downloading data from GCP Storage")
    command <- "gsutil -m cp -r gs://a4dphase2_upload /home/rstudio/data"
    exit_code <- system(command)
    if (exit_code != 0) {
        stop("Error during downloading data")
    }
    print("Finished downloading data from GCP Storage")
}

upload_data <- function() {
    print("Start uploading data to GCP Storage")
    command <- "gsutil -m cp -r /home/rstudio/data/a4dphase2_upload/output/ gs://a4dphase2_output"
    exit_code <- system(command)
    if (exit_code != 0) {
        stop("Error during uploading data")
    }
    print("Finished uploading data to GCP Storage")
}

download_data()
source("run_script_1_extract_raw_data.R")
source("run_script_2_clean_data.R")
upload_data()
