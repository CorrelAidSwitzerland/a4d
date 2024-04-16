#' Read in all processed and cleaned patient data files
#'
#' @param input_root root directory containing all files.
#' @param patient_data_files list of CSV files with cleaned patient data.
#'
#' @return data frame holding all cleaned patient data with correct column types.
#' @export
read_cleaned_patient_data <-
    function(input_root, patient_data_files) {
        patient_data <- patient_data_files %>%
            purrr::map(function(patient_file) {
                arrow::read_parquet(file.path(input_root, patient_file))
            }) %>%
            dplyr::bind_rows()

        patient_data
    }
