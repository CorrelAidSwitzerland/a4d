#' Read in all processed and cleaned patient data files
#'
#' @param input_root root directory containing all files.
#' @param patient_data_files list of CSV files with cleaned patient data.
#'
#' @return data frame holding all cleaned patient data with correct column types.
#' @export
read_cleaned_patient_data <-
    function(input_root, patient_data_files) {
        logInfo("Start read_cleaned_patient_data")
        patient_data_list <- list()

        # get the latest static patient data for each tracker file
        for (patient_file in patient_data_files) {
            patient_data <-  arrow::read_parquet(file.path(input_root, patient_file))

            patient_data_list[[patient_file]] <- patient_data
        }

        # Complete dataframe
        patient_data <- patient_data_list %>%
            bind_rows()

        logInfo("Finish read_cleaned_patient_data")
        patient_data
    }
