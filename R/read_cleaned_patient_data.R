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
        col_types <- list(
            "age" = "i",
            "blood_pressure_dias_mmhg" = "i",
            "blood_pressure_sys_mmhg" = "i",
            "bmi" = "n",
            "bmi_date" = "D",
            "clinic_code" = "c",
            "country_code" = "c",
            "dob" = "D",
            "edu_occ" = "c",
            "fbg_baseline_mg" = "n",
            "fbg_baseline_mmol" = "n",
            "fbg_updated_date" = "D",
            "fbg_updated_mg" = "n",
            "fbg_updated_mmol" = "n",
            "file_name" = "c",
            "hba1c_baseline" = "n",
            "hba1c_baseline_exceeds" = "l",
            "hba1c_updated" = "n",
            "hba1c_updated_exceeds" = "l",
            "hba1c_updated_date" = "D",
            "height" = "n",
            "hospitalisation_cause" = "c",
            "hospitalisation_date" = "D",
            "id" = "c",
            "insulin_regimen" = "c",
            "last_clinic_visit_date" = "D",
            "last_remote_followup_date" = "D",
            "lost_date" = "D",
            "name" = "c",
            "observations" = "c",
            "observations_category" = "c",
            "province" = "c",
            "recruitment_date" = "D",
            "sex" = "c",
            "sheet_name" = "c",
            "status" = "c",
            "status_out" = "c",
            "support_from_a4d" = "c",
            "t1d_diagnosis_age" = "i",
            "t1d_diagnosis_date" = "D",
            "t1d_diagnosis_with_dka" = "c",
            "testing_frequency" = "i",
            "tracker_month" = "i",
            "tracker_year" = "i",
            "updated_2022_date" = "D",
            "weight" = "n"
        )

        patient_data_list <- list()

        # get the latest static patient data for each tracker file
        for (patient_file in patient_data_files) {
            patient_data <- readr::read_csv(
                file.path(input_root, patient_file),
                locale = readr::locale(encoding = "UTF-16LE"),
                show_col_types = F,
                col_types = do.call(paste0, col_types)
            )

            patient_data_list[[patient_file]] <- patient_data
        }

        # Complete dataframe
        patient_data <- patient_data_list %>%
            bind_rows()

        logInfo("Finish read_cleaned_patient_data")
        patient_data
    }
