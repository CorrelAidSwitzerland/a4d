#' @title Create CSV with longitudinal patient data for a single variable.
#'
#' @description
#' Read in all cleaned patient data CSV and create a single data.frame.
#' Group this data by id and take only the months when there is a change in the medical data.
#'
#'
#' @param patient_data_files list of CSV files with cleaned patient data from step 2.
#' @param input_root root directory of the input CSV files.
#' @param output_root root directory of the output folder.
#' @param variable name of the column that should be exported.
#' @param name name used to create the export file name.
create_table_longitudinal_data <-
    function(patient_data_files,
             input_root,
             output_root,
             variable,
             name) {
        logInfo("Start creating single csv for table patient_data_changes_only and variable ", variable)

        dynamic_patient_columns <-
            c(
                "blood_pressure_dias_mmhg",
                "blood_pressure_sys_mmhg",
                "bmi",
                "bmi_date",
                "clinic_code",
                "country_code",
                "fbg_updated_date",
                "fbg_updated_mg",
                "fbg_updated_mmol",
                "file_name",
                "hba1c_updated",
                "hba1c_updated_exceeds",
                "hba1c_updated_date",
                "height",
                "hospitalisation_cause",
                "hospitalisation_date",
                "id",
                "insulin_regimen",
                "last_clinic_visit_date",
                "last_remote_followup_date",
                "observations",
                "observations_category",
                "sheet_name",
                "status",
                "support_from_a4d",
                "testing_frequency",
                "tracker_month",
                "tracker_year",
                "updated_2022_date",
                "weight"
            )

        patient_data_list <- list()

        # get the latest static patient data for each tracker file
        for (patient_file in patient_data_files) {
            patient_data <- read_csv(
                file.path(input_root, patient_file),
                locale = readr::locale(encoding = "UTF-16LE"),
                show_col_types = F,
                col_types = "iiinDccDcnnDnncnlnlDncDccDDDccccDccccciDciiiDn",
                col_select = all_of(dynamic_patient_columns)
            )

            patient_data_list[[patient_file]] <- patient_data
        }

        # Complete dataframe
        patient_data_df <- patient_data_list %>%
            bind_rows()

        # get latest static patient data overall
        variable_lag <- paste0(variable, "_lag")
        longitudinal_data <- patient_data_df %>%
            drop_na(!!variable) %>%
            dplyr::filter(!!variable != ERROR_VAL_NUMERIC) %>%
            group_by(id) %>%
            arrange(tracker_year, tracker_month) %>%
            mutate(!!variable_lag := dplyr::lag(get(variable), default = NULL)) %>%
            ungroup() %>%
            mutate(!!variable_lag := replace_na(get(variable_lag), ERROR_VAL_NUMERIC)) %>%
            dplyr::filter(get(variable) != get(variable_lag)) %>%
            select(-!!variable_lag) %>%
            arrange(id, tracker_year, tracker_month)


        export_data(
            data = longitudinal_data,
            filename = paste0("longitudinal_data_", name),
            output_root = output_root,
            suffix = ""
        )

        logInfo("Finish creating single csv for table patient_data_changes_only and variable ", variable)
    }