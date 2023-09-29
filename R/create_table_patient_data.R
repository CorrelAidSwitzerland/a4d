create_table_patient_data <- function(patient_data_files, input_root, output_root) {
    logInfo("Start creating single csv for table patient_data.")

    # THERE MIGHT BE STATIC COLUMNS MISSING - PLEASE ADD THEM
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
    patient_data_df <- patient_data_df %>%
        arrange(tracker_year, tracker_month, id)

    export_data(
        data = patient_data_df,
        filename = "patient_data",
        output_root = output_root,
        suffix = ""
    )

    logInfo("Finish creating single csv for table patient_data.")
}
