
create_table_patient_data_static <- function(patient_data_files, input_root, output_root) {
    logInfo("Start creating single csv for table patient_data_staic.")

    # THERE MIGHT BE STATIC COLUMNS MISSING - PLEASE ADD THEM
    static_patient_columns <-
        c(
            "id",
            "name",
            "province",
            "sex",
            "dob",
            "age",
            "t1d_diagnosis_age",
            "t1d_diagnosis_date",
            "t1d_diagnosis_with_dka",
            "recruitment_date",
            "edu_occ",
            "tracker_month",
            "file_name",
            "tracker_year",
            "status",
            "last_clinic_visit_date",
            "hba1c_baseline",
            "hba1c_baseline_exceeds",
            "fbg_baseline_mg",
            "fbg_baseline_mmol"
        )


    static_patient_data_list <- list()

    # get the latest static patient data for each tracker file
    for (patient_file in patient_data_files) {
        patient_data <- read_csv(
            file.path(input_root, patient_file),
            locale = readr::locale(encoding = "UTF-16LE"),
            show_col_types = F,
            col_types = "iiinDccDcnnDnncnlnlDncDccDDDccccDccccciDciiiDn",
            col_select = all_of(static_patient_columns)
        )

        static_patient_data_list[[patient_file]] <- patient_data
    }

    # Complete dataframe
    static_patient_data_df <- static_patient_data_list %>%
        bind_rows()

    # get latest static patient data overall
    static_patient_data_df <- static_patient_data_df %>%
        group_by(id) %>%
        slice_max(tracker_year, n = 1) %>%
        slice_max(tracker_month, n = 1) %>%
        slice_head(n = 1) %>%
        ungroup()

    testit::assert(sum(duplicated(static_patient_data_df$id)) == 0)

    export_data(
        data = static_patient_data_df,
        filename = "patient_data",
        output_root = output_root,
        suffix = "_static"
    )

    logInfo("Finish creating single csv for table patient_data_staic.")

}
