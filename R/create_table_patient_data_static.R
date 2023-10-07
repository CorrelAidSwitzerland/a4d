#' @title Create CSV with static patient data
#'
#' @description
#' Read in all cleaned patient data CSV and create a single data.frame.
#' Group this data by id and take the latest available data (latest year and month).
#'
#'
#' @param patient_data_files list of CSV files with cleaned patient data from step 2.
#' @param input_root root directory of the input CSV files.
#' @param output_root root directory of the output folder.
create_table_patient_data_static <- function(patient_data_files, input_root, output_root) {
    logInfo("Start creating single csv for table patient_data_static.")

    # THERE MIGHT BE STATIC COLUMNS MISSING - PLEASE ADD THEM
    static_patient_columns <-
        c(
            "age",
            "dob",
            "edu_occ",
            "fbg_baseline_mg",
            "fbg_baseline_mmol",
            "hba1c_baseline",
            "hba1c_baseline_exceeds",
            "id",
            "last_clinic_visit_date",
            "lost_date",
            "name",
            "province",
            "recruitment_date",
            "sex",
            "status_out",
            "t1d_diagnosis_age",
            "t1d_diagnosis_date",
            "t1d_diagnosis_with_dka",
            "tracker_month",
            "tracker_year"
        )


    static_patient_data_list <- list()

    # get the latest static patient data for each tracker file
    for (patient_file in patient_data_files) {
        patient_data <- readr::read_csv(
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

    logInfo("Finish creating single csv for table patient_data_static.")
}
