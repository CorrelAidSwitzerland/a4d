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
    # THERE MIGHT BE STATIC COLUMNS MISSING - PLEASE ADD THEM
    static_patient_columns <-
        c(
            "age",
            "clinic_id",
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
            "tracker_date",
            "tracker_month",
            "tracker_year"
        )


    static_patient_data <- read_cleaned_patient_data(input_root, patient_data_files) %>%
        dplyr::select(tidyselect::all_of(static_patient_columns))

    # get latest static patient data overall
    static_patient_data <- static_patient_data %>%
        dplyr::group_by(id) %>%
        dplyr::slice_max(tracker_year, n = 1) %>%
        dplyr::slice_max(tracker_month, n = 1) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(tracker_year, tracker_month, id)

    testit::assert(sum(duplicated(static_patient_data$id)) == 0)

    logInfo(
        log_to_json(
            message = "static_patient_data dim: {values['dim']}.",
            values = list(dim = dim(static_patient_data)),
            script = "script3",
            file = "create_table_patient_data_static.R",
            functionName = "create_table_patient_data_static"
        )
    )

    export_data_as_parquet(
        data = static_patient_data,
        filename = "patient_data",
        output_root = output_root,
        suffix = "_static"
    )
}
