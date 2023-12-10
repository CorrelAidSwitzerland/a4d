#' @title Create CSV with monthly patient data
#'
#' @description
#' Read in all cleaned patient data CSV and create a single data.frame.
#' Only take dynamic columns.
#'
#'
#' @param patient_data_files list of CSV files with cleaned patient data from step 2.
#' @param input_root root directory of the input CSV files.
#' @param output_root root directory of the output folder.
create_table_patient_data_monthly <- function(patient_data_files, input_root, output_root) {
    logInfo("Start creating single csv for table patient_data_monthly.")

    # THERE MIGHT BE MONTHLY COLUMNS MISSING - PLEASE ADD THEM
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

    patient_data <- read_cleaned_patient_data(input_root, patient_data_files) %>%
        dplyr::select(tidyselect::all_of(dynamic_patient_columns)) %>%
        dplyr::arrange(tracker_year, tracker_month, id)

    export_data_as_parquet(
        data = patient_data,
        filename = "patient_data_monthly",
        output_root = output_root,
        suffix = ""
    )

    logInfo("Finish creating single csv for table patient_data_monthly.")
}
