
#' @title Get patient data from "Patient AN Data" sheet
#'
#' @description
#' Extract the data in the "Patient AN Data" sheet
#' as data.frame.
#' The data is cleaned and the column headers are sanitized and
#' replaced with their standard variable name.
#'
#'
#' @param tracker_data_file description
#' @param sheet_list description
#' @param columns_synonyms description
#'
#' @return description
#' @export
#'
read_patient_an_data <- function(tracker_data_file, sheet_list, columns_synonyms) {
    if (any(grepl("AN Data", sheet_list))) {
        patient_sheet <- sheet_list[na.omit(grepl("AN Data", sheet_list))]

        # AN PATIENT DATA DATA (merge/join at the end of the if year):
        an_patient_data <-
            data.frame(readxl::read_xlsx(tracker_data_file, patient_sheet))
        all_patient_ids <- an_patient_data$Patient.ID

        an_patient_data <- clean_anon_data(an_patient_data)
        print("cleaned patient anon data")
    } else {
        warning("File has no AN DATA SHEET - Either fake data file or error")
        an_patient_data <- NA
    }
    print("patient AN Data extracted")
    return(an_patient_data)
}


clean_anon_data <- function(an_patient_data) {
    # Beginning with 2021, the patient an data has a different header
    # going over two rows, so here we check for the new format
    if (is.na(an_patient_data[1,1]) || str_to_lower(an_patient_data[1,1]) != "patient id") {
        colnames(an_patient_data) <- an_patient_data[1, ]
        an_patient_data <- an_patient_data[-1, ]

        an_patient_data <- an_patient_data[!names(an_patient_data) %in% "NA"]
    }

    col_names <- colnames(an_patient_data)
    col_names <- sanitize_column_name(col_names)
    synonym_headers <- sanitize_column_name(columns_synonyms$tracker_name)

    # replacing var codes
    colnames_found <- match(col_names, synonym_headers, nomatch = 0)
    col_names[col_names %in% synonym_headers] <- columns_synonyms$variable_name[colnames_found]
    colnames(an_patient_data) <- col_names

    if (sum(colnames_found == 0) != 0) {
        print("Could not find a match for these an patient data columns:")
        print(colnames(an_patient_data)[colnames_found == 0])
    }

    return(an_patient_data)
}
