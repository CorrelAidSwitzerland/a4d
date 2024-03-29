# extracting country and clinic code from patient ID
# expects that patient ID has a certain format
extract_country_clinic_code <- function(patient_data) {
    logDebug("Start extract_country_clinic_code.")
    patient_ids <- patient_data["id"] %>%
        dplyr::filter(id != "0") %>%
        tidyr::drop_na() %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            country = stringr::str_split(id, "_", n = 2, simplify = T)[1],
            clinic = substr(stringr::str_split(
                id, "_",
                n = 2, simplify = T
            )[2], 0, 2)
        )

    country_code <-
        names(sort(table(patient_ids$country), decreasing = T))[1]
    clinic_code <-
        names(sort(table(patient_ids$clinic), decreasing = T))[1]

    logDebug("country_code = ", country_code, ".")
    logDebug("clinic_code = ", clinic_code, ".")
    logDebug("Finish extract_country_clinic_code.")
    return(list("country_code" = country_code, "clinic_code" = clinic_code))
}


#' Extract the patient data from a month sheet of a tracker file.
#'
#' @description
#' Searches for first and last row that starts with country and clinic code
#' and extracts the data from this range as data.frame
#'
#'
#' @param tracker_data_file Excel tracker file.
#' @param sheet Excel sheet name.
#' @param year year of this tracker.
#'
#' @return data.frame with the patient data
#' @export
extract_patient_data <- function(tracker_data_file, sheet, year) {
    logDebug("Start extract_patient_data for sheet = ", sheet, ".")

    logDebug("Start openxlsx::read.xlsx to get tracker_data.")
    tracker_data <- openxlsx::read.xlsx(
        xlsxFile = tracker_data_file,
        sheet = sheet,
        colNames = F,
        detectDates = F,
        skipEmptyRows = F,
        fillMergedCells = T
    )
    # tracker_data <- readxl::read_excel(
    # path = tracker_data_file,
    # sheet = sheet,
    # range = readxl::cell_limits(c(1, NA), c(NA, NA)),
    # trim_ws = T,
    # col_names = F,
    # .name_repair = "unique_quiet"
    # )
    logDebug("Finish openxlsx::read.xlsx.")

    # Assumption: first column is always empty until patient data begins
    patient_data_range <- which(!is.na(tracker_data[, 1]))
    row_min <- min(patient_data_range)
    row_max <- max(patient_data_range)

    testit::assert(row_min <= row_max) # <= because there could only be a single patient

    header_cols <-
        stringr::str_replace_all(as.vector(t(tracker_data[row_min - 1, ])), "[\r\n]", "")
    header_cols_2 <-
        stringr::str_replace_all(as.vector(t(tracker_data[row_min - 2, ])), "[\r\n]", "")

    # trackers from 2020 and newer have an empty first row
    # and openxlsx always skips empty rows at the start of the file
    if (year == 2022 && sheet != "Patient List") {
        row_min <- row_min + 1
        row_max <- row_max + 1
    }

    logInfo("Patient data found in rows ", row_min, " to ", row_max, ".")
    logDebug("Start readxl::read_excel to get patient data.")
    df_patient <- readxl::read_excel(
        path = tracker_data_file,
        sheet = sheet,
        range = readxl::cell_limits(c(row_min, NA), c(row_max, length(header_cols))),
        trim_ws = F,
        col_names = F,
        col_types = c("text"),
        .name_repair = "unique_quiet"
    )
    logDebug("Finish readxl::read_excel.")

    if (header_cols[2] == header_cols_2[2]) {
        # take into account that date info gets separated from the updated values (not in the same row, usually in the bottom row)
        logInfo("Read in multiline header.")

        diff_colnames <- which((header_cols != header_cols_2))
        header_cols[diff_colnames] <-
            paste(header_cols_2[diff_colnames], header_cols[diff_colnames])

        empty_colnames <- which(is.na(header_cols))
        header_cols[empty_colnames] <- header_cols_2[empty_colnames]
    }

    colnames(df_patient) <- header_cols
    logDebug("Found patient column names = ", paste(header_cols, collapse = ","), ".")

    # delete columns without a header (=NA)
    df_patient <- df_patient[, !is.na(colnames(df_patient))]

    # removes duplicate columns that appear due to merged cells (e.g. insulin regimen)
    # df_patient <- df_patient %>% distinct()
    # remove empty rows with only NA
    df_patient <-
        df_patient[rowSums(is.na(df_patient)) != ncol(df_patient), ]

    logDebug("Finish extract_patient_data.")

    df_patient
}



#' Harmonize patient data column names.
#'
#' @description
#' Imports the patient df, cleans it and matches it against
#' column synonyms to unify column names
#'
#' @param patient_df data.frame holding the patient data of the month sheet of a tracker.
#' @param columns_synonyms data.frame with synonyms for tracker variables.
#'
#' @return data.frame with harmonized column names.
#' @export
harmonize_patient_data_columns <-
    function(patient_df, columns_synonyms) {
        logDebug("Start harmonize_patient_data_columns.")

        patient_df <- patient_df[!is.na(names(patient_df))]

        colnames(patient_df) <- sanitize_str(colnames(patient_df))
        synonym_headers <- sanitize_str(columns_synonyms$tracker_name)

        colnames_found <-
            match(colnames(patient_df), synonym_headers, nomatch = 0)
        colnames(patient_df)[colnames(patient_df) %in% synonym_headers] <-
            columns_synonyms$variable_name[colnames_found]

        mismatching_column_ids <- which(colnames_found == 0)
        if (length(mismatching_column_ids) > 0) {
            logWarn(
                "Non-matching column names found: ", paste(colnames(patient_df)[mismatching_column_ids], collapse = ","), "."
            )
        }

        logDebug("Finish harmonize_patient_data_columns.")
        patient_df
    }
