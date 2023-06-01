# extracting country and clinic code from patient ID
# expects that patient ID has a certain format
extract_country_clinic_code <- function(patient_data) {
    patient_ids <- patient_data["id"] %>%
        drop_na() %>%
        rowwise() %>%
        mutate(country = str_split(id, "_", n = 2, simplify = T)[1], clinic = substr(str_split(id, "_", n = 2, simplify = T)[2], 0, 2))
    country_code <- names(sort(table(patient_ids$country), decreasing = T))[1]
    clinic_code <- names(sort(table(patient_ids$clinic), decreasing = T))[1]

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
    tracker_data <-
        openxlsx::read.xlsx(
            xlsxFile = tracker_data_file,
            fillMergedCells = TRUE,
            skipEmptyRows = FALSE,
            skipEmptyCols = FALSE,
            colNames = FALSE,
            rowNames = FALSE,
            detectDates = FALSE,
            sheet = sheet
        )
    # Assumption: first column is always empty until patient data begins
    patient_data_range <- which(!is.na(tracker_data[, 1]))
    row_min <- min(patient_data_range)
    row_max <- max(patient_data_range)

    header_cols <- str_replace(as.vector(t(tracker_data[row_min - 1,])), "\r\n", "")
    patient_df <- readxl::read_excel(
        path = tracker_data_file,
        sheet=sheet,
        range=readxl::cell_limits(c(row_min, NA), c(row_max, length(header_cols))),
        trim_ws=T,
        col_names=F,
    )

    if (year %in% c(2019, 2020, 2021, 2022)) {
        # take into account that date info gets separated from the updated values (not in the same row, usually in the bottom row)
        header_cols_2 <- str_replace(as.vector(t(tracker_data[row_min - 2, ])), "\r\n", "")
        diff_colnames <- which((header_cols != header_cols_2))
        header_cols[diff_colnames] <- paste(header_cols_2[diff_colnames], header_cols[diff_colnames])

        empty_colnames <- which(is.na(header_cols))
        header_cols[empty_colnames] <- header_cols_2[empty_colnames]
    }

    colnames(patient_df) <- header_cols

    # only keep columns with header
    patient_df <- patient_df %>% select(header_cols[!is.na(header_cols)])

    # removes duplicate columns that appear due to merged cells (e.g. insulin regimen)
    patient_df <- patient_df %>% distinct()
    # patient_df <- patient_df %>% select(unique(colnames(.))) # is this a good alternative?

    patient_df <- patient_df %>% mutate(across(everything(), as.character))
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
harmonize_patient_data_columns <- function(patient_df, columns_synonyms) {
    patient_df <- patient_df %>% discard(~ all(is.na(.) | . == ""))
    patient_df <- patient_df[!is.na(names(patient_df))]

    colnames(patient_df) <- sanitize_column_name(colnames(patient_df))
    synonym_headers <- sanitize_column_name(columns_synonyms$tracker_name)

    # replacing var codes
    colnames_found <- match(colnames(patient_df), synonym_headers, nomatch = 0)
    colnames(patient_df)[colnames(patient_df) %in% synonym_headers] <- columns_synonyms$variable_name[colnames_found]

    if (sum(colnames_found == 0) != 0) {
        "Non-matching column names found (see 0)"
        view(colnames_found)
    } else {
        return(patient_df)
    }
}


# adjust new harmonize function ---------------------------------------------------------
# adjusted harmonize function that has the same name as the original function
# function is based on harmonize_patient_data_columns() but shortened
# Might need a better solution
harmonize_patient_data_columns_2 <- function(patient_df, columns_synonyms) {
    # Uncommented because we want to retain columns with only NAs
    # patient_df <- patient_df %>% discard(~ all(is.na(.) | . == ""))
    patient_df <- patient_df[!is.na(names(patient_df))]

    fbg_baseline_col_idx <- which(colnames(patient_df) %in% (columns_synonyms %>% dplyr::filter(variable_name == "baseline_fbg"))$tracker_name)
    if (length(fbg_baseline_col_idx) > 0) {
        patient_df <- patient_df %>%
            mutate(
                baseline_fbg_unit = sanitize_str(
                    str_match(
                        colnames(patient_df)[fbg_baseline_col_idx],
                        "\\(.*\\)"
                    )[1]
                )
            )
    }

    colnames(patient_df) <- sanitize_str(colnames(patient_df))
    synonym_headers <- sanitize_str(columns_synonyms$tracker_name)

    # replacing var codes
    colnames_found <- match(colnames(patient_df), synonym_headers, nomatch = 0)
    colnames(patient_df)[colnames(patient_df) %in% synonym_headers] <- columns_synonyms$variable_name[colnames_found]
    # browser()

    mismatching_column_ids <- which(colnames_found == 0)
    if (length(mismatching_column_ids) > 0) {
        print("Non-matching column names found:")
        print(colnames(patient_df)[mismatching_column_ids])
    }

    patient_df
}


extract_date_from_measurement_column <- function(patient_df, colname) {
    # produces columns with names coherent with original naming before refactor
    colname_value <- paste(c(colname, ""), collapse = "")
    colname_core <- sub("[_][^_]+$", "", colname) # remove last element after "_"
    colname_date <- paste(c(colname_core, "_date"), collapse = "")
    patient_df <- separate(
        data = patient_df, col = colname,
        into = c(colname_value, colname_date), sep = "([(])"
    )
    patient_df[[colname_date]] <- gsub(")", "", patient_df[[colname_date]])
    print(c("separated column: ", colname))

    return(patient_df)
}

transform_MM_DD_to_YYYY_MM_DD_str <- function(column, year) {
    for (i in 1:length(column)) {
        if (!is.na(column[i])) {
            arr <- str_split(column[i], "-") %>% unlist()
            day <- arr[2]
            month <- ifelse(day < 10, paste0("0", day), as.character(day))
            month <- match(c(arr[1]), month.abb)
            month <- ifelse(month < 10, paste0("0", month), as.character(month))

            column[i] <- as.character(paste(year, month, day, sep = "/"))
        }
    }
    return(as.character(column))
}



bmi_fix <- function(patient_df) {
    if ("height" %in% colnames(patient_df) & "weight" %in% colnames(patient_df)) {
        patient_df <- patient_df %>%
            mutate(bmi = if_else(is.na(height) | is.na(weight), NA_character_, bmi))
    }
    return(patient_df)
}



date_fix <- function(df, year) { # used to be initial_clean_up_patient_df

    format <- "%Y/%m/%d"


    if ("recruitment_date" %in% colnames(df) & year > 2018) {
        df <- df %>%
            mutate(recruitment_date = as.Date(as.numeric(recruitment_date) * (60 * 60 * 24),
                origin = "1899-12-30",
                format = format,
                tz = "GMT"
            ))
    }

    if ("last_clinic_visit_date" %in% colnames(df)) {
        df <- df %>%
            mutate(last_clinic_visit_date = as.POSIXct(as.numeric(last_clinic_visit_date) * (60 * 60 * 24),
                format = format,
                origin = "1899-12-30",
                tz = "GMT"
            ))
    }

    if ("bmi_date" %in% colnames(df)) {
        df <- df %>%
            mutate(
                bmi_date = as.POSIXct(as.numeric(bmi_date) * (60 * 60 * 24),
                    format = format,
                    origin = "1899-12-30",
                    tz = "GMT"
                ),
                bmi_date = format(as.Date(bmi_date), "%Y-%m")
            )
    }

    if ("updated_fbg_date" %in% colnames(df) & year > 2018) {
        df <- df %>%
            mutate(
                updated_fbg_date = as.POSIXct(as.numeric(updated_fbg_date) * (60 * 60 * 24),
                    format = format,
                    origin = "1899-12-30",
                    tz = "GMT"
                ),
                updated_fbg_date = case_when(year(updated_fbg_date) < 100 ~ updated_fbg_date %m+% years(2000), TRUE ~ updated_fbg_date)
            )
    }

    if ("updated_hba1c_date" %in% colnames(df) & year > 2018) {
        df <- df %>%
            mutate(
                updated_hba1c_date = as.POSIXct(as.numeric(updated_hba1c_date) * (60 * 60 * 24),
                    format = format,
                    origin = "1899-12-30",
                    tz = "GMT"
                ),
                updated_hba1c_date = case_when(year(updated_hba1c_date) < 100 ~ updated_hba1c_date %m+% years(2000), TRUE ~ updated_hba1c_date)
            )
    }

    return(df)
}



bp_fix <- function(df) {
    df <- df %>%
        separate(blood_pressure_mmhg, c("blood_pressure_sys_mmhg", "blood_pressure_dias_mmhg"), sep = "([/])")
}
