# helper function helping to extract country/clinic code based on regex; extracts code based on all word characters following a string_to_match
# first checks whether string_to_match (e.g. country_) is found in any row and returns column name where it was found
# if any column name returned, code is extracted from that column
# if not column names are checked
# otherwise do as it was done before (take from first patient id), works with patient data not with product data code
# e.g. country_PB would be replaced to PB
extract_code_from_df <- function(string_to_match, tracker_data) {
    column_names <- colnames(tracker_data)[grepl(string_to_match, tracker_data, ignore.case = TRUE)][1]

    if (!is_empty(column_names) & !is.na(column_names)) {
        tracker_data <- tracker_data[, !duplicated(colnames(tracker_data))] # remove duplicated column names, takes the first one which usually works, otherwise rlang error due to multiple columns with same name
        column_row <- tracker_data %>%
            dplyr::filter(grepl(string_to_match, !!rlang::sym(column_names), ignore.case = TRUE))
        code <- sub(paste0(".*", string_to_match, "([a-zA-Z]+).*"), "\\1", column_row[column_names], ignore.case = TRUE)
    } else if (any(grepl(string_to_match, colnames(tracker_data), ignore.case = TRUE))) {
        column_name <- colnames(tracker_data)[grepl(string_to_match, colnames(tracker_data), ignore.case = TRUE)]
        code <- sub(paste0(".*", string_to_match, "([a-zA-Z]+).*"), "\\1", column_name, ignore.case = TRUE)
    } else {
        tracker_data_sub <- tracker_data[2:nrow(tracker_data), 2]
        id_loc <- min(which(str_detect(tracker_data_sub, "_") == 1))

        if (string_to_match == "country_") {
            code <- substr(tracker_data_sub[id_loc], 1, 2)
        } else {
            code <- substr(tracker_data_sub[id_loc], 4, 5)
        }
    }

    return(code)
}

# extracting country and clinic code based on all word characters folowing the string country_ and clinic_
# expects that one sheet contains one country and one clinic code
extract_country_clinic_code <- function(tracker_data) {
    country_code <- extract_code_from_df("country_", tracker_data)[1]
    clinic_code <- extract_code_from_df("clinic_", tracker_data)[1]

    return(list("country_code" = country_code, "clinic_code" = clinic_code))
}


extract_patient_data <- function(tracker_data, country_code, clinic_code) {
    i <- min(which(tracker_data[, 2] %like% paste0(country_code, "_", clinic_code)))
    j <- max(which(tracker_data[, 2] %like% paste0(country_code, "_", clinic_code)))
    patient_df <- data.frame(tracker_data[i:j, ])

    # view(patient_df)
    return(patient_df)
}


# function to extract the columns names in the tracker
extract_tracker_cols <- function(tracker_data, year) {
    i <- min(which(tracker_data[, 2] %like% "ID"))
    j <- min(which(tracker_data[, 2] %like% "ID"))
    tracker_cols <- as.vector(t(tracker_data[i:j, ]))

    if (year %in% c(2019, 2020, 2021)) {
        # take into account that date info gets separated from the updated values (not in the same row, usually in the bottom row)
        i <- i + 1
        j <- j + 1
        tracker_cols_date <- as.vector(t(tracker_data[i:j, ]))

        diff_colnames <- which(tracker_cols_date != tracker_cols)

        tracker_cols[diff_colnames] <- paste0(tracker_cols[diff_colnames], tracker_cols_date[diff_colnames])
    }

    # view(patient_df)
    return(tracker_cols)
}


# @Description: Imports the patient df, cleans it and matches it against
# column synonyms to unify column names
# @columns_synonyms: Long format output of read_column_synonyms to match columns
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


extract_date_from_measurement_column <- function(patient_df, colname) {
    # produces columns with names coherent with original naming before refactor
    colname_value <- paste(c(colname, ""), collapse = "")
    colname_core <- sub("[_][^_]+$", "", colname) # remove last element after "_"
    colname_date <- paste(c(colname_core, "_date"), collapse = "")
    patient_df <- separate_(
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
