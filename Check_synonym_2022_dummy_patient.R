

#' Select added_AN_sheet-from_tyla_DUMMY 2022 Tracker Template.xlsx
#' Set breakpoint before line 140

tracker_root_path <- select_A4D_directory()
tracker_file <- rstudioapi::selectFile(path = tracker_root_path, filter = "Excel Workbook (*.xlsx)")

tracker_file <- "D:/A4D/from_tyla/added_AN_sheet-from_tyla_DUMMY 2022 Tracker Template.xlsx"

codebook_path <- "4ADMonthlyTrackerCodebook.xlsx"

## Extract codebooks for each data form
codebook_patient <- read_column_synonyms(
    codebook_path, sheet = "synonyms_PatientData"
)

codebook_product <- read_column_synonyms(
    codebook_path, sheet = "synonyms_ProductData"
)

columns_synonyms = codebook_patient

df_raw <- reading_a4d_patient_data(
    tracker_data_file = tracker_file,
    columns_synonyms = codebook_patient
)
filename_output = df_raw[[2]]
df_raw_data = df_raw[[1]]
View(df_raw_data)

df_raw_data$bmi_date

# check harmonize ---------------------------------------------------------

harmonize_patient_data_columns <- function(patient_df, columns_synonyms) {
    patient_df <- patient_df %>% discard(~ all(is.na(.) | . == ""))
    patient_df <- patient_df[!is.na(names(patient_df))]



    colnames(patient_df) <- sanitize_column_name(colnames(patient_df))
    synonym_headers <- sanitize_column_name(columns_synonyms$name_to_be_matched)

    colnames(patient_df) %>% length()

    # replacing var codes
    colnames_found <- match(colnames(patient_df), synonym_headers, nomatch = 0)
    colnames(patient_df)[colnames(patient_df) %in% synonym_headers] <- columns_synonyms$name_clean[colnames_found]


    if (sum(colnames_found == 0) != 0) {
        "Non-matching column names found (see 0)"
        view(colnames_found)
    } else {
        return(patient_df)
    }
}
