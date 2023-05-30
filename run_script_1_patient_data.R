# Read in data and codebook -----------------------------------------------

tracker_root_path <- select_A4D_directory()

# Only for single files, for now, for easier testing
# Loop will be implemented later
tracker_file <- rstudioapi::selectFile(path = tracker_root_path, filter = "Excel Workbook (*.xlsx)")


codebook_path <- "master_tracker_variables.xlsx"

## Extract codebooks for each data form
codebook_patient <- read_column_synonyms(
    codebook_path, sheet = "synonyms_PatientData"
)

codebook_product <- read_column_synonyms(
    codebook_path, sheet = "synonyms_ProductData"
)

columns_synonyms = codebook_patient


# Use new read function ---------------------------------------------------
df_raw <- reading_patient_data_2(
    tracker_data_file = tracker_file,
    columns_synonyms = codebook_patient
)


# Combine to a new data frame ---------------------------------------------
df_raw %>% dim # quickly check dimensions


# INCOMPLETE - Set sensitive rows to NA -------------------------------------
# level of education is in the patient list - we need to get data from there as well
df_raw_comb <-
    df_raw_comb %>%
    mutate(
        across(
            c(
            patient_name,
            country_code,
            clinic_code,
            # etc.
            ),
            ~NA
        ))


# INCOMPLETE - Store data ------------------------------------------------------

extracted_root <-  file.path(
    tracker_root_path,
    "Extracted files"
)

if (!file.exists(extracted_root)) {
    dir.create(extracted_root)
}


df_raw_comb %>%
    write.csv(file =
                  paste0(extracted_root,
                         "/",
                         basename(tracker_file),
                         "_extracted.csv"),
              row.names = F)
