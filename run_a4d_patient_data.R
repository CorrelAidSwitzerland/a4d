## Main script to process a tracker file

# Tracker path
tracker_root_path <- select_A4D_directory()
# tracker_file <- rstudioapi::selectFile(path = tracker_root_path, filter = "Excel Workbook (*.xlsx)")
tracker_files <- list.files(tracker_root_path, "*.xlsx")
tracker_files <- tracker_files[!str_detect("~", tracker_files)]

codebook_path <- "4ADMonthlyTrackerCodebook.xlsx"

## Extract codebooks for each data form
codebook_patient <- read_column_synonyms(
    codebook_path, sheet = "synonyms_PatientData"
)

codebook_product <- read_column_synonyms(
    codebook_path, sheet = "synonyms_ProductData"
)

columns_synonyms = codebook_patient

output_root = file.path(
    tracker_root_path,
    "output",
    gert::git_branch(),
    substr(gert::git_commit_id(), 1, 8)
)

if (!file.exists(output_root)) {
    dir.create(output_root, recursive = TRUE)
}

for (tracker_file_name in tracker_files) {
    # if you have selected a single tracker file, this loop will skip all other files!
    # so comment line 5 out if you want all files processed.
    if (exists("tracker_file") && basename(tracker_file) != tracker_file_name) next

    tracker_file_path <- file.path(tracker_root_path, tracker_file_name)

    ### Data extraction
    df_raw <- reading_a4d_patient_data(
      tracker_data_file = tracker_file_path,
      columns_synonyms = codebook_patient
      )
    filename_output = df_raw[[2]]
    df_raw_data = df_raw[[1]]
    #View(df_raw_data)

    save(
        df_raw_data,
        file=file.path(
            tracker_root_path,
            "output",
            gert::git_branch(),
            substr(gert::git_commit_id(), 1, 8),
            paste0(tools::file_path_sans_ext(tracker_file_name), "_raw", ".RData")
        )
    )

    ### Data cleanse
    df_cleaned <- clean_tracker_raw_patient_data(data = df_raw_data)
    #View(df_cleaned)

    save(
        df_cleaned,
        file=file.path(
            tracker_root_path,
            "output",
            gert::git_branch(),
            substr(gert::git_commit_id(), 1, 8),
            paste0(tools::file_path_sans_ext(tracker_file_name), "_cleaned", ".RData")
        )
    )

}
