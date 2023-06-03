# Read in data and codebook -----------------------------------------------

tracker_root_path <- select_A4D_directory()

# Only for single files, for now, for easier testing
# Loop will be implemented later
#tracker_file <-
#    rstudioapi::selectFile(path = tracker_root_path, filter = "Excel Workbook (*.xlsx)")
tracker_files <- list.files(tracker_root_path, "*.xlsx")
tracker_files <-
    tracker_files[str_detect(tracker_files, "~", negate = T)]

# codebook_path <- "master_tracker_variables.xlsx"# not needed anymore


## Extract codebooks for each data form
codebook_patient <-
    read_column_synonyms(synonym_file = "synonyms_patient.yaml")


codebook_product <-
    read_column_synonyms(synonym_file = "synonyms_product.yaml")


columns_synonyms = codebook_patient

output_root = file.path(tracker_root_path,
                        "output",
                        "sensitive_data_removed")

if (!file.exists(output_root)) {
    dir.create(output_root, recursive = TRUE)
}

for (tracker_file in tracker_files) {
    tracker_data_file <- file.path(tracker_root_path, tracker_file)
    cat("\n\nprocessing", tracker_file, fill=T)

    # Use new read function ---------------------------------------------------
    df_raw_patient <-
        reading_patient_data_2(tracker_data_file = tracker_data_file,
                               columns_synonyms = codebook_patient)


    # Combine to a new data frame ---------------------------------------------
    print(df_raw_patient %>% dim) # quickly check dimensions


    # INCOMPLETE - Set sensitive rows to NA -------------------------------------
    # level of education is in the patient list - we need to get data from there as well
    df_raw_patient <-
        df_raw_patient %>%
        mutate(across(
            any_of(c(
                "patient_name",
                "province",
                "dob",
                "country_code",
                "clinic_code",
                "gender",
                "edu_occ"
            )),
            ~NA
        )) %>%
        mutate(file_name=fs::path_ext_remove(tracker_file))

    df_raw_patient %>%
        write.csv(file =
                      file.path(
                          output_root,
                          paste0(tools::file_path_sans_ext(basename(tracker_file)), "_extracted.csv")
                      ),
                  row.names=F
                  )


    # product data extract
    df_raw_product <-
        reading_product_data_step1(tracker_data_file = tracker_data_file,
                               columns_synonyms = codebook_product)

    # product set sensitive column to NA and add tracker file name as a column
    df_raw_product <-
        df_raw_product %>%
        mutate(across(
            any_of(c(
                "product_released_to"
            )),
            ~NA
        )) %>%
        mutate(file_name=fs::path_ext_remove(tracker_file))

    # write output product extract
    df_raw_product %>%
        write.csv(file =
                      file.path(
                          output_root,
                          paste0(tools::file_path_sans_ext(basename(tracker_file)), "_extracted_product.csv")
                      ),
                  row.names=F
        )

}
