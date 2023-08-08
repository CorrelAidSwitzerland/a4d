tracker_root_path <- select_A4D_directory()
input_root <- file.path(
    tracker_root_path,
    "output",
    "patient_data_raw"
)

output_root <- file.path(
    tracker_root_path,
    "output",
    "patient_static_data"
)

dir.create(output_root)
dir.create(file.path(output_root, "logs"))


addDefaultFileLogger(file.path(
    output_root, "logs",
    paste0("patient_data_static.log")
), "patient_data_static")

logInfo("Start creating static patient data.")


extracted_patient_files <-
    list.files(input_root, pattern = ".csv") %>%
    str_subset("patient")


# THERE MIGHT BE STATIC COLUMNS MISSING - PLEASE ADD THEM
static_patient_columns <-
    c(
        "patient_id",
        "patient_name",
        "province",
        "gender",
        "dob",
        "age",
        "t1d_diagnosis_age",
        "recruitment_date",
        "edu_occ",
        "tracker_month",
        "file_name",
        "tracker_year",
        "status",
        "last_clinic_visit_date",
        "baseline_hba1c",
        "baseline_fbg"
    )


static_patient_data_list <- list()

# get the latest static patient data for each tracker file
for (i in extracted_patient_files) {
    extracted_patient_file <- read.csv(file.path(input_root, i))
    tryCatch(
        {
            latest_static_patient_data <- extracted_patient_file %>%
                arrange(tracker_month) %>%
                select(any_of(static_patient_columns))

            # later we should not have this problem as this should be run AFTER cleaning the data
            # so later all columns of all CSV should have the same data type
            latest_static_patient_data <- latest_static_patient_data %>%
                mutate(
                    across(
                        everything(),
                        ~ as.character(.x)
                    )
                )

            static_patient_data_list[[i]] <- latest_static_patient_data
        },
        error = function(e) {
            logError("Could not process ", i, ". Error = ", e, ".")
        },
        warning = function(w) {
            logWarn("Could not process ", i, ". Warning = ", w, ".")
        }
    )
}


# Complete dataframe
static_patient_data_df <- static_patient_data_list %>%
    bind_rows()

# get latest static patient data overall
static_patient_data_df <- static_patient_data_df %>%
    group_by(patient_id, tracker_year) %>%
    slice_max(tracker_year) %>%
    arrange(desc(tracker_mo)) %>%
    slice_head(n = 1) %>%
    ungroup()

testit::assert(sum(duplicated(static_patient_data_df$patient_id)) == 0)


static_patient_data_df %>%
    write.csv(
        file =
            file.path(
                output_root,
                "patient_data_static.csv"
            ),
        row.names = F
    )

logInfo("Finish creating static patient data.")

clearLoggers()
