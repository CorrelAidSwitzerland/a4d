options(readxl.show_progress = FALSE)

tracker_root <- "data"

main <- function() {
    # Define the file path
    tracker_files <- get_files(tracker_root)

    logInfo(
        "Found ",
        length(tracker_files),
        " xlsx files under ",
        tracker_root,
        "."
    )

    for (i in seq_along(tracker_files)) {
        tracker_file <- tracker_files[i]
        tracker_name <- tools::file_path_sans_ext(basename(tracker_file))
        tictoc::tic(paste("Processing tracker file:", tracker_name))
        tryCatch(
            replace_patient_names_in_tracker_file(tracker_file, tracker_name),
            error = function(e) {
                logError("Could not process ", tracker_name, ". Error = ", e, ".")
            }
        )
        tictoc::toc()
        cat(paste("Processed ", i, " of ", length(tracker_files), " (", round(i / length(tracker_files) * 100, 0), "%) tracker files.\n"))
    }
}

replace_patient_names_in_tracker_file <- function(tracker_file, tracker_name) {
    tracker_full_path <- file.path(tracker_root, tracker_file)
    wb <- openxlsx2::wb_load(tracker_full_path)

    sheet_names <- wb$sheet_names

    if (!"Patient List" %in% sheet_names) {
        logWarn("Sheet 'Patient List' not found in ", tracker_name, ". Skipping tracker file.")
        return()
    }

    # Define the old name and the new name
    patient_data <- readxl::read_excel(tracker_full_path, range = "Patient List!B8:C1000") %>% tidyr::drop_na()
    names(patient_data) <- patient_data[1, ]
    patient_data <- patient_data[-1, ]

    if (!"Patient Name" %in% names(patient_data)) {
        logWarn("Column C of sheet 'Patient List' has not the expected header 'Patient Name' in ", tracker_name, ". Skipping tracker file.")
        return()
    }

    # replace each patient name with patient id in all sheets in the workbook
    for (i in seq_along(patient_data[, 2])) {
        patient_id <- patient_data[i, 1] %>% dplyr::pull()
        patient_name <- patient_data[i, 2] %>% dplyr::pull()

        for (sheet in sheet_names) {
            openxlsx2::wb_set_active_sheet(wb, sheet)
            sheet_data <- openxlsx2::read_xlsx(
                wb,
                sheet,
                fill_merged_cells = FALSE,
                detect_dates = TRUE,
                keep_attributes = TRUE,
                convert = FALSE
            )
            sheet_data[sheet_data == patient_name] <- patient_id
            openxlsx2::writeData(wb, sheet, sheet_data)
        }
    }

    new_file_path <- file.path(tracker_root, dirname(tracker_file), paste0(tracker_name, "_replaced.xlsx"))
    openxlsx2::wb_save(wb, new_file_path, overwrite = TRUE)
}

main()

clearLoggers()
