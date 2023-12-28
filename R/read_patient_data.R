reading_patient_data <-
    function(tracker_data_file, columns_synonyms) {
        ParallelLogger::logDebug("Start reading_patient_data.")
        sheet_list <- readxl::excel_sheets(tracker_data_file)
        testit::assert(length(sheet_list) > 0)
        ParallelLogger::logInfo(
            "Found ",
            length(sheet_list),
            " sheets inside the current file = ",
            paste(sheet_list, collapse = ","),
            "."
        )

        month_list <-
            sheet_list[na.omit(pmatch(month.abb, sheet_list))]
        ParallelLogger::logInfo(
            "Found ",
            length(month_list),
            " month sheets inside the current file = ",
            paste(month_list, collapse = ","),
            "."
        )
        testit::assert(length(month_list) > 0)

        # Extract year
        year <- get_tracker_year(tracker_data_file, month_list)
        ParallelLogger::logInfo("Tracker year = ", year, ".")
        testit::assert(year %in% c(2017, 2018, 2019, 2020, 2021, 2022))

        tidy_tracker_list <- NULL

        ParallelLogger::logDebug("Start processing sheets.")
        for (curr_sheet in month_list) {
            ParallelLogger::logDebug("Start processing sheet ", curr_sheet, ".")

            df_patient <- extract_patient_data(tracker_data_file, curr_sheet, year)
            testit::assert(nrow(df_patient) > 0)
            ParallelLogger::logDebug("df_patient dim: ", dim(df_patient) %>% as.data.frame(), ".")

            df_patient <-
                harmonize_patient_data_columns(df_patient, columns_synonyms)
            testit::assert("id" %in% colnames(df_patient))
            # -- if we have duplicate columns, merge them
            if (anyDuplicated(colnames(df_patient)) > 0) {
                duplicated_cols <- colnames(df_patient) %>%
                    table() %>%
                    dplyr::as_tibble() %>%
                    dplyr::filter(n > 1) %>%
                    dplyr::select(1) %>%
                    dplyr::pull()
                for (col in duplicated_cols) {
                    mask <- colnames(df_patient) == col
                    merged_col <- df_patient[mask] %>%
                        tidyr::unite(!!col, sep = ",")
                    df_patient <- df_patient[!mask]
                    df_patient <- df_patient %>%
                        tibble::add_column(!!col := dplyr::pull(merged_col), .name_repair = "minimal")
                }
            }

            df_patient <- df_patient %>%
                dplyr::mutate(
                    sheet_name = curr_sheet,
                    tracker_month = match(substr(curr_sheet, 1, 3), month.abb),
                    tracker_year = year
                )

            tidy_tracker_list[[curr_sheet]] <- df_patient
            ParallelLogger::logDebug("Finish processing sheet ", curr_sheet, ".")
        }

        ParallelLogger::logDebug("Start combining sheet data into single data frame.")
        df_raw <- dplyr::bind_rows(tidy_tracker_list)
        ParallelLogger::logDebug("Finish combining sheet data into single data frame.")

        if ("Patient List" %in% sheet_list) {
            ParallelLogger::logDebug("Start extracting patient list.")
            patient_list <- extract_patient_data(
                tracker_data_file,
                "Patient List",
                year
            )
            patient_list <- harmonize_patient_data_columns(
                patient_list,
                columns_synonyms
            )

            df_raw <- dplyr::left_join(
                df_raw,
                patient_list %>%
                    dplyr::select(-any_of(c(
                        "fbg_baseline_mg",
                        "fbg_baseline_mmol",
                        "hba1c_baseline",
                        "name",
                        "updated_2022_date"
                    ))),
                by = "id",
                relationship = "many-to-one"
            )
            ParallelLogger::logDebug("Finish extracting patient list.")
        }

        ParallelLogger::logInfo("Finish reading_patient_data.")
        return(df_raw)
    }
