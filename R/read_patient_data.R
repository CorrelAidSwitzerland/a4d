# "reading_a4d_tracker" is a function that reads an excel workbook (.xlsx file) that contains a4d monthly trackers and creates a tidy dataframe with that data.
# It takes into account all the sheets in the excel workbook that contain patient data, and binds these together. (e.g., if a workbook contains sheets Jan'18, Feb'18
# and Mar'18, patient data from each sheet will be combined into one data frame)
#
# INPUT ARGUMENTS: -
# tracker_data_file = full path of the excel workbook that contains a4d monthly trackers (format: string e.g., "users/Documents/file.xlsx")
# year = year of tracker data (format: numeric. e.g., 2018)
# country = country code (format: character string e.g., "xyz")
# clinic = clinic code (format: character string e.g., "xyz")
#
# example: reading_a4d_tracker(tracker_data_file = "~/Desktop/2017 Tracker Template.xlsx",
# year = 2017,
# clinic = "ABC",
# country = "DEF")
#
# FUNCTION OUTPUT:
# df: "tidy" dataframe with patient data with the following columns (format: character. This is to facilitate merging of dataframes)
# [1] "no"
# [2] "patient_name"
# [3] "province"
# [4] "gender"
# [5] "dob"
# [6] "age"
# [7] "age_diagnosis"
# [8] "recruitment_date"
# [9] "baseline_hba1c_prc"
# [10] "updated_hba1c_prc"
# [11] "updated_hba1c_date"
# [12] "baseline_fbg_mldl"
# [13] "updated_fbg_mldl"
# [14] "updated_fbg_date"
# [15] "support_from_a4d"
# [16] "testing_fqr"
# [17] "est_strips_pmoth"
# [18] "status"
# [19] "updated_fbg_sample"
# [20] "tracker_year"
# [21] "clinic_code"
# [22] "country_code"
# [23] "sheet_name"
# [24] "insulin_regimen"
# [25] "blood_pressure_sys_mmhg"
# [26] "blood_pressure_dias_mmhg"
# [27] "weight"
# [28] "height"
# [29] "bmi"
# [30] "bmi_date"
# [31] "edu_occ"
# [32] "hospitalisation"
# [33] "last_clinic_visit_date"
# [34] "additional_support"
# [35] "id"
# [36] "latest_complication_screening_type"
# [37] "latest_complication_screening_date"
# [38] "remarks"


# FUNCTION TO READ THE A4D MONTHLY TRACKER --> PATIENT DATA --------------------------------------------------------
read_patient_data <-
    function(tracker_data_file, columns_synonyms) {
        sheet_list <- readxl::excel_sheets(tracker_data_file)

        # MONTHLY SHEETS: only select sheets with monthly data
        month_list <-
            sheet_list[na.omit(pmatch(month.abb, sheet_list))]

        # AN PATIENT DATA SHEET: select sheet in workbook with PATIENT AN DATA
        an_patient_data <-
            read_patient_an_data(tracker_data_file, sheet_list, columns_synonyms)

        # Extract year
        year <- 2000 + unique(parse_number(month_list))
        print(year)

        tidy_tracker_list <- NULL

        for (CurrSheet in month_list) {
            print(CurrSheet)

            tracker_data <-
                as.data.frame(
                    openxlsx::read.xlsx(
                        xlsxFile = tracker_data_file,
                        fillMergedCells = TRUE,
                        sheet = CurrSheet
                    )
                )
            print("tracker read in")

            cc_codes <-
                extract_country_clinic_code(tracker_data)
            country_code <- cc_codes$country_code
            clinic_code <- cc_codes$clinic_code

            patient_df <-
                extract_patient_data(tracker_data)
            tracker_cols <-
                extract_patient_data_header(tracker_data, year)
            colnames(patient_df) <- tracker_cols
            patient_df <-
                harmonize_patient_data_columns(patient_df, columns_synonyms)
            print("patient df extracted")

            #### 2017 + 2018 ####
            if (year == 2017 | year == 2018) {
                format <- "%Y/%m/%d"

                # fix dates (split dates in cells)
                patient_df <-
                    extract_date_from_measurement_column(patient_df, "updated_hba1c_prc")
                patient_df$updated_hba1c_date <-
                    transform_MM_DD_to_YYYY_MM_DD_str(patient_df$updated_hba1c_date, year)
                patient_df$updated_hba1c_date <- as.Date(
                    patient_df$updated_hba1c_date,
                    format = format,
                    origin = "1899-12-30",
                    tz = "GMT"
                )

                patient_df <-
                    extract_date_from_measurement_column(patient_df, "updated_fbg_mgdl")
                patient_df$updated_fbg_date <-
                    transform_MM_DD_to_YYYY_MM_DD_str(patient_df$updated_fbg_date, year)
                patient_df$updated_fbg_date <- as.Date(
                    patient_df$updated_fbg_date,
                    format = format,
                    origin = "1899-12-30",
                    tz = "GMT"
                )
                print("date extracted from compound cols")

                if ("recruitment_date" %in% colnames(patient_df)) {
                    patient_df <- patient_df %>%
                        dplyr::mutate(recruitment_date = openxlsx::convertToDate(as.numeric(
                            patient_df$recruitment_date
                        )))
                }
            }

            patient_df <- bmi_fix(patient_df)
            patient_df <- date_fix(patient_df, year)

            if ("blood_pressure_mmhg" %in% colnames(patient_df)) {
                patient_df <- bp_fix(patient_df)
            }
            print("patient cleaning done")

            patient_df <-
                patient_df %>% dplyr::left_join(an_patient_data, by = "id")
            print("added patient anon data")


            patient_df <- patient_df %>%
                dplyr::mutate(
                    sheet_name = CurrSheet,
                    tracker_mo = match(substr(CurrSheet, 1, 3), month.abb),
                    tracker_year = year,
                    country_code = country_code,
                    clinic_code = clinic_code
                )
            print("added tracker metadata")

            #### Save data ####
            # save data in a list
            tidy_tracker_list[[CurrSheet]] <- patient_df # %>%
            # mutate(across(everything(), as.character)) # all data is converted as characters otherwise many errors emerge
        } # sheet for loop


        # standard df, consistent for all tracker years
        standard_df <- tibble::tibble(
            patient_name = character(),
            province = character(),
            gender = character(),
            dob = character(),
            age = character(),
            age_diagnosis = character(),
            recruitment_date = date(),
            baseline_hba1c_prc = character(),
            updated_hba1c_prc = character(),
            updated_hba1c_date = character(),
            baseline_fbg_mgdl = character(),
            updated_fbg_mgdl = character(),
            updated_fbg_date = character(),
            support_from_a4d = character(),
            testing_fqr = character(),
            est_strips_pmoth = character(),
            status = character(),
            updated_fbg_sample = character(),
            tracker_year = character(),
            clinic_code = character(),
            country_code = character(),
            sheet_name = character(),
            insulin_regimen = character(),
            blood_pressure_sys_mmhg = character(),
            blood_pressure_dias_mmhg = character(),
            weight = character(),
            height = character(),
            bmi = character(),
            bmi_date = character(),
            edu_occ = character(),
            hospitalisation = character(),
            last_clinic_visit_date = character(),
            additional_support = character(),
            id = character(),
            latest_complication_screening_type = character(),
            latest_complication_screening_date = character(),
            remarks = character(),
            dm_complication_comment = character(),
            dm_complication_other = character(),
            dm_complication_kidney = character(),
            dm_complication_eye = character(),
            num_admin_hosp_total = character(),
            num_admin_hosp_dka = character(),
            num_admin_hosp_hypo = character(),
            num_admin_hosp_other_reason = character(),
            num_admin_hosp_other = character(),
            inactive_reason = character(),
            lost_date = character(),
            lost_age = character(),
            diag_date = character(),
            dka_diag = character()
        )

        df <- bind_rows(tidy_tracker_list)
        if ("testing_fqr_pday" %in% colnames(df)) {
            df$testing_fqr <- df$testing_fqr_pday
            df <- df %>% select(-testing_fqr_pday)
        }

        # cols_in_df <- colnames(df) %in% colnames(standard_df)
        # df <- df[cols_in_df]

        # Add empty missing columns
        cols_missing <-
            colnames(standard_df)[!colnames(standard_df) %in% colnames(df)]
        df[cols_missing] <- NA

        filename <-
            paste0(
                "tracker_",
                unique(df$country_code),
                "_",
                unique(df$clinic_code),
                "_",
                unique(df$tracker_year)
            )
        tracker_info <- list(df, filename)


        # tracker_info <- list(df)

        return(tracker_info)
    }

# check read patient data -------------------------------------------------
# note the different function name
# function is based on reading_a4d_patient_data() but shortened
reading_patient_data_2 <-
    function(tracker_data_file, columns_synonyms) {
        # list the sheets in excel workbook & filter these
        sheet_list <- readxl::excel_sheets(tracker_data_file)
        log_info("Found {length(sheet_list)} sheets inside the current file = {sheet_list}.")

        # MONTHLY SHEETS: only select sheets with monthly data
        month_list <-
            sheet_list[na.omit(pmatch(month.abb, sheet_list))]
        log_info("Found {length(month_list)} month sheets inside the current file = {month_list}.")

        # Extract year
        year <- 2000 + unique(parse_number(month_list))
        if (is.na(year)) {
            year <- as.integer(str_match(tracker_data_file, "[:digit:]{4}"))
        }
        log_info("Tracker year = {year}.")

        tidy_tracker_list <- NULL

        log_info("Start processing month sheets.")
        for (curr_sheet in month_list) {
            cat("\n")
            print(curr_sheet)

            patient_df <- extract_patient_data(tracker_data_file, curr_sheet, year)
            print("patient df extracted")

            patient_df <-
                harmonize_patient_data_columns_2(patient_df, columns_synonyms)
            print("patient df harmonized")

            cc_codes <- extract_country_clinic_code(patient_df)
            country_code <- cc_codes$country_code
            clinic_code <- cc_codes$clinic_code

            patient_df <- patient_df %>%
                dplyr::mutate(
                    sheet_name = curr_sheet,
                    tracker_mo = match(substr(curr_sheet, 1, 3), month.abb),
                    tracker_year = year,
                    country_code = country_code,
                    clinic_code = clinic_code
                )
            print("added tracker metadata")

            tidy_tracker_list[[curr_sheet]] <- patient_df # %>%
            # mutate(across(everything(), as.character)) # all data is converted as characters otherwise many errors emerge
        }

        df_raw <- dplyr::bind_rows(tidy_tracker_list)

        if ("Patient List" %in% sheet_list) {
            patient_list <- extract_patient_data(
                tracker_data_file,
                "Patient List",
                year
            )
            patient_list <- harmonize_patient_data_columns_2(
                patient_list,
                columns_synonyms
            )

            message("check with A4D whether duplicate columns in patient list sheet should be removed")
            df_raw <- dplyr::left_join(
                df_raw,
                patient_list %>%
                    dplyr::select(-c(
                        baseline_fbg,
                        baseline_hba1c,
                        patient_name,
                        updated_2022_date
                    )),
                by = "id"
            )
        }

        return(df_raw)
    }
