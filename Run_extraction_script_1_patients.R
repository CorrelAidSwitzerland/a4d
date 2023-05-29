# Read in data and codebook -----------------------------------------------

tracker_root_path <- select_A4D_directory()
tracker_file <- rstudioapi::selectFile(path = tracker_root_path, filter = "Excel Workbook (*.xlsx)")
# tracker_file <- "D:/A4D/from_tyla/added_AN_sheet-from_tyla_DUMMY 2022 Tracker Template.xlsx"


codebook_path <- "4ADMonthlyTrackerCodebook.xlsx"

## Extract codebooks for each data form
codebook_patient <- read_column_synonyms(
    codebook_path, sheet = "synonyms_PatientData"
)

codebook_product <- read_column_synonyms(
    codebook_path, sheet = "synonyms_ProductData"
)

columns_synonyms = codebook_patient




# adjust new harmonize function ---------------------------------------------------------
# adjusted harmonize function that has the same name as the original function
# Might need a better solution
harmonize_patient_data_columns <- function(patient_df, columns_synonyms) {
    # Uncommented because we want to retain columns with only NAs
    # patient_df <- patient_df %>% discard(~ all(is.na(.) | . == ""))
    patient_df <- patient_df[!is.na(names(patient_df))]

    colnames(patient_df) <- sanitize_column_name(colnames(patient_df))
    synonym_headers <- sanitize_column_name(columns_synonyms$name_to_be_matched)

    # replacing var codes
    colnames_found <- match(colnames(patient_df), synonym_headers, nomatch = 0)
    colnames(patient_df)[colnames(patient_df) %in% synonym_headers] <- columns_synonyms$name_clean[colnames_found]
    # browser()

    if (sum(colnames_found == 0) != 0) {
        "Non-matching column names found (see 0)"
        view(colnames_found)
    } else {
        return(patient_df)
    }
}



# check read patient data -------------------------------------------------
# note the different function name
# function is based on reading_a4d_patient_data() but shortened
reading_a4d_patient_data_2 <-

    function(tracker_data_file, columns_synonyms) {
        # list the sheets in excel workbook & filter these
        sheet_list <- readxl::excel_sheets(tracker_data_file)
# browser()
        # MONTHLY SHEETS: only select sheets with monthly data
        month_list <-
            sheet_list[na.omit(pmatch(month.abb, sheet_list))]

# browser()

        # Extract year
        year <- 2000 + unique(parse_number(month_list))
        print(year)




        tidy_tracker_list <- NULL

        sheet_num <- 1
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

            # view(tracker_data)


            patient_df <- extract_patient_data(tracker_data, country_code, clinic_code)
            print("patient df extracted")



            tracker_cols <- extract_tracker_cols(tracker_data, year)
            print("tracker_col names extracted")

            colnames(patient_df) <- tracker_cols
            print("tracker_col names added to patient df")

            patient_df <- harmonize_patient_data_columns(patient_df, columns_synonyms)
            print("finished harmonizing patient df")

            # removes duplicate columns that appear due to merged cells (e.g. insulin regimen)
            patient_df <- patient_df %>% distinct()
            # patient_df <- patient_df %>% select(unique(colnames(.))) # is this a good alternative?



# INCOMPLETE Load "Patient List" data and later merge it ------------------------------------------------------------
# The follownig section has to be improved from here on now to get patient info from Sheet "Patient List"
{
            # AN PATIENT DATA SHEET: select sheet in workbook with PATIENT AN DATA
            # if (any(grepl("Patient List", sheet_list))) {
            #     patient_sheet <- sheet_list[na.omit(grepl("Patient List", sheet_list))]
            #
            #     # AN PATIENT DATA DATA (merge/join at the end of the if year):
            #     an_patient_data <-
            #         data.frame(readxl::read_xlsx(tracker_data_file, patient_sheet))
            #     all_patient_ids <- an_patient_data$Patient.ID
            #
            #     an_patient_data <- clean_anon_data(an_patient_data)
            #     print("cleaned patient anon data")
            # } else {
            #     warning("File has no Patient List - Either fake data file or error")
            #     an_patient_data <- NA
            # }
            # print("patient AN Data extracted")
            # patient_df <-
            #     patient_df %>% left_join(an_patient_data, by = "id")
            # print("added patient anon data")

}


            patient_df <- patient_df %>%
                mutate(
                    sheet_name = CurrSheet,
                    tracker_mo = match(substr(CurrSheet, 1, 3), month.abb),
                    tracker_year = year,
                    country_code = country_code,
                    clinic_code = clinic_code
                )
            print("added tracker metadata")

            tidy_tracker_list[[sheet_num]] <- patient_df # %>%
            # mutate(across(everything(), as.character)) # all data is converted as characters otherwise many errors emerge

            sheet_num <- sheet_num + 1

        }

        return(tidy_tracker_list)
    }


# Use new read function ---------------------------------------------------
df_raw <- reading_a4d_patient_data_2(
    tracker_data_file = tracker_file,
    columns_synonyms = codebook_patient
)

# Combine to a new data frame ---------------------------------------------
df_raw_comb <- df_raw %>% bind_rows()
df_raw_comb %>% dim # quickly check dimensions


# INCOMPLETE - Set sensitive rows to NA -------------------------------------
# level of education is in the patient list - we need to get data from there as well
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
        )) %>% View



# INCOMPLETE - Extract data frame ------------------------------------------------------

# df_raw_comb <-
#     write.csv2()
