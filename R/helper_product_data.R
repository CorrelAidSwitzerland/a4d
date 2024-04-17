#### Define functions #####

`%notin%` <- Negate(`%in%`)

#' @title set_second_row_as_headers_and_remove_first_row
#'
#' @description
#' Clean df where first row is empty
set_second_row_as_headers_and_remove_first_row <- function(df) {
    colnames(df) <- df[1, ]
    df <- df[-1, ]
    return(df)
}

#### 1. Identify xlsx cutoff ####
# ==============================================================================

# Test whether both helper functions work for all years
# and if word searches need to be done for years separately
# Conditions to cut of at start msd data
helper_is_msd_start_row <- function(df, i) {
    condition <- dplyr::case_when(

        # 2021
        # any(grepl("MEDICAL", df[i, ])) &
        # any(grepl("Product", df[i + 1, ])) &
        # any(grepl("Entry Date", df[i + 1, ])) ~ TRUE,

        # 2024
        any(grepl("Product", df[i, ])) &
            any(grepl("Date", df[i, ])) &
            any(grepl("Received", df[i, ])) ~ TRUE,

        # 2019 & 2020 & 2021
        any(grepl("Product", df[i, ])) &
            any(grepl("Date", df[i, ])) &
            any(grepl("Units Received|Units \r\nReceived", df[i, ])) ~ TRUE,

        # 2017 & 2018
        any(grepl("Description of Support", df[i, ])) &
            any(grepl("Date", df[i, ])) &
            any(grepl("Units Received", df[i, ])) ~ TRUE,
        TRUE ~ FALSE
    )
}

# Conditions to cut of at start patient data
helper_is_msd_end_row <- function(df, i) {
    includes_patient_name <- any(grepl("Patient Name", df[i + 1, ]))
    includes_patient_recruitment <- any(grepl("patient recruitment", tolower(df[i, ])) |
        grepl("patient data summary", tolower(df[i - 1, ]))) # second check for 2022 data onwards

    condition <- dplyr::case_when(
        !includes_patient_name ~ FALSE,
        !includes_patient_recruitment ~ FALSE,
        !(any(grepl("patient id", tolower(df[i + 1, ]))) | any(grepl("ID", df[i + 1, ]))) ~ FALSE,
        # !any(grepl("No", df[i + 1, ])) ~ FALSE,
        # !any(grepl("ID", df[i + 1,])) ~ FALSE,
        TRUE ~ TRUE
    )

    return(condition)
}


get_msd_start <- function(df, j) {
    if (helper_is_msd_start_row(df, j) == TRUE) {
        start_df_msd <- j + 1
        return(start_df_msd)
    }
}

get_msd_end <- function(df, j) {
    if (helper_is_msd_end_row(df, j)) {
        end_df_msd <- j - 1
        return(end_df_msd)
    }
}

get_patient_start <- function(df, j) {
    if (helper_is_msd_end_row(df, j)) {
        start_df_patient <- j + 1
    }
}

get_patient_end <- function(df, j) {
    if (helper_is_msd_end_row(df, j)) {
        end_df_patient <- nrow(df)
    }
}

#' @title extract_product_data
#'
#' @description
#' Reads product data from a monthly file based on extraction logic
extract_product_data <- function(monthly_tracker_df) {
    start_df_msd <- NULL
    end_df_msd <- NULL

    for (i in 1:nrow(monthly_tracker_df)) {
        start <- get_msd_start(monthly_tracker_df, i)
        end <- get_msd_end(monthly_tracker_df, i)

        if (!purrr::is_empty(start) & purrr::is_empty(start_df_msd)) {
            start_df_msd <- start - 1
        }
        if (!purrr::is_empty(end) & purrr::is_empty(end_df_msd)) {
            end_df_msd <- end
        }
    }

    if (is.null(start_df_msd)) {
        logWarn(
            log_to_json(
                message = "Cannot find the initial row for the product data.",
                file = "helper_product_data.R",
                functionName = "extract_product_data",
                warningCode = "script1_warning_read_product_data"
            )
        )
    }

    if (is.null(end_df_msd)) {
        logWarn(
            log_to_json(
                message = "Cannot find the final row for the product data.",
                file = "helper_product_data.R",
                functionName = "extract_product_data",
                warningCode = "script1_warning_read_product_data"
            )
        )
    }

    product_data_df <- monthly_tracker_df[start_df_msd:end_df_msd, ]

    # Clean empty remaining first row
    product_data_df <- set_second_row_as_headers_and_remove_first_row(product_data_df)

    return(product_data_df)
}

#### 2. Match Product data columns ####
# ==============================================================================

#' @title harmonize_input_data_columns
#'
#' @description
#' Imports the product df, cleans it and matches it against
# column synonyms to unify column names
#
#' @param product_df: The product data to be cleaned and matched against column synonyms
#' @param columns_synonyms: Long format output of read_column_synonyms to match columns
#' @param sheet: The current sheet name of the tracker file
harmonize_input_data_columns <- function(product_df, columns_synonyms, sheet) {
    # In case that there is additional data in strange columns, keep only relevant columns
    # keep.cols <- names(product_df) %in% c("")

    product_df <- product_df %>% purrr::discard(~ all(is.na(.) | . == ""))
    product_df <- product_df[!is.na(names(product_df))]

    colnames(product_df) <- sanitize_column_name(colnames(product_df))
    synonym_headers <- sanitize_column_name(columns_synonyms$name_to_be_matched)

    ## report all column names which have not been found
    unknown_column_names <- colnames(product_df)[!colnames(product_df) %in% synonym_headers]
    if (length(unknown_column_names) > 0) {
        logWarn(
            log_to_json(
                message = "Sheet {values['sheet']}: Unknown column names: {values['col_names']}.",
                values = list(sheet = sheet, col_names = unknown_column_names),
                file = "helper_product_data.R",
                functionName = "harmonize_input_data_columns",
                warningCode = "script1_warning_read_product_data"
            )
        )
    }

    # replacing var codes
    colnames_found <- match(colnames(product_df), synonym_headers, nomatch = 0)
    colnames(product_df)[colnames(product_df) %in% synonym_headers] <- columns_synonyms$name_clean[colnames_found]

    if (sum(colnames_found == 0) != 0) {
        # SK: remove non matching column names
        relcols <- which(colnames(product_df) %in% columns_synonyms$name_clean)
        product_df <- product_df[, relcols]
    }

    return(product_df)
}

#' @title read_column_synonyms_product
#'
#' @description
#' Imports the codebook, cleans, removes duplicates and transforms it
#' into long df format
read_column_synonyms_product <- function(codebook_data_file) {
    columns_synonyms <- codebook_data_file %>%
        readxl::read_xlsx(sheet = "synonyms_ProductData") %>%
        dplyr::as_tibble() %>%
        tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "name_clean",
            values_to = "name_to_be_matched"
        ) %>%
        # subset(!is.na(name_to_be_matched)) %>% # commented out by SK, as this should also include final column names
        # lapply(sanitize_column_name) %>%
        dplyr::as_tibble() %>%
        dplyr::group_by(name_to_be_matched) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()

    return(columns_synonyms)
}

sanitize_column_name <- function(column_name) {
    column_name <- column_name %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all(stringr::fixed(" "), "") %>%
        stringr::str_replace_all("[^[:alnum:]]", "")

    column_name_clean <- column_name

    return(column_name_clean)
}


#### 3. Change Product data Date format and standardize columns ####

# ==============================================================================
#' @title format_date_excelnum
#'
#' @description
#' Reformates dates entered into excel in wrong format (e.g., 44042) to final date format (yyyy-mm-dd)
format_date_excelnum <- function(product_df) {
    rel_rows <- which(!grepl("-", product_df$product_entry_date) & !grepl("\\.", product_df$product_entry_date))
    product_df[rel_rows, "product_entry_date"] <- suppressWarnings(as.character(openxlsx::convertToDate(as.numeric(unlist(product_df[rel_rows, "product_entry_date"])))))

    return(product_df)
}

#' @title format_date_exceldate
#'
#' @description
#' Reformates dates entered into excel in wrong format (e.g., dd-mm-yyyy) to final date format (yyyy-mm-dd)
format_date_exceldate <- function(product_df) {
    rel_rows <- which(grepl("-", product_df$product_entry_date) | grepl("\\.", product_df$product_entry_date))
    product_df[rel_rows, "product_entry_date"] <- suppressWarnings(as.character(lubridate::dmy(unlist(product_df[rel_rows, "product_entry_date"]))))

    return(product_df)
}

#' @title format_date
#'
#' @description
#' Combines all date reformatting functions into one overall function
format_date <- function(product_df) {
    product_df <- format_date_exceldate(product_df)
    product_df <- format_date_excelnum(product_df)

    return(product_df)
}

#' @title extract_month
#'
#' @description
#' Extract month as digit (e.g., 12 for december) based on excel tab name
extract_month <- function(sheetname) {
    output <- dplyr::case_when(
        grepl("Jan", sheetname) ~ "01",
        grepl("Feb", sheetname) ~ "02",
        grepl("Mar", sheetname) ~ "03",
        grepl("Apr", sheetname) ~ "04",
        grepl("May", sheetname) ~ "05",
        grepl("Jun", sheetname) ~ "06",
        grepl("Jul", sheetname) ~ "07",
        grepl("Aug", sheetname) ~ "08",
        grepl("Sep", sheetname) ~ "09",
        grepl("Oct", sheetname) ~ "10",
        grepl("Nov", sheetname) ~ "11",
        grepl("Dec", sheetname) ~ "12"
    )

    return(output)
}

#' @title recode_unitcolumnstozero
#'
#' @description
#' Recode NAs to 0 in all "unit columns"
recode_unitcolumnstozero <- function(product_df) {
    product_df <- product_df %>%
        dplyr::mutate_at(dplyr::vars(c("product_units_received", "product_units_released", "product_units_returned")), ~ replace(., is.na(.), 0))

    return(product_df)
}

#' @title clean_unitsreceived
#'
#' @description
#' Clean column "product_units_received" from character values and rows with values but no further information
clean_unitsreceived <- function(product_df) {
    # Clean column "product_units_received from" from character values
    drop_rows <- product_df %>%
        dplyr::filter(grepl("START|END|BALANCE", product_units_received, ignore.case = TRUE)) %>%
        dplyr::select(index) %>%
        unlist() %>%
        as.numeric()
    product_df <- product_df %>%
        dplyr::mutate(product_units_received = ifelse(index %in% drop_rows,
            0,
            suppressWarnings(as.numeric(product_units_received))
        ))

    return(product_df)
}


#' @title Transfers in trackers with specific format which have "START BALANCE" in product data the start balance to the correct column.
#'
#' @description
#' Run before clean_receivedfrom and recode_unitcolumnstozero. Necessary for specific format such as in 2019_PKH and 2020_STH. When start balance in tracker next to text "start balance" one need to extract the balance either from the "received from" column or the "units released" column.
#'
#' @param product_df Input product data.
#'
#' @return Dataframe with corrected product_received_from column extracting the starting balance from trackers where necessary.
update_receivedfrom <- function(product_df) {
    if (any(grepl("Balance", product_df[["product_units_received"]], ignore.case = TRUE)) & any(is.na(product_df$product_received_from))) {
        product_df <- product_df %>%
            dplyr::mutate(product_received_from = dplyr::case_when(
                grepl("Balance", product_units_received, ignore.case = TRUE) & !is.na(product_units_released) ~ product_units_released,
                grepl("Balance", product_units_received, ignore.case = TRUE) & !is.na(product_received_from) ~ product_received_from
            )) %>%
            dplyr::mutate(product_units_released = ifelse(!is.na(product_received_from), NA, product_units_released))

        logInfo(
            log_to_json(
                message = "The rule for the case was applied successfully- Released (product_units_released) column also includes values for Start/End Balance",
                file = "helper_product_data.R",
                functionName = "update_receivedfrom"
            )
        )
    }

    return(product_df)
}

#' @title clean_receivedfrom
#'
#' @description
#' Clean column "product_received_from" from character values
clean_receivedfrom <- function(product_df) {
    # If there is notion of a balance trackkeeping in received column, which would align with a "START" statement in product_received column:
    if (any(grepl("START", product_df[["product_units_received"]], ignore.case = TRUE))) {
        # Copy balance start values from this column to product_balance column. Balance end values are not kept since these seem to represent final accounting values.
        product_df <- product_df %>%
            dplyr::mutate(product_balance = dplyr::case_when(
                grepl("START", product_units_received, ignore.case = TRUE) ~ product_received_from
            ))
    }

    # Clean column
    drop_rows <- product_df %>%
        dplyr::filter(!grepl("[[:alpha:]]", product_received_from) & is.na(product_received_from) == FALSE) %>%
        dplyr::select(index) %>%
        unlist() %>%
        as.numeric()
    product_df <- product_df %>%
        dplyr::mutate(product_received_from = ifelse(index %in% drop_rows,
            NA,
            product_received_from
        ))

    return(product_df)
}

#' @title compute_balance_cleanrows
#'
#' @description
#' Preparation function for compute_balance and compute_balance_status.
#'
#' Remove rows without any meaningful information regarding units (release, return, received) or entry status
compute_balance_cleanrows <- function(product_df) {
    delete_rows <- as.vector(0)
    for (i in 1:nrow(product_df)) {
        if ((is.na(product_df$product_units_received[i]) == TRUE) & (is.na(product_df$product_units_released[i]) == TRUE) & (is.na(product_df$product_units_returned[i]) == TRUE) &
            (is.na(product_df$product_released_to[i]) == TRUE) & (is.na(product_df$product_entry_date[i]) == TRUE) & (is.na(product_df$product_balance[i]) == TRUE)) {
            delete_rows[length(delete_rows) + 1] <- i
        }
    }

    if (delete_rows > 0) {
        product_df <- product_df[-delete_rows, ]
    }

    return(product_df)
}

#' @title compute_balance_status
#'
#' @description
#' Computate balance status (start vs. change vs. end) per product
compute_balance_status <- function(product_df) {
    # Define change as default
    indices_start <- product_df %>%
        dplyr::group_by(product) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::as_tibble() %>%
        dplyr::select(index) %>%
        unlist() %>%
        as.numeric()

    indices_end <- product_df %>%
        dplyr::group_by(product) %>%
        dplyr::slice_tail(n = 1) %>%
        dplyr::as_tibble() %>%
        dplyr::select(index) %>%
        unlist() %>%
        as.numeric()

    product_df <- product_df %>%
        dplyr::mutate(product_balance_status = dplyr::case_when(
            index %in% indices_start ~ "start",
            index %in% indices_end ~ "end",
            TRUE ~ "change"
        ))

    return(product_df)
}

#' @title compute_balance
#'
#' @description
#' Computes balance based on start balance value, units released, units returned, units received.-
compute_balance <- function(product_df, year) {
    # Change variable type
    product_df["product_balance"] <- as.numeric(unlist(product_df["product_balance"]))
    product_df["product_units_released"] <- as.numeric(unlist(product_df["product_units_released"]))
    product_df["product_units_returned"] <- as.numeric(unlist(product_df["product_units_returned"]))

    # Need to put units received and released to 0 since here only summarising end of month
    if (year >= 2021) {
        product_df <- product_df %>%
            dplyr::mutate(product_units_received = dplyr::if_else(product_balance_status == "end", 0, product_units_received)) %>%
            dplyr::mutate(product_units_released = dplyr::if_else(product_balance_status == "end", 0, product_units_released))
    }

    # Calculate start balance: take units_received value if there is no balance value present
    product_df <- product_df %>%
        dplyr::group_by(product) %>%
        dplyr::mutate(
            product_balance = dplyr::case_when(
                (product_balance_status == "start") & (is.na(product_balance) == TRUE) ~ (product_units_received - product_units_released),
                TRUE ~ as.numeric(product_balance)
            )
        )

    # Calculate change and end of balance values: take last balance + received - release
    for (i in 1:nrow(product_df)) {
        if (product_df$product_balance_status[i] %in% c("change", "end")) {
            release <- NA
            reception <- NA
            if (is.na(as.numeric(product_df[i, "product_units_released"])) == TRUE) {
                release <- as.numeric(0)
            } else {
                release <- as.numeric(product_df[i, "product_units_released"])
            }
            if (is.na(as.numeric(product_df[i, "product_units_received"])) == TRUE) {
                reception <- as.numeric(0)
            } else {
                reception <- as.numeric(product_df[i, "product_units_received"])
            }
            product_df[i, "product_balance"] <- as.numeric(product_df[i - 1, "product_balance"]) - release + reception
        }
    }

    # ToDo LOW (KW): Ungroup statement can possibly be moved a further up limiting
    # the group scope for clarity. I added the ungroup as it was missing from the
    # function, but did not look for the best position so far.
    return(product_df %>% dplyr::ungroup())
}

#' @title adjust_column_classes
#'
#' @description
#' Adjust classes of dataframe names
adjust_column_classes <- function(product_df) {
    list_date <- c("product_entry_date")
    list_character <- c(
        "product", "product_received_from", "product_released_to", "product_returned_by", "product_balance_status",
        "product_sheet_name", "file_name", "product_units_notes"
    )
    list_numeric <- c(
        "product_units_received", "product_units_released", "product_balance", "product_units_returned",
        "product_table_month", "product_table_year", "index"
    )

    product_df <- product_df %>%
        dplyr::mutate_at(list_date, as.Date) %>%
        dplyr::mutate_at(list_character, as.character) %>%
        dplyr::mutate_at(list_numeric, as.numeric)

    return(product_df)
}

#' @title testit
#'
#' @description
#' Wait for x seconds in script
testit <- function(x) {
    p1 <- proc.time()
    Sys.sleep(x)
    proc.time() - p1
}


#### 4. Extract concatenated product name information ####

#' @title extract_product_multiple
#'
#' @description
#' Splits product name cells with multiple values into separate rows. Uses numeric product content in cell for columns units_received or units_released
extract_product_multiple <- function(product_df) {
    # Split multiple-product cell into several rows
    df_product_test_x <- product_df %>%
        tidyr::separate_rows(., product, convert = TRUE, sep = "\\; ") %>%
        tidyr::separate_rows(., product, convert = TRUE, sep = " and ")


    # Extract product unit information from product cell and add to release or received cell, depending on entries in the corresponding column
    df_product_test_x$product_units_notes <- as.character(NA)

    # If units columns not present yet, add
    if ("product_units_received" %notin% colnames(df_product_test_x)) {
        df_product_test_x$product_units_received <- as.numeric(NA)
    } # else{
    # df_product_test_x$product_units_received <- as.numeric(df_product_test_x$product_units_received)}
    if ("product_units_released" %notin% colnames(df_product_test_x)) {
        df_product_test_x$product_units_released <- as.numeric(NA)
    } # else{
    # df_product_test_x$product_units_released <- as.numeric(df_product_test_x$product_units_released)}

    # Create column "notes", containing input from brackets if word "box" or "unit" is present
    df_product_test_x <- df_product_test_x %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            product_units_notes = dplyr::case_when( # e.g., "1 box" or "1 unit". Other units (e.g., 2ml x5) are not extracted.
                (grepl("\\(", product) & grepl("\\)", product) & (grepl("box", product) | grepl("unit", product))) ~ as.character(substring(stringr::str_extract_all(product, "\\([^()]+\\)")[[1]], 2, nchar(stringr::str_extract_all(product, "\\([^()]+\\)")[[1]]) - 1))[1],
                TRUE ~ product_units_notes
            )
        )

    # Adjust columns units_receceived and units_released with box/unit content in brackets
    for (row in 1:nrow(df_product_test_x)) {
        product <- df_product_test_x[row, "product"]
        if ("product_received_from" %in% colnames(df_product_test_x)) {
            product_received_from <- df_product_test_x[row, "product_received_from"]
        } else {
            product_received_from <- NA
        }
        if ("product_released_to" %in% colnames(df_product_test_x)) {
            product_released_to <- df_product_test_x[row, "product_released_to"]
        } else {
            product_released_to <- NA
        }

        # recode product_units_received
        if ((grepl("\\(", product) & grepl("\\)", product) & (grepl("box", product) | grepl("unit", product)) & is.na(product_received_from) == FALSE) & (!grepl("\\/", product))) {
            df_product_test_x[row, "product_units_received"] <- as.character(
                as.numeric(
                    stringi::stri_extract_all_regex(
                        substring(
                            stringr::str_extract_all(product, "\\([^()]+\\)")[[1]],
                            2,
                            nchar(stringr::str_extract_all(product, "\\([^()]+\\)")[[1]]) - 1
                        ),
                        "[1-9]+"
                    )[[1]][1]
                )
            )
        }

        # recode product_units_released
        if ((grepl("\\(", product) & grepl("\\)", product) & (grepl("box", product) | grepl("unit", product)) & is.na(product_released_to) == FALSE) & (!grepl("\\/", product))) {
            df_product_test_x[row, "product_units_released"] <- as.numeric(
                stringi::stri_extract_all_regex(
                    substring(
                        stringr::str_extract_all(product, "\\([^()]+\\)")[[1]],
                        2,
                        nchar(stringr::str_extract_all(product, "\\([^()]+\\)")[[1]]) - 1
                    ),
                    "[1-9]+"
                )[[1]][1]
            )
        }
    }

    return(df_product_test_x)
}
