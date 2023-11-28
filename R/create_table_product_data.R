#' @title Create a Single CSV for Table Product Data
#'
#' @description
#' This function reads all parquet files in a specified directory, merges them into a single data frame,
#' and writes the merged data to a new file with reordered and processed columns according to the list of fields.
#' If there are any columns that are present in some files but not others,
#' this function will add those missing columns to the data frames where they are missing and fill them with NA.
#'
#' @param input_root A string specifying the path to the directory containing the input .parquet files.
#' @param output_root A string specifying the path to the directory where the output .parquet file should be written.
#'
#' @return This function does not return a value. It writes the merged data to a new CSV file
#' (with reordered columns according to the list of fields) in the output_root directory.
#'
#' @examples
#' \dontrun{
#' create_table_product_data("path/to/input/directory", "path/to/output/directory")
#' }
create_table_product_data <- function(input_root, output_root) {
    logInfo("Start creating single file for table product_data.")

    # Get a list of all CSV files in the input_root directory
    files <- list.files(input_root, pattern = "*.parquet", full.names = TRUE)

    # Read all CSV files and store them in a list
    data_list <- lapply(files, function(x) arrow::read_parquet(x))

    logInfo(length(data_list), " files will be processed for creating the single file for table product_data.")

    # Get the union of all column names
    all_names <- unique(unlist(lapply(data_list, colnames)))

    # Add missing columns to each data frame and fill them with NA
    data_list <- lapply(data_list, function(x) {
        x[setdiff(all_names, colnames(x))] <- NA
        x
    })

    # Merge all data frames
    merged_data <- do.call(rbind, data_list)

    logDebug("Copying original parient IDs...")
    merged_data$orig_product_released_to <- merged_data$product_released_to

    logDebug("Trying to fix patient IDs...")
    merged_data$product_released_to <- sapply(merged_data$product_released_to, fix_id)

    logDebug("Extracting product_county and product_hospisal from patients IDs...")
    merged_data <- id_2_county_hospisal(
        merged_data, "product_released_to",
        "product_country", "product_hospital"
    )

    # Reorder, add, and ensures the correct data type for each column according to the list of fields
    merged_data <- preparing_product_fields(merged_data)

    # Write the merged and processed data to a CSV file in the output_root directory
    export_data_as_parquet(
        data = merged_data,
        filename = "product_data",
        output_root = output_root,
        suffix = ""
    )

    logInfo("Finish creating single file for table product_data.")
}


#' @title Update country and hospital based on patient ID
#'
#' @description
#' This function updates the `country` and `hospital` columns in a dataframe based on the `ID` column.
#' It looks for rows where `ID` matches a specific pattern (two letters, an underscore, two more letters, and then digits).
#' For these rows, it puts the first two letters into `country` and the second two letters into `hospital`.
#'
#' @param df A dataframe that contains the columns `ID`, `country`, and `hospital`.
#' @param id The name of the column in df that contains the patients ID information.
#' @param country The name of the column in df where the country information should be stored.
#' @param hospital The name of the column in df where the hospital information should be stored.
#' @return The original dataframe with updated `country` and `hospital` columns.
#' @export
#' @examples
#' df <- data.frame(
#'     id = c("US_CA123", "UK_LN456", "FR_PA789"),
#'     country = NA,
#'     hospital = NA,
#'     stringsAsFactors = FALSE
#' )
#' df <- id_2_county_hospisal(df, "ID", "country", "hospital")
id_2_county_hospisal <- function(df, id, country, hospital) {
    # Find rows with id matching the pattern (2 letters + _ + 2 letters + digits)
    matching_rows <- grepl("^[a-zA-Z]{2}_[a-zA-Z]{2}[0-9]+$", df[[id]])

    # Extract and update the product_country and product_hospital columns
    df[[country]][matching_rows] <- substr(df[[id]][matching_rows], 1, 2)
    df[[hospital]][matching_rows] <- substr(df[[id]][matching_rows], 4, 5)

    return(df)
}


#' @title Preparing Product Fields
#'
#' @description
#' This function processes fields for a single csv product_data.
#' It checks if all fields are present in merged_data and adds missing fields with NA.
#' It ensures the correct data type for each column, replaces incorrect values with specified error values,
#' and reorders the columns according to the list of fields.
#' Any additional fields in the input dataframe are moved to the end.
#'
#' @param merged_data A data frame that needs to be processed.
#'
#' @return A data frame with processed fields.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(product = c("A", "B"), product_units_notes = c("note1", "note2"))
#' df <- preparing_product_fields(df)
#' }
#'
preparing_product_fields <- function(merged_data) {
    # List of fields and their corresponding data types
    fields <- list(
        "product" = "character",
        "product_units_notes" = "character",
        "product_entry_date" = "Date",
        "product_units_released" = "numeric",
        "product_released_to" = "character",
        "product_units_received" = "numeric",
        "product_received_from" = "character",
        "product_balance" = "numeric",
        "product_units_returned" = "numeric",
        "product_returned_by" = "character",
        "product_table_month" = "integer",
        "product_table_year" = "integer",
        "product_sheet_name" = "character",
        "file_name" = "character",
        "product_balance_status" = "character",
        "product_country" = "character",
        "product_hospital" = "character",
        "product_category" = "character",
        "orig_product_released_to" = "character",
        "product_unit_capacity" = "integer"
    )

    logInfo("Start processing fields for the single csv product_data...")

    # Check if all fields are present in merged_data
    missing_fields <- setdiff(names(fields), names(merged_data))

    # If there are missing fields, add them with NA
    if (length(missing_fields) > 0) {
        merged_data[missing_fields] <- NA
    }

    # Ensure the correct data type for each column
    for (field in names(fields)) {
        tryCatch({
            if (fields[[field]] == "Date") {
                original_values <- merged_data[[field]]
                merged_data[[field]] <- suppressWarnings(as.Date(original_values))
                incorrect_rows <- which(is.na(merged_data[[field]]) & !is.na(original_values))
                if (length(incorrect_rows) > 0) {
                    logWarn(paste(
                        "In", field, "incorrect date values were replaced with",
                        ERROR_VAL_DATE, "in", length(incorrect_rows), "rows:",
                        paste(incorrect_rows, collapse = ", ")
                    ))
                    merged_data[incorrect_rows, field] <- ERROR_VAL_DATE
                }
            } else if (fields[[field]] == "numeric") {
                original_values <- merged_data[[field]]
                merged_data[[field]] <- suppressWarnings(as.numeric(original_values))
                incorrect_rows <- which(is.na(merged_data[[field]]) & !is.na(original_values))
                if (length(incorrect_rows) > 0) {
                    logWarn(paste(
                        "In", field, "incorrect numeric values were replaced with",
                        ERROR_VAL_NUMERIC, "in", length(incorrect_rows), "rows:",
                        paste(incorrect_rows, collapse = ", ")
                    ))
                    merged_data[incorrect_rows, field] <- ERROR_VAL_NUMERIC
                }
            } else if (fields[[field]] == "integer") {
                original_values <- merged_data[[field]]
                merged_data[[field]] <- suppressWarnings(as.integer(original_values))
                incorrect_rows <- which(is.na(merged_data[[field]]) & !is.na(original_values))
                if (length(incorrect_rows) > 0) {
                    logWarn(paste(
                        "In", field, "incorrect integer values were replaced with",
                        ERROR_VAL_NUMERIC, "in", length(incorrect_rows), "rows:",
                        paste(incorrect_rows, collapse = ", ")
                    ))
                    merged_data[incorrect_rows, field] <- ERROR_VAL_NUMERIC
                }
            } else {
                original_values <- merged_data[[field]]
                merged_data[[field]] <- as.character(original_values)
                incorrect_rows <- which(is.na(merged_data[[field]]) & !is.na(original_values))
                if (length(incorrect_rows) > 0) {
                    logWarn(paste(
                        "In", field, "incorrect character values  were replaced with",
                        ERROR_VAL_CHARACTER, "in", length(incorrect_rows), "rows:",
                        paste(incorrect_rows, collapse = ", ")
                    ))
                    merged_data[incorrect_rows, field] <- ERROR_VAL_CHARACTER
                }
            }
        }, warning = function(w) {
            logError(paste("Warning in converting", field, ": ", w))
        }, error = function(e) {
            logWarn(paste("Error in converting", field, ": ", e))
        }, finally = {
            logDebug(paste("Finished converting", field))
        })
    }

    # Reorder the columns according to the list of fields
    logInfo("Reorder the columns according to the list of fields...")
    merged_data <- merged_data[, c(names(fields), setdiff(names(merged_data), names(fields)))]

    logInfo("Finished processing fields for the single csv product_data.")

    return(merged_data)
}
