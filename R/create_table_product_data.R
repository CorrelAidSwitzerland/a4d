#' @title Create a Single CSV for Table Product Data
#'
#' @description
#' This function reads all CSV files in a specified directory, merges them into a single data frame,
#' and writes the merged data to a new CSV file with reordered columns according to the list of fields.
#' If there are any columns that are present in some CSV files but not others,
#' this function will add those missing columns to the data frames where they are missing and fill them with NA.
#'
#' @param input_root A string specifying the path to the directory containing the input CSV files.
#' @param output_root A string specifying the path to the directory where the output CSV file should be written.
#'
#' @return This function does not return a value. It writes the merged data to a new CSV file
#' (with reordered columns according to the list of fields) in the output_root directory.
#'
#' @examples
#' create_table_product_data("path/to/input/directory", "path/to/output/directory")
create_table_product_data <- function(input_root, output_root) {
    logInfo("Start creating single csv for table product_data.")

    # Get a list of all CSV files in the input_root directory
    files <- list.files(input_root, pattern = "*.csv", full.names = TRUE)

    # Read all CSV files and store them in a list
    data_list <- lapply(files, function(x) read.csv(x, fileEncoding = "UTF-16LE"))

    logInfo(length(data_list), " csv files will be processed for creating the single csv for table product_data.")

    # Get the union of all column names
    all_names <- unique(unlist(lapply(data_list, colnames)))

    # Add missing columns to each data frame and fill them with NA
    data_list <- lapply(data_list, function(x) {
        x[setdiff(all_names, colnames(x))] <- NA
        x
    })

    # Merge all data frames
    merged_data <- do.call(rbind, data_list)

    # Reorder the columns according to the list of fields
    merged_data <- reorder_product_fields(merged_data)

    # Write the merged data to a CSV file in the output_root directory
    export_data(
        data = merged_data,
        filename = "product_data",
        output_root = output_root,
        suffix = ""
    )

    logInfo("Finish creating single csv for table product_data.")
}


#' @title Reorder and Add Missing Fields in a Dataframe
#'
#' @description
#' This function checks if all specified fields are present in the input dataframe.
#' If any fields are missing, it adds them with NA values.
#' It then reorders the columns according to the specified list of fields.
#' Any additional fields in the input dataframe are moved to the end.
#'
#' @param merged_data A dataframe that needs its columns to be reordered and missing fields added.
#'
#' @return A dataframe with reordered columns and added missing fields.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(product = c("A", "B"), product_units_notes = c("note1", "note2"))
#' df <- reorder_product_fields(df)
#' }
#'
reorder_product_fields <- function(merged_data) {
    # List of fields
    fields <- c(
        "product",
        "product_units_notes",
        "product_entry_date",
        "product_units_released",
        "product_released_to",
        "product_units_received",
        "product_received_from",
        "product_balance",
        "product_units_returned",
        "product_returned_by",
        "product_table_month",
        "product_table_year",
        "product_sheet_name",
        "file_name",
        "product_balance_status",
        "product_country",
        "product_hospital"
    )

    # Check if all fields are present in merged_data
    missing_fields <- setdiff(fields, names(merged_data))

    # If there are missing fields, add them with NA
    if (length(missing_fields) > 0) {
        merged_data[missing_fields] <- NA
    }

    # Reorder the columns according to the list of fields
    merged_data <- merged_data[, c(fields, setdiff(names(merged_data), fields))]

    return(merged_data)
}
