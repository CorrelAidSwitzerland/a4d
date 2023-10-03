#' @title Create a Single CSV for Table Product Data
#'
#' @description
#' This function reads all CSV files in a specified directory, merges them into a single data frame, and writes the merged data to a new CSV file. If there are any columns that are present in some CSV files but not others, this function will add those missing columns to the data frames where they are missing and fill them with NA.
#'
#' @param input_root A string specifying the path to the directory containing the input CSV files.
#' @param output_root A string specifying the path to the directory where the output CSV file should be written.
#'
#' @return This function does not return a value. It writes the merged data to a new CSV file in the output_root directory.
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

    # Write the merged data to a CSV file in the output_root directory
    export_data(
        data = merged_data,
        filename = "product_data",
        output_root = output_root,
        suffix = ""
    )

    logInfo("Finish creating single csv for table product_data.")
}
