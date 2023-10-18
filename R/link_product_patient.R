#' @title Link Product and Patient Data
#'
#' @description
#' This function links a product csv file with a patient csv file. It reads the csv files, merges them based on 'file_name' and patient 'id',
#' finds the ids that are in the product data but not in the patient data, creates a summary of these ids, and logs the process.
#'
#' @param product_file A string specifying the path to the product csv file.
#' @param patient_file A string specifying the path to the patient csv file.
#'
#' @return This function does not return a value. It prints log messages about the linking process.
#'
#' @examples
#' link_product_patient("path/to/product_data.csv", "path/to/patient_data.csv")
link_product_patient <- function(product_file, patient_file) {
    logInfo("Trying to link product csv file ", product_file, " with patient csv file ", patient_file)

    patient_data <- read.csv(patient_file, fileEncoding = "UTF-16LE")
    product_data <- read.csv(product_file, fileEncoding = "UTF-16LE")

    # Merge the data frames by file_name and patients ids
    merged_data <- merge(product_data, patient_data,
        by.x = c("file_name", "product_released_to"),
        by.y = c("file_name", "id"),
        all.x = TRUE
    )

    # Find the ids that are in product_data but not in patient_data
    missing_ids <- merged_data[is.na(merged_data$sheet_name), ]

    if (nrow(missing_ids) > 0) {
        # Create a summary of the missing_ids data frame
        summary_table <- table(missing_ids$file_name, missing_ids$product_released_to)

        # Convert the table to a data frame
        summary_df <- as.data.frame(summary_table)

        # Rename the columns
        names(summary_df) <- c("file_name", "product_released_to", "count")

        summary_df <- dplyr::filter(summary_df, count > 0)

        tryCatch(
            {
                if (nrow(summary_df) > 0) {
                    logWarn(
                        "The number of mismatched patient IDs between the product and patient data is ",
                        nrow(summary_df), ". ",
                        paste("File Name: ", summary_df$file_name,
                            " Patient ID in product: ", summary_df$product_released_to,
                            " Count in product: ", summary_df$count,
                            sep = "", collapse = ", "
                        )
                    )
                }
            },
            error = function(e) {
                logError("Could not link csv files for product and patient data. Error: ", e$message)
            },
            warning = function(w) {
                logWarn("Could not link csv files for product and patient data. Warning: ", w$message)
            }
        )
    } else {
        logInfo(
            "There are no mismatched patient IDs between the product data - ",
            product_file, " and patient data - ", patient_file
        )
    }

    logInfo("Finished attempting to link product csv file with patient csv file.")
}
