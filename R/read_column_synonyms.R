# @Description: Imports the codebook, cleans, removes duplicates and transforms it
# into long df format
#' @title Get all synonyms for all variable names
#'
#' @description
#' This function reads a specific sheet from an excel file
#' and generates a tibble containing unique column names and their synonyms.
#'
#' @param master_tracker_file An excel file containing the synonyms.
#' @param sheet The name of the sheet to read from the excel file.
#'
#' @return A tibble containing unique column names and their synonyms.
#' @export
#'
#' @examples
#' \dontrun{
#' read_column_synonyms("master_tracker_variables.xlsx", "synonyms_PatientData")
#' }
read_column_synonyms <- function(master_tracker_file, sheet) {
    columns_synonyms <- master_tracker_file %>%
        readxl::read_xlsx(sheet = sheet) %>%
        as_tibble() %>%
        pivot_longer(
            cols = everything(),
            names_to = "variable_name",
            values_to = "tracker_name"
        ) %>%
        subset(!is.na(tracker_name)) %>%
        # lapply(sanitize_column_name) %>%
        as_tibble() %>%
        group_by(tracker_name) %>%
        slice(1) %>%
        ungroup()
}
