#' @title initialize all necessary paths
#'
#' @description
#' Create necessary output folder for main script under tracker_root_path.
#' This script creates a new output folder next to the data files, and
#' deletes all old files in it.
#' The script asks the user to set the tracker_root_path, either by
#' selecting a folder or reading from the A4D_DATA_ROOT env var.
#'
#' @param names Folder names that will be created under the output folder.
#' @param output_root_name The name of the main output folder created in the data folder.
#' @param delete If TRUE, delete all files under output.
#'
#' @return A list with tracker_root_path and output_root path
init_paths <- function(names, output_dir_name = "output", delete = FALSE) {
    paths <- list()
    tracker_root_path <- select_A4D_directory()
    paths$tracker_root <- tracker_root_path

    output_root <- file.path(
        tracker_root_path,
        output_dir_name
    )

    paths$output_root <- output_root

    for (name in names) {
        subdir <- file.path(
            tracker_root_path,
            output_dir_name,
            name
        )

        if (fs::dir_exists(subdir)) {
            if (delete) {
                fs::dir_delete(subdir)
            }
        }

        fs::dir_create(subdir)

        paths[[name]] <- subdir
    }

    paths
}


#' @title Find all files matching a search pattern in a given directory.
#'
#' @description
#' Searches recursively for files matching a search pattern inside the root dir.
#' Only returns the file names without the paths.
#'
#'
#' @param tracker_root The root directory to search in.
#' @param pattern The search pattern to filter files.
#'
#' @return A vector with file names.
get_files <- function(tracker_root, pattern = "\\.xlsx$") {
    tracker_files <- list.files(path = tracker_root, recursive = T, pattern = pattern)
    tracker_files <-
        tracker_files[str_detect(tracker_files, "~", negate = T)]
}


#' @title Read synonyms from the synonyms YAML files.
#'
#' @description
#' Read in all defined synonyms from the YAML files inside the synonyms folder.
#'
#' @return A list with both patient and product data synonyms as tibble.
get_synonyms <- function() {
    ## Extract synonyms for products and patients
    ## If you encounter new columns, just add the synonyms to these YAML files
    synonyms_patient <-
        read_column_synonyms(synonym_file = "synonyms_patient.yaml")
    synonyms_product <-
        read_column_synonyms(synonym_file = "synonyms_product.yaml")

    list(patient = synonyms_patient, product = synonyms_product)
}


#' @title Get all synonyms for all variable names
#'
#' @description
#' This function reads the synonyms from a YAML file
#' and generates a tibble containing unique column names and their synonyms.
#'
#' @param synonym_file A YAML file containing the synonyms
#'
#' @return A tibble containing unique column names and their synonyms.
#' @export
#'
#' @examples
#' \dontrun{
#' read_column_synonyms(synonym_file = "synonyms_patient.yaml")
#' read_column_synonyms(synonym_file = "synonyms_product.yaml")
#' }
read_column_synonyms <- function(synonym_file) {
    columns_synonyms <-
        yaml::read_yaml(file.path("synonyms/", synonym_file)) %>%
        unlist() %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        # remove digits that were created when converting to data frame
        mutate(
            rowname = str_replace(rowname, pattern = "[:digit:]$", "")
        ) %>%
        rename(
            "variable_name" = "rowname",
            "tracker_name" = "."
        ) %>%
        as_tibble()
}


#' @title Export data as CSV to a given destination.
#'
#' @param data Data frame to export as CSV.
#' @param filename Output file name.
#' @param output_root Root output directory.
#' @param suffix Suffix will be appended to the original file name (e.g. "patient_data").
#'
#' @examples
#' \dontrun{
#' export_data(
#'     data = df_raw_product,
#'     filename = tracker_name,
#'     output_root = output_root,
#'     suffix = "_product_data"
#' )
#' }
export_data <- function(data, filename, output_root, suffix) {
    logDebug("Start export_data. Suffix = ", suffix, ".")
    data %>%
        write.csv(
            file =
                file.path(
                    output_root,
                    paste0(
                        filename,
                        suffix,
                        ".csv"
                    )
                ),
            row.names = F
        )
    logInfo("Finish export_data. Suffix = ", suffix, ".")
}



#' @title Read in patient data from CSV.
#'
#' @param patient_file_path Path to the CSV file.
#'
#' @return tibble with patient data
read_raw_csv <- function(file) {
    logDebug("Start reading data with read_csv.")
    df_patient_raw <- read_csv(
        file,
        name_repair = "check_unique",
        progress = FALSE,
        show_col_types = FALSE,
        col_types = readr::cols(.default = "c")
    )
    logDebug("Finished loading data with read_csv.")
    logInfo("Dim: ", dim(df_patient_raw))
    logInfo("Columns: ", spec(df_patient_raw))

    df_patient_raw
}


#' @title Read allowed provinces from a YAML file.
#'
#' @description
#' Read in all provinces from a YAML file inside the provinces folder.
#'
#' @return A named character vector with all allowed provinces.
get_allowed_provinces <- function() {
    ## Should new countries and provinces be added, update the YAML file
    provinces <- yaml::read_yaml("provinces/allowed_provinces.yaml") %>% unlist()
    return(provinces)
}
