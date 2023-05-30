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
        yaml::read_yaml(here::here("synonyms", synonym_file)) %>%
        unlist %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        # remove digits that were created when converting to data frame
        mutate(
            rowname = str_replace(rowname, pattern = "[:digit:]$", "")
        )  %>%
        rename(
            "variable_name" = "rowname",
            "tracker_name" =  "."
        ) %>% as_tibble()

}
