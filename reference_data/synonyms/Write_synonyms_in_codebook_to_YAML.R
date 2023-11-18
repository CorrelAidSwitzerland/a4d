# Small script that shows how to write the Excel sheets to a YAML  --------

codebook_data_file <- here::here("reference_data", "master_tracker_variables.xlsx")

codebook_data_file %>%
    readxl::read_xlsx(sheet = "synonyms_PatientData") %>%
    purrr::map(\(x) purrr::discard(x, is.na)) %>%
    # alternative - without using external packages
    # lapply(na.omit) %>% lapply(as.character) %>%
    yaml::write_yaml(file = here::here("reference_data/synonyms/synonyms_patient.yaml"))

codebook_data_file %>%
    readxl::read_xlsx(sheet = "synonyms_ProductData") %>%
    purrr::map(\(x) purrr::discard(x, is.na)) %>%
    # alternative - without using external packages
    # lapply(na.omit) %>% lapply(as.character) %>%
    yaml::write_yaml(file = here::here("reference_data/synonyms/synonyms_product.yaml"))
