cleaning_config <- yaml::read_yaml(here::here("reference_data","data_cleaning.yaml"))
allowed_provinces <- yaml::read_yaml(here::here("reference_data","provinces","allowed_provinces.yaml")) %>% unlist()

for (i in length(cleaning_config$province$steps)) {
  if (cleaning_config$province$steps[[i]]$type == "allowed_values")
      cleaning_config$province$steps[[i]]$allowed_values <- allowed_provinces
}

config <- list(cleaning = cleaning_config)

save(config,file = here::here("R","sysdata.rda"))
