#### Define functions #####

`%notin%` <- Negate(`%in%`)

#@Description: Clean df where first row is empty
set_second_row_as_headers_and_remove_first_row <- function(df){
  colnames(df) <- df[1, ]
  df <- df[-1, ]
  return(df)
}

#### 1. Identify xlsx cutoff ####
# ==============================================================================

# TODO: Test whether both helper functions work for all years 
#       and if word searches need to be done for years separately
# Conditions to cut of at start msd data
helper_is_msd_start_row <- function(df, i){
  
  condition <- case_when(
    
    #2021
    any(grepl("MEDICAL", df[i, ]))
    & any(grepl("Product", df[i+1, ])) 
    & any(grepl("Entry Date", df[i+1, ])) ~ TRUE,
  
    # 2019 & 2020
    any(grepl("Product", df[i, ])) &
      any(grepl("Date", df[i, ])) &
      any(grepl("Units Received", df[i, ])) ~ TRUE,
    
    # 2017 & 2018
    any(grepl("Description of Support", df[i,])) & 
      any(grepl("Date", df[i,])) &
      any(grepl("Units Received", df[i,])) ~ TRUE,
    
    TRUE ~ FALSE
  )
}

# Conditions to cut of at start patient data
helper_is_msd_end_row <- function(df, i){

  includes_patient_name <- any(grepl("Patient Name", df[i+1, ]))
  includes_patient_recruitment <- any(grepl("patient recruitment", 
                                            tolower(df[i, ])))
  
  condition <- case_when(
    !includes_patient_name ~ FALSE,
    !includes_patient_recruitment ~ FALSE,
    !any(grepl("patient id", tolower(df[i+1, ]))) ~ FALSE,
    #!any(grepl("No", df[i + 1, ])) ~ FALSE,
    !any(grepl("ID", df[i + 1,])) ~ FALSE,
    TRUE ~ TRUE
    )

  return(condition)
}


get_msd_start <- function(df, j) {
  if(helper_is_msd_start_row(df, j)){
      
      start_df_msd <- j+1
    }
}

get_msd_end <- function(df, j){
  if(helper_is_msd_end_row(df, j)){
    end_df_msd <- j-1
  }
}

get_patient_start <- function(df, j){
  if(helper_is_msd_end_row(df, j)){
    start_df_patient <- j+1
  }
}

get_patient_end <- function(df, j){
  if(helper_is_msd_end_row(df, j)){
    end_df_patient <- nrow(df)
  }
}

# @Description: Reads product data from a monthly file based on extraction logic
extract_product_data <- function(monthly_tracker_df){
  
  print("Extract product data - Start")
  
  for(i in 1:nrow(monthly_tracker_df)){
    if(is_empty(start_df_msd)){
    start_df_msd <- get_msd_start(monthly_tracker_df, i)
    }
    if(is_empty(end_df_msd)){
    end_df_msd <- get_msd_end(monthly_tracker_df, i)
    }
  }
  product_data_df <- monthly_tracker_df[start_df_msd:end_df_msd, ]
  
  # Exception 2021: Remove row with "Medical Supplies Distribution" Title
  for(i in 1:nrow(monthly_tracker_df)){
    if(any(grepl("MEDICAL", df_alt1[i, ]))){
      remove_row_index <- i
    }
  }
  product_data_df <- product_data_df[-i,]
  
  # Clean empty remaining first row
  product_data_df <- set_second_row_as_headers_and_remove_first_row(product_data_df)

  print("Extract product data - End")
  
  return(product_data_df)
}

# @Description: Reads patient summary (? -> to be checked)
extract_patient_data_in_products <- function(monthly_tracker_df){
  print("Extract patient summary data - Start")
  
  for(i in 1:nrow(monthly_tracker_df)){
    start_df_patient <- get_patient_start(monthly_tracker_df, i)
    end_df_patient <- get_patient_end(monthly_tracker_df, i)
  }
  patient_df <- monthly_tracker_df[start_df_patient:end_df_patient, ]
  
  # Clean empty  first row
  patient_df <- set_second_row_as_headers_and_remove_first_row(patient_df)
  
  
  print("Extract patient summary data - End")
  
  return(patient_df)
}

#### 2. Match Product data columns ####
# ==============================================================================

# @Description: Imports the patient df, cleans it and matches it against 
#               column synonyms to unify column names
# @columns_synonyms: Long format output of read_column_synonyms to match columns
harmonize_product_data_columns <- function(product_df, columns_synonyms){
  
  product_df <- product_df %>% discard(~all(is.na(.) | . ==""))
  product_df <- product_df[!is.na(names(product_df))]
  
  
  
  colnames(product_df) <- sanitize_column_name(colnames(product_df)) 
  synonym_headers <- sanitize_column_name(columns_synonyms$name_to_be_matched)
  
  
  
  # replacing var codes
  colnames_found <- match(colnames(product_df),synonym_headers , nomatch = 0)
  colnames(product_df)[colnames(product_df) %in% synonym_headers] <- columns_synonyms$name_clean[colnames_found]
  
  
  if (sum(colnames_found == 0) != 0) {"Non-matching column names found (see 0)"
    view(colnames_found)}
  else {return(product_df)}
  
  
}
# @Description: Imports the codebook, cleans, removes duplicates and transforms it
#               into long df format
read_column_synonyms <- function(codebook_data_file){
  columns_synonyms <- codebook_data_file %>%
    read_xlsx(sheet = "synonyms_ProductData") %>%
    as_tibble() %>%
    pivot_longer(cols = everything(),
                 names_to = "name_clean",
                 values_to = "name_to_be_matched") %>%
    subset(!is.na(name_to_be_matched)) %>%
    # lapply(sanitize_column_name) %>%
    as_tibble() %>%
    group_by(name_to_be_matched) %>%
    slice(1) %>%
    ungroup()
  # view(columns_synonyms)
  return(columns_synonyms)
}

sanitize_column_name <- function(column_name){
  column_name <- column_name %>% 
    str_to_lower() %>% 
    str_replace_all(fixed(" "), "") %>% 
    str_replace_all("[^[:alnum:]]", "")
  
  column_name_clean <- column_name 
  
  return(column_name_clean)
}




