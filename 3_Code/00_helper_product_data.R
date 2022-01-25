#### Define functions #####

`%notin%` <- Negate(`%in%`)



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
    !any(grepl("No", df_alt1[i + 1, ])) ~ FALSE,
    !any(grepl("ID", df_alt1[i + 1,])) ~ FALSE,
    TRUE
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

#### 2. Identify xlsx cutoff ####
# ==============================================================================
