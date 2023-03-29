## Example script to process a tracker file
## TODO: Enhance with upload to database


## Set wd equal path of repository
#setwd("D:/a4d_code/a4d_analytics")

# Tracker path
example_tracker_path <- "2_Data/2017_Tracker.xlsx"
# "d:/a4d_data/01_2017 AN Clinic IX A4D Tracker (1).xlsx"
# example_tracker_path <- "2_Data/2017_Tracker.xlsx"

codebook_path <- "4ADMonthlyTrackerCodebook.xlsx"

# Setup
source("3_Code/00_a4d_patient_tracker_extract_helper.R")
source("3_Code/01_a4d_patient_tracker_extract.R")
source("3_Code/02_a4d_patient_tracker_format.R")

## Extract codebooks for each data form
codebook_patient <- read_column_synonyms(
  codebook_path, sheet = "synonyms_PatientData"
)

codebook_product <- read_column_synonyms(
  codebook_path, sheet = "synonyms_ProductData"
)

columns_synonyms = codebook_patient

### Data extraction
df_raw <- reading_a4d_tracker(
  tracker_data_file = example_tracker_path,
  columns_synonyms = codebook_patient
  )
filename_output = df_raw[[2]]
df_raw_data = df_raw[[1]]
View(df_raw_data)

### Data cleanse
df_cleaned <- cleaning_a4d_tracker(data = df_raw_data)
View(df_cleaned)



# ===============
# read the excel
# split into 2 parts
# process patient
# process product
# combine different trackers
# write to the db (?)


