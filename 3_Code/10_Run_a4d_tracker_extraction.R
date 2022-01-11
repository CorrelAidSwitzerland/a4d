## Example script to process a tracker file
## TODO: Enhance with upload to database


## Set wd equal path of repository
setwd("D:/a4d_code/a4d_analytics")

# Tracker path
example_tracker_path <- "d:/a4d_data/01_2017 AN Clinic IX A4D Tracker (1).xlsx"
# example_tracker_path <- "2_Data/2017_Tracker.xlsx"

codebook_path <- "4ADMonthlyTrackerCodebook.xlsx"

# Setup
source("3_Code/01_a4d_tracker_extract.R")
source("3_Code/02_a4d_tracker_format.R")

codebook <- read_column_synonyms(codebook_path)

### Data extraction
df_raw <- reading_a4d_tracker(tracker_data_file = example_tracker_path, codebook = codebook)


### Data cleanse
df_cleaned <- cleaning_a4d_tracker(data = df_raw)
View(df_raw)
View(df_cleaned)
