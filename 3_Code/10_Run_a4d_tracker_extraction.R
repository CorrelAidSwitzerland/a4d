## Example script to process a tracker file
## TODO: Enhance with upload to database


## Set wd equal path of repository
# setwd("D:/Work/Correlaid/A4D/a4d_analytics")

# Tracker path
example_tracker_path <- "2_Data/2021_Tracker.xlsx"

# Setup
source("3_Code/01_a4d_tracker_extract.R")
source("3_Code/02_a4d_tracker_format.R")



### Data extraction
df_raw <- reading_a4d_tracker(tracker_data_file = example_tracker_path,
                                 year = 2021,
                                 clinic = "ABC",
                                 country = "DEF")


### Data cleanse
df_cleaned <- cleaning_a4d_tracker(data = df_raw)
View(df_raw)
View(df_cleaned)

