library(arrow)
library(tidyverse)

errorClassificationData<- read_csv2(file = "tools/LogViewerA4D/errorClassification.csv")
errorClassificationData$...1 <- NULL


logData  <-read_parquet(file = "tools/LogViewerA4D/temp.parquet")
logData$logCode <- NA
logData$logString<- NA
logData$RFile<- NA
logData$Line<- NA
logData$logRegex <- NA
logData$val1 <- NA
logData$val2 <- NA
logData$val3 <- NA
logData$val4 <- NA
logData$val5 <- NA


extract_values <- function(message,pattern) {
    # Extract matches
    matches <- regmatches(message, gregexpr(pattern, message))

    # If matches were found, extract the captured groups
    if(length(matches[[1]]) > 0) {
        # Extracting the groups

        val1 <- NA
        val2 <- NA
        val3 <- NA
        val4 <- NA
        val5 <- NA

        val1 <- gsub(pattern, "\\1", matches[[1]])
        val2 <- gsub(pattern, "\\2", matches[[1]])
        val3 <- gsub(pattern, "\\3", matches[[1]])
        val4 <- gsub(pattern, "\\4", matches[[1]])
        val5 <- gsub(pattern, "\\5", matches[[1]])

        return(data.frame(   val1=val1, val2=val2,val3=val3,val4=val4,val5=val5))
    } else {
        return(data.frame(   val1=NA, val2=NA,val3=NA,val4=NA,val5=NA))
    }
}



matchErrors <- function(logDataRow) {
for(errorClassificationData_row in 1:nrow(errorClassificationData))
{
        row <- errorClassificationData[errorClassificationData_row,]
        logDataRow[names(row)] <- row
        data_list <- extract_values(message = logDataRow$Message,pattern = row$logRegex)
        if(!is.na(data_list$val1))
        {
        logDataRow[names(data_list)] <- data_list
        logData[i,] <-  logDataRow
        return(logData[i,])
        break
}
}
    return(logDataRow)
}
for (i in 1:nrow(logData)) {
    logDataRow <- logData[i,]
    newRow <- matchErrors( logDataRow )
    if(!is.na(newRow$val1)) logData[i,] <- newRow
}





library(arrow)
library(tidyverse)

# Reading the classification data and removing the first unnamed column
errorClassificationData <- read_csv2("tools/LogViewerA4D/errorClassification.csv") %>%
    select(-...1)

# Reading the log data
logData <- read_parquet("tools/LogViewerA4D/temp.parquet")
logData %>%  filter(str_detect(Message,"not create table"))

unique(logData$logRegex)
# Initializing new columns in logData with NA
logData$logCode <- NA
logData$logString<- NA
logData$RFile<- NA
logData$Line<- NA
logData$logRegex <- NA
logData$val1 <- NA
logData$val2 <- NA
logData$val3 <- NA
logData$val4 <- NA
logData$val5 <- NA

# Function to extract values based on pattern matching
extract_values <- function(message, pattern) {
    matches <- regmatches(message, gregexpr(pattern, message, perl = TRUE))

    if (length(matches[[1]]) > 0) {
        suppressMessages(data_list <- map_dfc(1:5, ~ gsub(pattern, paste0("\\", .x), matches[[1]][1])))
        names(data_list) <- paste0("val", 1:5)
    } else {
        data_list <- tibble(val1 = NA, val2 = NA, val3 = NA, val4 = NA, val5 = NA)
    }

    return(data_list)
}

# Optimized matchErrors function
matchErrors <- function(logDataRow) {
    for (row in split(errorClassificationData, seq(nrow(errorClassificationData)))) {
        data_list <- extract_values(message = logDataRow$Message, pattern = row$logRegex)

        if (!is.na(data_list$val1[1])) {
            logDataRow[names(data_list)] <- data_list
            logDataRow[names(row)] <- row

            return(logDataRow)
        }
    }
    return(logDataRow)
}


logError(curr_sheet, " trying with non_processed_dates in product_entry_date. Error: ", e$message)



# Applying the optimized matchErrors function
logData <- map_dfr(1:nrow(logData), ~ matchErrors(logData[.x, ]))

