# The error report should report standard statistics about the data ingestion
# process like how many files were processed, how many successfully, how many
# errors overall, how many warnings (avoid duplicates), list all files with
# errors or warnings, kind of warnings per file etc.

# list of log files
logs <- list.files(path = here::here("data", "output", "logs"),
                   # probably redundant, as logs only contains log files?
                   pattern = "*.log"
                   )

# read log files and combine into one dataframe
all_logs <- purrr::map(seq_along(logs), function(i) {

    df <- readr::read_delim(here::here("data", "output", "logs", logs[[i]]),
                      delim = "\t", col_names = FALSE)

    df <- df %>%
        mutate(id = logs[[i]])

    return(df)
})

logs_df <- bind_rows(all_logs)



