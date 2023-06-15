#' @title Get mapping table for A4D
#'
#' @param tracker_files The paths to the tracker files found using `get_tracker_files()`
#' @param mapping_table_output The output destination of the mapping table
#'
#' @return A mapping table containing the original and scrambled file names
#' and a .csv file that is exported to selected A4D directory
#' The scrambled names contain the year of the original file name and a hash
#'
#'
get_mapping_table <- function(tracker_files, mapping_table_output) {
    myrules <- data.frame(
        Column = c("scrambled_name"),
        Method = c("hash"),
        Method.Param = c("md5"),
        Max.Length = c(8)
    )

    mapping_table <- tracker_files %>%
        bind_cols(.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>%
        set_names("original_name") %>%
        mutate(
            scrambled_name = original_name,
            year = str_extract(original_name, pattern = "[:digit:]{4}"),
            scrambled_name = as.factor(scrambled_name)
        ) %>%
        # only works with data frames
        as.data.frame() %>%
        scrambler::scrambleDataFrame(
            data = .,
            seed = 123,
            scrambling.rules = myrules
        ) %>%
        unite("scrambled_name", year:scrambled_name, sep = "_")

    write.csv(x = mapping_table, file.path(mapping_table_output, "Mapping_table.csv"), row.names = F)

    return(mapping_table)
}
