#' @title Get mapping table for A4D
#'
#' @param tracker_files The paths to the tracker files found using `get_tracker_files()`
#' @param output_dir The output destination of the mapping table
#'
#' @return A mapping table containing the original and scrambled file names
#' and a .csv file that is exported to selected A4D directory
#' The scrambled names contain the year of the original file name and a hash
#'
#'
get_mapping_table <- function(tracker_files, output_dir) {
    dirnames <- purrr::map_chr(tracker_files, dirname)
    basenames <- purrr::map_chr(tracker_files, function(x) {
        tools::file_path_sans_ext(basename(x))
    })
    pseudo_names <- stringi::stri_rand_strings(length(basenames), 10)
    mapped_names <- purrr::map2_chr(basenames, pseudo_names, function(x, y) {
        paste0(stringr::str_extract(x, pattern = "[:digit:]{4}"), "_", y)
    })

    mapping_table <- data.frame(original = tracker_files, dirname = dirnames, basename = basenames, pseudoname = mapped_names)

    write.csv(x = mapping_table, file.path(output_dir, "mapping_table.csv"), row.names = F)

    return(mapping_table)
}
