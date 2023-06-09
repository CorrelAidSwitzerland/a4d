#' Configure basic logger
#'
#' @param output_dir: Output directory of the main script
#' @export
setup_logger <- function(output_dir) {
    example_layout <-
        layout_glue_generator(format = '{level} {format(time, \"%Y-%m-%d %H:%M:%S\")} {fn}: {msg}')

    log_layout(example_layout)

    log_threshold(TRACE)

    log_dir <- file.path(output_dir, "logs")

    if (!file.exists(log_dir)) {
        dir.create(log_dir, recursive = FALSE)
    }
}


setup_sink <- function(output_dir, tracker_file) {
    file_name <-
        paste(format(Sys.time(), "%Y%m%d_%H%M%S"),
            tools::file_path_sans_ext(tracker_file),
            sep = "_"
        )

    log_dir <- file.path(output_dir, "logs")
    log_file_name <- paste0(file_name, ".log")
    log_file <- file.path(log_dir, log_file_name)
    sink(file = log_file, append = F)
}
