#' Configure basic logger
#'
#' @param output_dir: Output directory of the main script
#' @export
setup_logger <- function(output_dir) {
    example_layout <-
        layout_glue_generator(format = '{level} {format(time, \"%Y-%m-%d %H:%M:%S\")} {fn}: {msg}')

    log_layout(example_layout)

    log_threshold(TRACE)

    file_name <- format(Sys.time(), "%Y%m%d_%H%M%S")

    log_dir <- file.path(output_dir, "logs")
    log_file_name <- paste0(file_name, ".log")
    log_file <- file.path(log_dir, log_file_name)
    log_appender(appender_file(log_file))

    log_threshold(ERROR, namespace = "logger.error")

    log_dir <- file.path(output_dir, "logs")
    log_file_name <- paste0(file_name, "_error", ".log")
    log_file <- file.path(log_dir, log_file_name)
    log_appender(appender_file(log_file), namespace = "logger.error")

    log_threshold(WARN, namespace = "logger.warning")

    log_dir <- file.path(output_dir, "logs")
    log_file_name <- paste0(file_name, "_warning", ".log")
    log_file <- file.path(log_dir, log_file_name)
    log_appender(appender_file(log_file), namespace = "logger.warning")

    if (!file.exists(log_dir)) {
        dir.create(log_dir, recursive = FALSE)
    }
}
