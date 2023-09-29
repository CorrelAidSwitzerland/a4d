#' Configure basic logger
#'
#' @param output_dir: Output directory of the main script
#' @export
setup_logger <- function(output_dir, log_name) {
    logFileName <- file.path(output_dir, "logs", paste0("main_", log_name, ".log"))

    if (file.exists(logFileName)) {
        file.remove(logFileName)
    }

    logger <- createLogger(
        name = "MAIN",
        threshold = "TRACE",
        appenders = list(
            createFileAppender(
                layout = layoutParallel,
                fileName = logFileName
            )
        )
    )
    registerLogger(logger)

    log_dir <- file.path(output_dir, "logs")

    fs::dir_create(log_dir)
}


#' @title Add a basic default file logger for a newly processed file.
#'
#' @param output_root Output root directory for the current process.
#' @param logfile The name of the log file.
setup_file_logger <- function(output_root, logfile) {
    logFileName <- file.path(output_root, "logs", paste0(logfile, ".log"))
    if (file.exists(logFileName)) {
        file.remove(logFileName)
    }
    addDefaultFileLogger(logFileName, name = logfile)
}
