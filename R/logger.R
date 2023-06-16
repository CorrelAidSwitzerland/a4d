#' Configure basic logger
#'
#' @param output_dir: Output directory of the main script
#' @export
setup_logger <- function(output_dir) {
    logFileName <- file.path(output_dir, "logs", "main.log")

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

    if (!file.exists(log_dir)) {
        dir.create(log_dir, recursive = FALSE)
    }
}
