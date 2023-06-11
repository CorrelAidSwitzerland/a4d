#' Configure basic logger
#'
#' @param output_dir: Output directory of the main script
#' @export
setup_logger <- function(output_dir) {


    logFileName <- paste0(init_paths()$output_root, "/logs/main.log")

    logger <- ParallelLogger::createLogger(name = "PARALLEL",
                                           threshold = "TRACE",
                                           appenders = list(ParallelLogger::createFileAppender(layout = ParallelLogger::layoutParallel,
                                                                                               fileName = logFileName)))
    ParallelLogger::registerLogger(logger)

    log_dir <- file.path(output_dir, "logs")

    if (!file.exists(log_dir)) {
        dir.create(log_dir, recursive = FALSE)
    }
}
