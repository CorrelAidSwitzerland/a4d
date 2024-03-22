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
#' @param logfile The name of the log file.
#' @param output_root Output root directory for the current process.
#'
#' @return returns the loggers that where previously set for usage with with_
setup_file_logger <- function(logfile, output_root) {
    loggers <- getLoggers()
    clearLoggers()
    logFileName <- file.path(output_root, "logs", paste0(logfile, ".log"))
    if (file.exists(logFileName)) {
        file.remove(logFileName)
    }
    addDefaultFileLogger(logFileName, name = logfile)
    loggers
}



#' Temporary File Logger
#'
#' Temporarily change logging to exclusively log to specific log file
#'
#' @param new New filename for the logfile
#' @param code Code to be executed
#' @param output_root Root output folder from where to construct the log path

#' @export
with_file_logger <- withr::with_(
    setup_file_logger,
    function(loggers) {
        clearLoggers()
        for (logger in loggers) {
            registerLogger(logger)
        }
    }
)


#' @title Log-To-Json Formatter
#'
#' @param message Log Message with Glue Syntax {values['X']}
#' @param values A named list with the Glue Referenced Values as single Strings
#' @param currentFile String of current File with Path the Log is evoked
#' @param line Current Line of the LogMessage (May Change over time but should stay close to the real value)
#' @param errorCode A Combination of UID Letters for Easy location of the Message Origin
#' @return Stringified JSON to Log with logX-Functions
log_to_json <- function(message, values = NA, file = NA, line = NA, errorCode = NA, functionName = NA) {
    logObject <- list(
        message = glue::glue(message),
        values = values,
        file = file,
        line = line,
        errorCode = errorCode,
        functionName = functionName
    )
    return(jsonlite::toJSON(logObject))
}
