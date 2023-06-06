#' Configure basic logger
#'
#' @param output_dir: Output directory of the main script
#'
#' @return
#' @export
setup_logger <- function(output_dir) {
    example_layout <-
        layout_glue_generator(format = '{level} {format(time, \"%Y-%m-%d %H:%M:%S\")} {call} {fn}: {msg}')

    log_layout(example_layout)

    log_threshold(TRACE)

    log_dir = file.path(output_dir, "logs")
    log_file_name = paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
    log_file = file.path(log_dir, log_file_name)
    log_appender(appender_file(log_file))

    if (!file.exists(log_dir)) {
        dir.create(log_dir, recursive = FALSE)
    }


}
