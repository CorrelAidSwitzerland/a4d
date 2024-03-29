#' Selects path to A4D data and sets it as an environment variable.
#'
#' @export
#'
#' @param reset A boolean. If set to TRUE, the directory containing the tracker.
#' data is changed.
#'
#' @return Returns a character representing the path to the tracker data.
#'
select_A4D_directory <- function(reset = FALSE) {
    a4d_data_root <- Sys.getenv("A4D_DATA_ROOT")
    if (reset || a4d_data_root == "") {
        a4d_data_root <- set_a4d_data_root()
    }
    return(a4d_data_root)
}
