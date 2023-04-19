#' Helper function that sets the env variable to the A4D tracker files.
#'
#' @return Returns a character representing the path to the tracker data.
#' @export
#'
set_a4d_data_root <- function() {
    cat("Select the directory containing the tracker files")
    a4d_data_root <- rstudioapi::selectDirectory()
    Sys.setenv(A4D_DATA_ROOT = a4d_data_root)

    cat("\n\nA4D data folder set to:", a4d_data_root, "\n")
    return(a4d_data_root)
}
