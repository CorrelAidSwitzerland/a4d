#' Changes the path to A4D data that is set as an environment variable
#'
#' @return Changes the env variable to A4D data folder
#' @export
#'
change_A4D_directory <- function() {
    cat("Navigate to the directory")

    a4d_data_root <- rstudioapi::selectDirectory()
    Sys.setenv(A4D_DATA_ROOT = a4d_data_root)

    cat("\n\nA4D data folder changed to:", a4d_data_root, "\n")
}
