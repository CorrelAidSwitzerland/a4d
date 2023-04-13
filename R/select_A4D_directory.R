#' Selects path to A4D data and sets it as an environment variable
#'
#' @return Sets the env variable to A4D data folder
#' @export
#'
select_A4D_directory <- function() {
    a4d_data_root <- Sys.getenv("A4D_DATA_ROOT")
    if (a4d_data_root == "") {
        answer <- readline(prompt = cat('\nDo you want to set an env variable for the A4D data folder? (write "y/n" and press enter):\n'))

        if (answer == "y") {
            cat("Navigate to the directory")
            a4d_data_root <- rstudioapi::selectDirectory()
            Sys.setenv(A4D_DATA_ROOT = a4d_data_root)

            cat("\n\nA4D data folder set to:", a4d_data_root, "\n")
        }
    }
    return(a4d_data_root)
}
