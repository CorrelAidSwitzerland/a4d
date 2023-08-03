to_numeric <- function(x) {
    d <- try(as.numeric(d), silent = TRUE)
    if (class(d) == "try-error") {
        d <- 999999
    }
    return(d)
}
