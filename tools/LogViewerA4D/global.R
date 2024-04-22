library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(arrow)
library(plotly)
library(ggplot2)


levelsValues <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
colorLevels <- c("TRACE", "DEBUG", "WARN", "ERROR", "FATAL")
colors <- c(rgb(0.8, 0.9, 1), rgb(0.8, 1, 0.8), rgb(1.0, 0.88, 0.7), rgb(1, 0.84, 0.8), rgb(1, 0.8, 0.94))
truncScript <- "function(data, type, row, meta) {\n
      return type === 'display' && data != null && data.length > 100 ?\n
        '<span title=\"' + data.substr(0, 1000) + '\">' + data.substr(0, 100) + '...</span>' : data;\n
     }"


parseLines <- function(lines) {
    rows <- strsplit(lines, "\t")
    malformed <- sapply(rows, function(x) length(x) != 6)
    rows <- rows[!malformed]
    result <- data.frame(
        Timestamp = as.POSIXct(sapply(rows, function(x) x[1])),
        Thread = sapply(rows, function(x) x[2]),
        Level = sapply(rows, function(x) x[3]),
        Package = sapply(rows, function(x) x[4]),
        Function = sapply(rows, function(x) x[5]),
        Message = sapply(rows, function(x) x[6])
    )
    return(result)
}
