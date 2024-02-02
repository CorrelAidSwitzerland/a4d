# This file contains the server-side code for a Shiny application that serves as a log viewer.
# It loads log data from a Parquet file, allows uploading of log files, and displays the log data in a DataTable.
# The server code defines various reactive expressions and event observers to handle user interactions and data updates.
# It also renders the UI elements and defines the behavior of the log table and details panel.



# Define the shinyServer function to handle the server-side logic of the Shiny application.
shinyServer(function(input, output, session) {

  # Define a reactiveValues object to store the event log data and the current table data.
  values <- reactiveValues(eventLog = NULL ,currentTable = NULL)

  # Render the UI for the "Load Last Saved Log-Data" button if the "temp.parquet" file exists.
  output$loadTempFileUI <- renderUI({
    if(file.exists("temp.parquet")) {
      actionButton("loadTempFile", "Load Last Saved Log-Data")
    } else
      NULL
  })

  # Observer for the "Load Last Saved Log-Data" button click event.
  observeEvent(input$loadTempFile,{
    print("Load Old Data")
    values$eventLog <- read_parquet("temp.parquet")
  })

  # Define a reactive expression to get the selected row from the log table.
  selectedRow <- reactive({
    idx <- input$logTable_rows_selected
    if (is.null(idx)) {
      return(NULL)
    } else {
      row <- values$currentTable[idx, ]
      return(row)
    }
  })

  # Observer for the file upload event.
  observeEvent(input$fileUpload, {
    req(input$fileUpload)
    allLogs <- tibble()
    withProgress(message = 'Creating Log DF', value = 0, {
      for (x in 1:length(input$fileUpload$datapath)) {
        tryCatch(
          {
            filePath <- input$fileUpload$datapath[x]
            x <- input$fileUpload$name[x]
            fileConnection <- file(filePath, open = "r")
            lines <- readLines(fileConnection)
            close(fileConnection)

            if(!is.null(lines)) {
              result <- parseLines(lines) %>%
                mutate(fileName = basename(x)) %>%
                mutate(fileNameParts = list(str_split(basename( x), "_") %>%
                                              first())) %>%
                unnest_wider(fileNameParts, names_sep = "_")
              allLogs <- allLogs %>%
                dplyr::bind_rows(result) %>%
                mutate(across(where(is.character), as.factor)) %>%
                mutate(Message = as.character(Message))
            }
          },
          error = function(cond) {
            message("Error message:")
            message(conditionMessage(cond))
            NA
          },
          warning = function(cond) {
            message("Warning message:")
            message(conditionMessage(cond))
            NULL
          },
          finally = {
            incProgress(1/length(input$fileUpload$datapath), detail = paste("Processed file ",x))
            message("Some other message at the end.Processed file ",x)
          }
        )
      }
    })
    values$eventLog <- allLogs
    write_parquet(allLogs,"temp.parquet")
  })

  # Render the log table using the DataTable library.
  output$logTable <- renderDT({
    req(values$eventLog)

    visibleLevels <- levelsValues[which(levelsValues == input$level):length(levelsValues)]
    idx <- values$eventLog$Level %in% visibleLevels

    options <- list(
      searching = TRUE,
      autoWidth = TRUE,
      paging = TRUE,
      pageLength = 20,
      lengthChange = T,
      scrollX = T,
      ordering = T,
      info = T,
      columnDefs = list(list(
        targets = c(5),
        render = JS(truncScript)
      ))
    )
    selection <- list(mode = "single", target = "row")
    values$currentTable <-  values$eventLog[idx, ]
    table <- datatable(values$eventLog[idx, ],
      options = options,
      filter = list(
        position = "top", clear = FALSE
      ),
      selection = selection,
      rownames = FALSE,
      escape = FALSE
    )
    table <- formatStyle(
      table = table,
      columns = 3,
      target = "row",
      backgroundColor = styleEqual(colorLevels, colors)
    )
    return(table)
  })

  # Render the status table to display the count of log entries by file name and level.
  output$status <- renderDT({
    req(values$eventLog)
    values$currentTable[input$logTable_rows_all,] %>% dplyr::count(fileName, Level)
  },
  filter = list(
    position = "top", clear = FALSE
  ))

  output$logOverviewPlot <- renderPlotly({

    req(values$eventLog)
    aggregatedData <- values$currentTable[input$logTable_rows_all,] %>% dplyr::count(fileName, Level) %>% filter(grepl(input$regexFilter,fileName))

    p<- ggplot(data= aggregatedData, aes(x=Level, y=fileName,fill=n)) + geom_tile() +  scale_y_discrete(label=abbreviate) +theme(axis.text=element_text(size=6))


    plotly::ggplotly(p,height = 800, width = 600)
  })

  # Render the details panel to display the selected log entry details.
  observeEvent(input$logTable_cell_clicked,{
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {


      lines <- list(
        "<table>",
        sprintf("<tr><td><strong>Timestamp</strong></td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", row$Timestamp),
        sprintf("<tr><td><strong>Thread</strong></td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", row$Thread),
        sprintf("<tr><td><strong>Level</strong></td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", row$Level),
        sprintf("<tr><td><strong>Package</strong></td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", row$Package),
        sprintf("<tr><td><strong>Function</strong></td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", row$Function),
        "</table>",
        "<h2>Message</h2>",
        sprintf("<p>%s</p>", row$Message)
      )

      showModal(modalDialog(
          tags$div(id = session$ns("constraintPlaceholder")),
          fade = F,
          easyClose = TRUE,
          footer = NULL
      ))
      insertUI(
          selector = paste0("#", session$ns("constraintPlaceholder")),
          where = "afterEnd",
          ui = HTML(  paste(lines, collapse = "\n"))
      )
    }
  })


  output$trackerSummary <- renderDT({
        req(values$eventLog)
      datatable(values$eventLog %>% group_by(fileName,Level) %>% count() %>%
                    pivot_wider(names_from = Level, values_from = n),
                filter = list(
                    position = "top", clear = FALSE
                ),
                rownames = FALSE,
                escape = FALSE
      )

  })

})
