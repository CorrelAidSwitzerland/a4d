# This file contains the server-side code for a Shiny application that serves as a log viewer.
# It loads log data from a Parquet file, allows uploading of log files, and displays the log data in a DataTable.
# The server code defines various reactive expressions and event observers to handle user interactions and data updates.
# It also renders the UI elements and defines the behavior of the log table and details panel.
options(shiny.maxRequestSize = 30 * 1024^2)
# Define the shinyServer function to handle the server-side logic of the Shiny application.
shinyServer(function(input, output, session) {
    # Define a reactiveValues object to store the event log data and the current table data.
    values <- reactiveValues(eventLog = NULL, currentTable = NULL, proccessedFilesInfo = NULL, clinic_info_df = NULL)

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

    # Observer for the "Load Last Saved Log-Data" button click event.
    observeEvent(input$loadTempFile, {
        print("Load Old Data")
        values$eventLog <- read_parquet("temp.parquet")
    })


    # Render the details panel to display the selected log entry details.
    observeEvent(input$logTable_cell_clicked, {
        row <- selectedRow()
        if (is.null(row)) {
            return(NULL)
        } else {
            lines <- list(
                "<table>",
                sprintf("<tr><td><strong>Timestamp</strong></td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", row$Timestamp),
                sprintf("<tr><td><strong>Level</strong></td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", row$Level),
                sprintf("<tr><td><strong>Script</strong></td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", row$script),
                sprintf("<tr><td><strong>File</strong></td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", row$fileName),
                sprintf("<tr><td><strong>Function</strong></td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", row$functionName),
                sprintf("<tr><td><strong>ErrorCode</strong></td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", row$errorCode),
                sprintf("<tr><td><strong>WarningCode</strong></td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", row$warningCode),
                "</table>",
                "<h2>Message</h2>",
                sprintf("<p>%s</p>", row$message)
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
                ui = HTML(paste(lines, collapse = "\n"))
            )
        }
    })




    # Observer for the file upload event.
    observeEvent(input$fileUpload, {
        req(input$fileUpload)
        allLogs <- tibble()
        withProgress(message = "Creating Log DF", value = 0, {
            for (x in 1:length(input$fileUpload$datapath)) {
                tryCatch(
                    {
                        filePath <- input$fileUpload$datapath[x]
                        x <- input$fileUpload$name[x]
                        fileConnection <- file(filePath, open = "r")
                        lines <- readLines(fileConnection)
                        close(fileConnection)

                        if (!is.null(lines)) {
                            result <- parseLines(lines) %>%
                                mutate(fileName = basename(x)) %>%
                                mutate(fileNameParts = list(str_split(basename(x), "_") %>%
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
                        incProgress(1 / length(input$fileUpload$datapath), detail = paste("Processed file ", x))
                        message("Some other message at the end.Processed file ", x)
                    }
                )
            }
        })

        values$clinic_info_df <- allLogs %>%
            filter(str_detect(Message, "extract_country_clinic_code")) %>%
            mutate(
                clinic_code = gsub(".* clinic ([^.]{2}).*", "\\1", Message),
                country_code = gsub(".* country ([^.]{2}).*", "\\1", Message),
                tracker_in_fileName = gsub("logs_\\d{4}_(.*?)\\ A4D.*", "\\1", fileName)
            ) %>%
            select(tracker_in_fileName, clinic_code) %>%
            distinct(tracker_in_fileName, .keep_all = TRUE) %>%
            arrange(clinic_code) %>%
            left_join(read_csv(here::here("reference_data/clinic_data_static.csv")), by = "clinic_code", relationship = "many-to-many")

        values$eventLog <- allLogs %>%
            mutate(tracker_in_fileName = gsub("logs_\\d{4}_(.*?)\\ A4D.*", "\\1", fileName)) %>%
            left_join(values$clinic_info_df, by = "tracker_in_fileName", relationship = "many-to-many") %>%
            select(-"tracker_in_fileName")

        values$eventLog <- values$eventLog %>%
            dplyr::filter(substr(Message, 1, 1) == "{") %>%
            mutate(Message = map(Message, ~ jsonlite::fromJSON(.) %>%
                lapply(., function(x) if (is.null(x)) NA else if (is.list(x)) toString(x) else x) %>%
                as_tibble())) %>%
            unnest(Message) %>%
            rename(Message = message) %>%
            mutate(across(c("file", "errorCode", "warningCode", "functionName"), factor))

        values$proccessedFilesInfo <- allLogs %>%
            filter(str_detect(pattern = "Found", Message)) %>%
            extract(Message, into = c("number", "type"), regex = "Found (\\d+) (\\w+)") %>%
            select(fileName, type, number) %>%
            distinct(fileName, type, number)
        write_parquet(allLogs, "temp.parquet")
    })


    # Render the UI for the "Load Last Saved Log-Data" button if the "temp.parquet" file exists.
    output$loadTempFileUI <- renderUI({
        if (file.exists("temp.parquet")) {
            actionButton("loadTempFile", "Load Last Saved Log-Data")
        } else {
            NULL
        }
    })

    output$sankey <- renderPlotly({
        req(values$proccessedFilesInfo)


        fig <- plot_ly(
            type = "sankey",
            orientation = "h",
            node = list(
                label = unique(paste(values$proccessedFilesInfo$fileName, values$proccessedFilesInfo$type)),
                color = c("blue", "blue", "blue", "blue", "blue"),
                pad = 15,
                thickness = 20,
                line = list(
                    color = "black",
                    width = 0.5
                )
            ),
            link = list(
                source = c(0, 0, 0, 1, 2),
                target = c(0, 1, 2, 3, 4),
                value =  values$proccessedFilesInfo$number
            )
        )
        fig <- fig %>% layout(
            title = "Basic Sankey Diagram",
            font = list(
                size = 10
            )
        )
        fig
    })


    # Render the log table using the DataTable library.
    output$logTable <- renderDT({
        req(values$eventLog)

        options <- list(
            searching = TRUE,
            autoWidth = TRUE,
            paging = TRUE,
            pageLength = 20,
            lengthChange = T,
            dom = '<"top" p>tB',
            buttons = c("copy", "csv", "excel"),
            scrollX = T,
            ordering = T,
            info = T,
            columnDefs = list(list(
                targets = c(5),
                render = JS(truncScript)
            ))
        )
        selection <- list(mode = "single", target = "row")
        table <- datatable(values$eventLog,
            extensions = "Buttons",
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


    observeEvent(input$logTable_rows_all, {
        req(values$eventLog)
        values$currentTable <- values$eventLog[input$logTable_rows_all, ]
    })

    # Render the status table to display the count of log entries by file name and level.
    output$status <- renderDT(
        {
            req(values$eventLog)
            values$currentTable %>% dplyr::count(fileName, Level)
        },
        filter = list(
            position = "top", clear = FALSE
        )
    )

    output$logOverviewPlot <- renderPlotly({
        req(values$eventLog)
        aggregatedData <- values$currentTable %>%
            dplyr::count(fileName, Level) %>%
            filter(grepl(input$regexFilter, fileName))
        p <- ggplot(data = aggregatedData, aes(x = Level, y = fileName, fill = n)) +
            geom_tile() +
            scale_y_discrete(label = abbreviate) +
            theme(axis.text = element_text(size = 6))
        plotly::ggplotly(p, height = 800, width = 600)
    })

    output$trackerSummary <- renderDT({
        req(values$eventLog)
        datatable(
            values$eventLog %>% group_by(fileName, Level) %>% count() %>%
                pivot_wider(names_from = Level, values_from = n),
            filter = list(
                position = "top", clear = FALSE
            ),
            extensions = "Buttons",
            options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                dom = "tB",
                buttons = c("copy", "csv", "excel")
            ),
            rownames = FALSE,
            escape = FALSE
        )
    })

    # Display reference_data df in tabItem Reference_Data
    output$ref_data_df <- renderDT({
        req(values$clinic_info_df)
        clinic_info_df <- values$clinic_info_df %>% select(-clinic_start_date, -clinic_quota_22)

        missing_entries_df <- clinic_info_df %>%
            filter(is.na(country_name))
        multiple_entries_df <- clinic_info_df %>%
            group_by(clinic_code) %>%
            filter(n() > 1)

        col_filter <- switch(input$choose_tab,
            "all" = clinic_info_df,
            "missing entries" = missing_entries_df,
            "multiple entries" = multiple_entries_df
        )
        DT::datatable(
            {
                col_filter
            },
            extensions = "Buttons",
            options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                dom = "tB",
                buttons = c("copy", "csv", "excel")
            ),
            class = "display"
        )
    })
})
