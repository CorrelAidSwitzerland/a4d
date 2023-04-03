library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(
    title = HTML("A4D Quality Checks")
  ),
  
  
  dashboardSidebar(
    disable = TRUE
  ),
  
  
  dashboardBody(
    
    fluidRow(
      fileInput("xlsx_import",
                label="Upload xlsx here",
                multiple = TRUE)
    )
  )
)

server <- function(input, output) { 
  
  data_raw <- reactiveVal(NULL)
  
  
  # Observer Import Data
  observeEvent(input$xlsx_import, {
    inFile <- input$xlsx_import
    if (!is.null(inFile)) {   
      dataFile <- read_excel(inFile$datapath, sheet=1)
      dat <- data.frame(EndoPaste(dataFile)[1], stringsAsFactors=FALSE)
      data_raw(dat)
    }
    
    
  })
  
  }

shinyApp(ui, server)