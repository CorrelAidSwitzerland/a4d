# This file contains the user interface code for the Log File Viewer - A4D application.
# It uses the shiny and shinydashboard libraries to create a dashboard-style interface.
# The UI consists of a header, sidebar, and body, with multiple tabs for different views.
# The user can upload log files, select a log level, and view the log data in a table.
# There is also a details section for displaying additional information about selected log entries.
# The "Overview" tab provides a summary view of the log data.
# The fileInput and selectInput functions are used to create file upload and level selection controls, respectively.
# The DTOutput and DT functions from the DT library are used to render the log table and status table.


shinyUI(
  dashboardPage(
    dashboardHeader(title = "Log File Viewer - A4D"),
    dashboardSidebar( fluidRow(
      uiOutput( "loadTempFileUI"),
             fileInput(
               "fileUpload",
               "Upload Log Files",
               multiple = TRUE, accept = ".log")),


    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Overview", tabName = "overview", icon = icon("th")),
      menuItem("Reference_Data", tabName = "ref_data", icon = icon("clipboard"))
    )),
    dashboardBody(
      tabItems(
      tabItem(tabName = "dashboard",

      fluidRow(
        box(width = 6,
          title = "Main Table",
        DTOutput("logTable")),
        box(width = 6,
            title = "Tracker Overview",
            DTOutput("trackerSummary"),
            plotlyOutput("sankey")),



      )),
      tabItem(tabName = "overview",
              fluidRow(box(
                title = "Overview",
              DTOutput("status")
              )),
              fluidRow(box(title= "Overview-Plot", height=1000, weight=1000,
                           textInput("regexFilter",label = "RegexFilter Trackerfiles"),
                  plotlyOutput( "logOverviewPlot")))
      ),

      tabItem(tabName = "ref_data",
              fluidPage(
                  sidebarLayout(
                      sidebarPanel(
                          h3("Checking reference_data"),
                          br(),
                          p("We use reference_data to match clinic information
                          to clinic_code used in messages."),
                          radioButtons("choose_tab",
                                             label = "Choose Output",
                                             choices = c("all", "missing entries", "multiple entries"),
                                             selected = "all"
                                             )
                      ),
                      mainPanel(
                          tableOutput("ref_data_df")

                      )
              ))

              )
      ))

      )
    )


