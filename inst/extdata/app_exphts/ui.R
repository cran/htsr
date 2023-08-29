library(shiny)
library(shinyFiles)

fluidPage(
  theme = NULL,
  titlePanel("hts file import"),

  fluidRow(sidebarLayout(
    sidebarPanel(
      width = 5,
      shinyFilesButton("file", "File select", "Please select a sqlite data base", 
        multiple = FALSE, viewtype = "detail", class = "btn btn-primary"),
      # tags$hr(),
      # shinySaveButton("save", "Save file", "Save file as...", 
      #   filetype = list(sqlite = "sqlite", RData = "RData"), viewtype = "icon")
      # ),
      
      # textInput('folder', "Working directory"),
      textInput("Station_id", "Station ID"),
      textInput("Sensor_id", "Sensor ID"),
      checkboxInput("Set_time", "Reduce time interval", value = FALSE),
      dateRangeInput("dates", "Time interval range"),
      actionButton("submit", "Submit", class = "btn btn-warning"),
      radioButtons(
        "plot_opt",
        br("Plot the extracted file"),
        c("line" = "line", "bar" = "bar"),
        selected = "line"
      ),
      actionButton("plot", "Plot", class = "btn btn-success"),
      hr(),
      actionButton("close", "Done", class="btn btn-danger")
    ),
    mainPanel(
      width = 7,
      # tags$h5("Output of the file selection"),
      # verbatimTextOutput("filepaths"),
      # tableOutput("tab"),
      textOutput("fsq"),
      textOutput("extract_written"),
      plotOutput("file_plot")
    )
  ))
  
)