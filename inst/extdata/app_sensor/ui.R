library(shiny)
library(shinyFiles)
library(RSQLite)

# Define UI
fluidPage(

  titlePanel("Create, remove or modify a sensor"),
  
  fluidRow(
    shinyFilesButton("file", "File select", "Please select a sqlite data base", 
                     multiple = FALSE, viewtype = "detail", class ="btn btn-primary"),
    textOutput("fsq"),    
    hr()
  ),

  sidebarLayout(
    sidebarPanel(width= 7,
      div("For accessing to more fields than those displayed, use the detailed function",
        "d_sensor ()"), br(),
      splitLayout(
        textInput("Id_Station", "Station ID*"),
        textInput("Id_Sensor", "Sensor ID*")),
      splitLayout(
        actionButton("confirmst", "Confirm station", class = "btn btn-info"),
        actionButton("confirmss", "Confirm sensor", class = "btn btn-info")
      ),
      br(),
      div("Confirm station and sensor before ANY operation!"), br(),

      radioButtons("op", "Operation (CAUTION with REMOVE, it is definitive !)",
                   c("Create", "Modify", "Remove")),
      
      conditionalPanel(
        condition = "input.op != 'Create'",
        textOutput("tab1")
      ),
      
      conditionalPanel(
        condition = "input.op == 'Create'",
          radioButtons("tab", "Table (cannot be modified after creation)", c("Water levels", "Discharges",
            "Quality", "Precipitation", "Weather")),
      ),
      br(),
      
      actionButton("confirm2", "Confirm operation", class = "btn btn-warning"), br(),
      hr(),
      actionButton("close", "Done", class = "btn btn-danger")
    ),

    mainPanel(width= 5,
      splitLayout(
        textInput("nature", "Nature"),
        textOutput("nature1")),
      splitLayout(
        textInput("description", "Description"),
        textOutput("description1")),
      splitLayout(
        textInput("comment", "Comment"),
        textOutput("comment1")),
      textOutput("Message1"),
      textOutput("Message2")
    )
  )
)



