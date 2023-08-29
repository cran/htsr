library(shiny)
library(shinyFiles)
library(htsr)

# Define UI
fluidPage(
  theme = NULL,  
  waiter::use_waiter(),
  
  titlePanel("Data base inventory"),
  
  sidebarLayout(
    sidebarPanel(width = 5,
      shinyFilesButton("file", "File select", "Please select a sqlite data base", 
        multiple = FALSE, viewtype = "detail", class="btn btn-primary"),
      br(), br(),      
      div("Station_id blank: all stations are displayed"),
      div("Station_id filled: its sensors are displayed"),

      textInput("Station_id", "Station ID"),
      actionButton("station", "Submit", class="btn btn-warning"),
      br(),br(),
      actionButton("close", "Done", class="btn btn-danger")
    ),
  
    mainPanel(width =7,
      textOutput("fsq"),
      br(),
      tableOutput("station")
    )
  )
)