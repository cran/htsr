library(shiny)
library(shinyFiles)
library(tibble)
library(dplyr)
library(lubridate)
library(waiter)

tstep <- c("monthly", "daily", "12h", "6h", "3h", "2h", "hourly", "30mn",
           "10mn", "5mn")
lmode <- c("average", "max", "min", "sum")

# Define UI
fluidPage(
  
  waiter::use_waiter(),
  titlePanel("Calculation of fixed time-step files"),
  
  fluidRow(
    shinyFilesButton("file", "File upload", "Please select hts files in the same folder", 
                       multiple = FALSE, viewtype = "detail", class="btn btn-primary"),
  textOutput("ff"),
  br()
  ),
  
  fluidRow(
    column(width = 4,
      selectInput("ts", "Time-step", tstep, selected = "daily"),
      numericInput("shift", "If daily, shift (hours)",0,0,23,1)
    ),
    column(width = 4,
      selectInput("mode", "Mode", lmode, selected = "average")
    ),
    column(width = 4,
      p(strong("If monthly")),
      checkboxInput("climedit", "climatogy file"),
      checkboxInput("rmna", "remove NA"),
      checkboxInput("gapfill", "gapfilling"),
      checkboxInput("hts_year", "extract year stat")
    )
  ),
  
  fluidRow(
    verbatimTextOutput("MESS2"),
    actionButton("submit", "Submit", class = "btn btn-warning"),
    br(),
    textOutput("mon"),
    textOutput("MESS"),
    textOutput("MESS1"),
    hr(),
    actionButton("close", "Done", class = "btn btn-danger")
  )
  
)