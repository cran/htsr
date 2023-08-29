library(shiny)
library(shinyFiles)
library(RSQLite)

fluidPage(

  titlePanel("Create, remove or modify a station"),

  fluidRow(
    shinyFilesButton("file", "File select", "Please select a sqlite data base", 
                     multiple = FALSE, viewtype = "detail", class ="btn btn-primary"),
    textOutput("fsq"),    
    hr()
  ),
  
  sidebarLayout(
    sidebarPanel(width= 5,
      div("For accessing to more fields than those displayed, use the detailed function"),
      textInput("Id_Station", "Station ID*"),
      actionButton("confirm1", "Confirm station", class = "btn btn-info"), br(),
      div("Confirm station before ANY operation!"), br(),
      textInput("Nom", "Name*"),
      
      conditionalPanel(
        condition = ("input.op != 'Create'"),
        textOutput("Nom1"),
        textOutput("typ1"),
      ),

      radioButtons('op', "Operation", c("Create", "Modify", "Remove")),

      conditionalPanel(
        condition = ("input.op == 'Create'"),
        radioButtons('typ', "Type", c("Hydro", "Meteo"))
      ),
 
      
      actionButton("confirm2", "Confirm operation", class = "btn btn-warning"), 
      br(),br(),
      actionButton("close", "Done", class = "btn btn-danger")
    ),
    mainPanel(width= 7,
         splitLayout(
          textInput("Pays", "Country"),
          textOutput("Pays1")),
      splitLayout(
          textInput("Zone", "Zone"),
          textOutput("Zone1"),
          textInput("SousZone", "Sub-zone"),
          textOutput("SousZone1")),
      splitLayout(
          textInput("GrandBassin", "Large Basin"),
          textOutput("GrandBassin1"),
          textInput("Bassin", "Basin"),
          textOutput("Bassin1")),
      splitLayout(
          textInput("PetitBassin", "Small basin"),
          textOutput("PetitBassin1"),
          textInput("Riviere", "River"),
          textOutput("Riviere1")),
      splitLayout(
          textInput("Longitude", "Longitude  (deg.)", value = NA),
          textOutput("Longitude1"),
          textInput("Latitude", "Latitude (deg.)", value = NA),
          textOutput("Latitude1")),
      splitLayout(
          textInput("Altitude", "Altitude (m)", value = NA),
          textOutput("Altitude1"),
          textInput("Superficie_bv", "Basin area (km2)", value = NA),
          textOutput("Superficie_bv1")),
      splitLayout(
          textInput("Gestionnaire", "Manager"),
          textOutput("Gestionnaire1")),
      br(),
      textOutput("Message"), br(),
      verbatimTextOutput("Resultat")
    )
  )
)


