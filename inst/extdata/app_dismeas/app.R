# This is a Shiny web application

library(shiny)
library(tidyverse)
library(lubridate)
library(editData)
library(RSQLite)

# load data
load(file=system.file("extdata/fichier_discalib.RData",package="htsr"))
conn <- dbConnect(SQLite(),fsq)
sta1 <- paste("'",as.character(sta),"'",sep="")
sen1 <- paste("'",as.character(sen),"'",sep="")
selection <- paste ("SELECT * FROM DM WHERE Id_Station =",sta1, "AND Capteur =", sen1)
x <- dbGetQuery(conn, selection)
dbDisconnect(conn)
myx <- tibble(x)
myx <- select(myx, Date, H, Q, Date_Debut, Date_Fin, H_Debut, H_Fin, Auteur_Jaugeage,
  Auteur_Depouillement, Mode_Operatoire, Mode_Depouillement)
colnames(myx) <- c("Date", "H", "Q", "StartDate", "EndDate", "StartH", "EndH", "MeasureAuthor",
                   "ProcessAuthor", "OperationMode", "ProcessMode")
myx$Date <- as_datetime(myx$Date)
myx$StartDate <- as_datetime(myx$StartDate)
myx$EndDate<- as_datetime(myx$EndDate)


# Define UI ------
ui <- fluidPage(

  titlePanel("add, modify or remove discharge measurement"),
  strong(textOutput("FSQ")),
  strong(textOutput("STA")),
  strong(textOutput("SEN")),
  br(),

  mainPanel(
      editData::editableDTUI("table"),
      br(),
      actionButton("save", "Save")
  )
)


# Define server logic -----
server <- function(input, output) {
  output$FSQ <- renderText({paste("Data base: ",basename(fsq))})
  output$STA <- renderText({paste("Station:", sta)})
  output$SEN <- renderText({paste("Sensor:", sen)})

  df2 = callModule(editData::editableDT,"table",data=reactive(myx))

  observeEvent(input$save, {
  #   myconf <- tibble (title = input$title,  yaxis_label = input$yaxis,
  #                     norm_val = input$normval, fix_time = input$fixtime,
  #                     dstart = as.character(input$daterange[1]), dend = as.character(input$daterange[2]),
  #                     fix_scale = input$fixy, ymin = input$ymin, ymax = input$ymax,
  #                     lin_trend = input$trend, facet = input$facet)
  #   conf <- myconf
  xt <- df2()
  #   save(nbst, fil, conf, mapalette, tzo, file = system.file("extdata/settings.RData",package="htsr"))
  })
  #

}

# Run the app
shinyApp(ui = ui, server = server)
