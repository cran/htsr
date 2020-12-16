#
library(shiny)
library(RSQLite)
library(htsr)

load(file=system.file("extdata/fichier_fsq.RData",package="htsr"))

# Define UI
ui <- fluidPage(

    titlePanel("Create, remove or modify a sensor"),

    sidebarLayout(
        sidebarPanel(width= 5,
          h4("Sqlite data base:"),
          textOutput("FSQ"), br(),
          div("For accessing to more fields than those displayed, use the detailed function",
              "d_sensor ()"), br(),
          splitLayout(
            textInput("Id_Station", "Station ID (required)"),
            textInput("Id_Sensor", "Sensor ID (required)")),
          splitLayout(
            actionButton("confirmst", "Confirm station"),
            actionButton("confirmss", "Confirm sensor")),
            br(),
          div("Confirm station and sensor before ANY operation!"), br(),
          splitLayout(
            radioButtons("tab", "Table", c("Water levels", "Discharges",
              "Quality", "Precipitation", "Weather")),
            textOutput("tab1")),
          div("The table cannot by modified after creation!"), br(),
          radioButtons("op", "Operation", c("Create", "Modify", "Remove")),
          actionButton("confirm2", "Confirm operation"), br(),
          div("CAUTION with REMOVE, it is definitive !!! In case of mistake,
          use the backup file."),
          actionButton("close", "Done")
        ),
        mainPanel(width= 7,
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


# Define server
server <- function(input, output) {

  # data base
  output$FSQ <- renderText({basename(fsq)})


  # confirm station and sensor
  observeEvent(input$confirmst, ({
    sta = input$Id_Station
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), fsq)
    sel <- paste0 ("SELECT * FROM ST  WHERE Id_Station = '", sta, "'")
    lsta <- RSQLite::dbGetQuery(conn, sel)
    stID <- (nrow(lsta) != 0)
    if(stID) {
      nom <- lsta$Nom
      output$Message1 <- renderText({paste0("Station ", sta, " / ", nom, ": OK")})}
    else output$Message1 <- renderText({paste("The station not exists for the
      data base, but it can be created. Use the function ds_station!")})
    RSQLite::dbDisconnect(conn)
    }))

  observeEvent(input$confirmss, ({
    sta = input$Id_Station
    sen = input$Id_Sensor
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), fsq)
    sel <- paste0 ("SELECT * FROM SS  WHERE Id_Station = '", sta, "'")
    lsen <- RSQLite::dbGetQuery(conn, sel)
    seID <- (sen %in% lsen$Capteur)
    if (seID) {
      output$Message2 <- renderText({"Sensor OK"})
      tab <- lsen$Tabl
      nature <- lsen$Nature
      description <- lsen$Description
      comment <- lsen$Commentaire
    } else {
      output$Message2 <- renderText({"The sensor not exists for the station,
      but it can be created. In that case a table is requested! CAUTION: this
        cannot be changed later! In case of mistake, use the backup file."})
      tab <- nature <- description <- comment <- NA
    }
    output$tab1 <- renderText({tab})
    output$nature1 <- renderText({nature})
    output$description1 <- renderText({description})
    output$comment1 <- renderText({comment})
    RSQLite::dbDisconnect(conn)
  }))

  # confirm operation
  observeEvent(input$confirm2, ({
    sta = input$Id_Station
    sen = input$Id_Sensor
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), fsq)
    sel <- paste0 ("SELECT * FROM SS  WHERE Id_Station = '", sta,
      "' AND Capteur = '", sen, "'")
    lsen <- RSQLite::dbGetQuery(conn, sel)
    tab <- lsen$Tabl
    name_fld = c("Nature", "Description", "Commentaire")
    if(is.null(lsen$Nature)) nature1 <- NA else nature1 <- lsen$Nature
    if(is.null(lsen$Description)) description1 <- NA else description1 <- lsen$Description
    if(is.null(lsen$Commentaire)) comment1 <- NA else comment1 <- lsen$Commentaire

    # Create
    if(input$op == "Create") {
      ope <- "C"
      if(input$tab == "Water levels") tab <- "WL"
      if(input$tab == "Discharges") tab <- "DI"
      if(input$tab == "Quality") tab <- "QU"
      if(input$tab == "Precipitation") tab <- "PR"
      if(input$tab == "Weather") tab <- "WE"
      value_fld = c(input$nature, input$description, input$comment)
      for (i in 1:3)
        if (is.null (value_fld[i])) value_fld[i] <- NA
    }

    # Remove
    if(input$op == "Remove") {
      ope <- "R"
      value_fld = NA
    }

    # Modify
    if(input$op == "Modify") {
      ope <- "M"
      if(input$nature != "") nature1 <- input$nature
      if(input$description != "") description1 <- input$description
      if(input$comment != "") comment1 <- input$comment
      value_fld = c(nature1, description1, comment1)
    }

    # Operation
    RSQLite::dbDisconnect(conn)
    htsr::d_sensor(db.sqlite=fsq, op=ope, sta=sta, sen=sen, table = tab, name_fld=name_fld,
      value_fld=value_fld, bku = TRUE)
  }))



  #STOP
    observeEvent(input$close, stopApp())
}

# Run the application
shinyApp(ui = ui, server = server)

