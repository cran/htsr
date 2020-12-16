#
library(shiny)
library(RSQLite)
library(htsr)

load(file=system.file("extdata/fichier_fsq.RData",package="htsr"))

# Define UI
ui <- fluidPage(

    titlePanel("Create, remove or modify a station"),

    sidebarLayout(
        sidebarPanel(width= 5,
          h4("Sqlite data base:"),
          textOutput("FSQ"), br(),
          div("For accessing to more fields than those displayed, use the detailed function",
              "d_station ()"), br(),
          textInput("Id_Station", "Station ID (required)"),
          actionButton("confirm1", "Confirm station"), br(),
          div("Confirm station before ANY operation!"), br(),
          splitLayout(
            radioButtons('typ', "Type", c("Hydro", "Meteo")),
            textOutput("typ1")),
          div("The type cannot by modified after creation!"), br(),
          splitLayout(
            textInput("Nom", "Name"),
            textOutput("Nom1")),
          radioButtons('op', "Operation", c("Create", "Modify", "Remove")),
          actionButton("confirm2", "Confirm operation"), br(),
          actionButton("close", "Done")
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
            textOutput("Resultat")
         )
    )
  )


# Define server
server <- function(input, output) {

  # fonction exsta
  exsta <- function (db.sqlite, stationID){
    library(RSQLite)
    conn <- RSQLite::dbConnect(RSQLite::SQLite(),db.sqlite)
    sel <- paste0 ("SELECT * FROM ST  WHERE Id_Station = '", stationID,"'")
    lsta <- RSQLite::dbGetQuery(conn, sel)
    nom_sta <- lsta$Nom
    pays <- lsta$Pays
    zon <- lsta$Zone
    sszon <- lsta$SousZone
    grbas <- lsta$GrandBassin
    bas <- lsta$Bassin
    ptbas <- lsta$PetitBassin
    riv <- lsta$Riviere
    lon <- lsta$Longitude
    lat <- lsta$Latitude
    alt <- lsta$Altitude
    bv <- lsta$Superficie_bv
    ges <- lsta$Gestionnaire
    typ <- lsta$Type_Station
    x <- c(nom_sta, pays, zon, sszon, grbas, bas, ptbas, riv, lat, lon, alt, bv, ges, typ)
    RSQLite::dbDisconnect(conn)
    return(x)
  }

  # data base
  output$FSQ <- renderText({basename(fsq)})

  # confirm station
  observeEvent(input$confirm1, ({
    stationID = input$Id_Station
    x <- exsta(db.sqlite=fsq, stationID)
    if(is.na(x[1])) output$Message <- renderText({
      "The station not exists in the data base, but it can be created.
      in that case a name is requested!"})
    output$Nom1 <- renderText({x[1]})
    output$Pays1 <- renderText({x[2]})
    output$Zone1 <- renderText({x[3]})
    output$SousZone1 <- renderText({x[4]})
    output$GrandBassin1 <- renderText({x[5]})
    output$Bassin1 <- renderText({x[6]})
    output$PetitBassin1 <- renderText({x[7]})
    output$Riviere1 <- renderText({x[8]})
    output$Longitude1 <- renderText({paste0(as.numeric(x[9]))})
    output$Latitude1 <- renderText({paste0(as.numeric(x[10]))})
    output$Altitude1 <- renderText({paste0(as.numeric(x[11]))})
    output$Superficie_bv1 <- renderText({paste0(as.numeric(x[12]))})
    output$Gestionnaire1 <- renderText({x[13]})
    output$typ1 <- renderText({x[14]})
  }))

  # confirm operation
  observeEvent(input$confirm2, ({
    sta = input$Id_Station
    name_fld = c("Pays", "Zone", "SousZone",
                 "GrandBassin", "Bassin", "PetitBassin", "Riviere",
                 "Longitude", "Latitude", "Altitude", "Superficie_bv",
                 "Gestionnaire")
    x <- exsta(db.sqlite=fsq, stationID = sta)
    if(is.na(x[1])) {
      output$Message <- renderText({
        "The station not exists in the data base, but it can be created.
        in that case a name is requested!"
      })
    }

    # Create
    if(input$op == "Create") {
      ope <- "C"
      name_st = input$Nom
      if(input$typ == "Hydro") ty_st <- "H" else ty_st <- "M"
      value_fld = c(input$Pays,
        input$Zone, input$SousZone,
        input$GrandBassin, input$Bassin,
        input$PetitBassin, input$Riviere,
        as.numeric(input$Longitude),
        as.numeric(input$Latitude),
        as.numeric(input$Altitude),
        as.numeric(input$Superficie_bv),
        input$Gestionnaire)
      for (i in 1:12)
        if (is.null (value_fld[i])) value_fld[i] <- NA
    }

    # Remove
    if(input$op == "Remove") {
      ope <- "R"
      name_st = NA
      value_fld = NA
    }

    # Modify
    if(input$op == "Modify") {
      ope <- "M"
      for (i in 2:13) if (is.null (x[i])) x[i] <- NA
      if(input$Nom != "") name_st <- input$Nom else name_st <- x[1]
      if(input$Pays != "") pays1 <- input$Pays else pays1 <- x[2]
      if(input$Zone != "") zone1 <- input$Zone else zone1 <- x[3]
      if(input$SousZone != "") souszone1 <- input$SousZone else souszone1 <- x[4]
      if(input$GrandBassin != "") grandbassin1 <- input$GrandBassin else grandbassin1 <- x[5]
      if(input$Bassin != "") bassin1 <- input$Bassin else bassin1 <- x[6]
      if(input$PetitBassin != "") petitbassin1 <- input$PetitBassin else petitbassin1 <- x[7]
      if(input$Riviere != "") riviere1 <- input$Riviere else riviere1 <- x[8]
      if(input$Longitude != "") longitude1 <- as.numeric(input$Longitude) else longitude1 <- x[9]
      if(input$Latitude != "") latitude1 <- as.numeric(input$Latitude) else latitude1 <- x[10]
      if(input$Altitude != "") altitude1 <- as.numeric(input$Altitude) else altitude1 <- x[11]
      if(input$Superficie_bv != "") superficie1 <- as.numeric(input$Superficie_bv) else superficie1 <- x[12]
      if(input$Gestionnaire != "") gestionnaire1 <- input$Gestionnaire else gestionnaire1 <- x[13]
      value_fld = c(pays1, zone1, souszone1, grandbassin1, bassin1, petitbassin1,
        riviere1, longitude1, latitude1, altitude1, superficie1, gestionnaire1)
    }

    # Operation
    result <- htsr::d_station(db.sqlite=fsq, op = ope, sta = sta,
      ty_st = ty_st, name_st = name_st, name_fld = name_fld,
      value_fld = value_fld, bku = TRUE)
    output$Resultat <- renderText({result})

  }))



  #STOP
    observeEvent(input$close, stopApp())
}

# Run the application
shinyApp(ui = ui, server = server)

