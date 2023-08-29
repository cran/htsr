library(shiny)
library(shinyFiles)
library(RSQLite)
library(htsr)

# fonction exsta
exsta <- function (fsq, stationID){
  library(RSQLite)
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),fsq)
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


# Define server
shinyServer (function(input, output, session) {
  options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyFileChoose(input, "file", roots = volumes, session = session, 
                  filetypes="sqlite")

  # Upload files
  observeEvent(input$file, {
    tabfile <- parseFilePaths(volumes, input$file)
    fsq <- as.character(tabfile[1,4])
    output$fsq <- renderText({fsq})

    # confirm station
    observeEvent(input$confirm1, ({
      req(input$file)
      tabfile <- parseFilePaths(volumes, input$file)
      fsq <- as.character(tabfile[1,4])
      sta = input$Id_Station
      
      x <- exsta(fsq=fsq , sta)
      
      if(is.na(x[1])) {
        output$Message <- renderText({
        "The station not exists in the data base, but it can be created 
        and a name is requested!"})
      } else {
        output$Message <- renderText({
        "The station exists in the data base"})
      }
      
      output$Nom1 <- renderText({x[1]})
      output$Pays1 <- renderText({x[2]})
      output$Zone1 <- renderText({x[3]})
      output$SousZone1 <- renderText({x[4]})
      output$GrandBassin1 <- renderText({x[5]})
      output$Bassin1 <- renderText({x[6]})
      output$PetitBassin1 <- renderText({x[7]})
      output$Riviere1 <- renderText({x[8]})
      long <- round(as.numeric(x[9],5))
      lati <- round(as.numeric(x[10],5))
      alti <- round(as.numeric(x[11],0))
      area <- round(as.numeric(x[12],0))
      output$Longitude1 <- renderText({as.character(long)})
      output$Latitude1 <- renderText({as.character(lati)})
      output$Altitude1 <- renderText({as.character(alti)})
      output$Superficie_bv1 <- renderText({as.character(area)})
      output$Gestionnaire1 <- renderText({x[13]})
      output$typ1 <- renderText({paste ("type :", x[14])})
    }))

    # confirm operation
    observeEvent(input$confirm2, ({
      req(input$file, input$confirm1)
      tabfile <- parseFilePaths(volumes, input$file)
      fsq <- as.character(tabfile[1,4])
      sta = input$Id_Station
      
      x <- exsta(fsq=fsq , sta)
      
      name_fld = c("Pays", "Zone", "SousZone",
                   "GrandBassin", "Bassin", "PetitBassin", "Riviere",
                   "Longitude", "Latitude", "Altitude", "Superficie_bv",
                   "Gestionnaire")
     
      # Create
      if(input$op == "Create") {
        ope <- "C"
        name_st <- input$Nom
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
        name_st <- input$Nom
        ope <- "R"
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
      
      result <- d_station(fsq=fsq , op = ope, sta = sta,
        ty_st = ty_st, name_st = name_st, name_fld = name_fld,
        value_fld = value_fld, bku = TRUE)
      
      output$Resultat <- renderPrint({"Done !"})
  
    }))
  })

  #STOP
  observeEvent(input$close, stopApp())
    
  # fonction exsta
  exsta <- function (fsq, stationID){
    library(RSQLite)
    conn <- RSQLite::dbConnect(RSQLite::SQLite(),fsq)
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
    
})


