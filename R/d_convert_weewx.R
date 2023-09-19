#' @title Convert a weewx data base into a htsr sqlite base
#'
#' @author P. Chevallier - Feb 2018 - Sep 2023
#'
#' @description Convert (or update) a weewx data base into a htsr sqlite base
#'
#' @param db.weewx Full name of the weewx data base
#' @param fsq Full name of the htsr data base
#' @param update (default = TRUE)
#' @param sta Station id (default = NA)
#' @param name_st Station name (default = NA)
#'
#' @details If update is TRUE, sta and name_st are unnecessary. I update is FALSE and
#' fsq is NA, fsq is named "weewx.sqlite".
#'
#'
#'
#' @examples \dontrun{
#'
#' d_convert_weewx("weewx.sql", "foo.sqlite")
#' }
#'

d_convert_weewx <- function(db.weewx, fsq = NA, update=TRUE, sta = NA, name_st = NA){

  # Warnings
  dn <- dirname(db.weewx)
  if(is.na(fsq) && !(update)) fsq <- paste0(dn,"/weewx.sqlite")
  if(update) {
  	if ((!file.exists(db.weewx) || !file.exists(fsq)))
    return(warning("\nWeewx and/or htsr database(s) don't exist, when an update is requested."))
	 } else {
	 	if((is.na(sta) || is.na(name_st)))
		return(warning("\nWhen update is false, station id and name are requested."))
	 }

  # Init ou recup fsq data base
  if(update) {
  	conn <- RSQLite::dbConnect(RSQLite::SQLite(), fsq)
  	selection <- paste ("SELECT Id_Station FROM ST" )
  	staa <- RSQLite::dbGetQuery(conn, selection)
  	sta <- staa[[1]]
  	selection <- paste ("SELECT Nom FROM ST" )
  	name_sta <- RSQLite::dbGetQuery(conn, selection)
  	name_st <- name_sta[[1]]
  	selection <- paste ("SELECT Date FROM WE" )
  	date <- RSQLite::dbGetQuery(conn, selection)
  	dmxwe <- max(date)
  	selection <- paste ("SELECT Date FROM PR" )
  	date <- RSQLite::dbGetQuery(conn, selection)
  	dmxpr <- max(date)
  	dmx <- max(dmxwe, dmxpr)
  	RSQLite::dbDisconnect(conn)
  } else d_create(fsq, cr_table = TRUE, bku = FALSE)


  # Recuperation base weewx
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),db.weewx)

  # I data
  l <- c("barometer", "inTemp", "outTemp", "inHumidity",
  "outHumidity", "windSpeed", "windDir", "windGust", "windGustDir", "rain")
  x <- NA
  for (i in 1:length(l)){
    selection <- paste ("SELECT dateTime FROM archive")
    date <- RSQLite::dbGetQuery(conn, selection)
    date <- date$dateTime
    date <- as.POSIXct(date, origin = "1970-01-01")
    selection <- paste ("SELECT", l[i],  "FROM archive")
    value <- RSQLite::dbGetQuery(conn, selection)
    value <- value[,1]
    if (l[i] == "barometer") {value <- value * 33.864 ; sensor <- "IPA"}
    if (l[i] == "inTemp") {value <- (value-32) /1.8 ; sensor <- "ITAi"}
    if (l[i] == "outTemp") {value <- (value-32) /1.8 ; sensor <- "ITAo"}
    if (l[i] == "inHumidity") {sensor <- "IHRi"}
    if (l[i] == "outHumidity") {sensor <- "IHRo"}
    if (l[i] == "windSpeed") {value <- value * 0.45 ; sensor <- "IWV"}
    if (l[i] == "windDir") {sensor <- "IWD"}
    if (l[i] == "windGust") {value <- value * 0.45 ; sensor <- "IWG"}
    if (l[i] == "windGustDir") {sensor <- "IWGD"}
    if (l[i] == "rain") {value <- value * 25.4 ; sensor <- "IPR"}
    t <- tibble(date,value,sensor)
    if (i == 1) x <- t
    else x <- bind_rows(x,t)
  }

  # J outTemp
  l <- c("min", "max")
  for (i in 1:length(l)){
    selection <- paste ("SELECT dateTime FROM archive_day_outTemp")
    date <- RSQLite::dbGetQuery(conn, selection)
    date <- date$dateTime + 43200
    date <- as.POSIXct(date, origin = "1970-01-01")
    selection <- paste ("SELECT", l[i],  "FROM archive_day_outTemp")
    value <- RSQLite::dbGetQuery(conn, selection)
    value <- value[,1]
    if (l[i] == "min") {value <- (value-32) /1.8 ; sensor <- "JTAn"}
    if (l[i] == "max") {value <- (value-32) /1.8 ; sensor <- "JTAx"}
    t <- tibble(date,value,sensor)
    x <- bind_rows(x,t)
  }

  # J windDir, inTemp, outTemp, windSpeed
  ka <- c("archive_day_inTemp", "archive_day_outTemp",
          "archive_day_windDir", "archive_day_windSpeed")
  kb <- c("JTAi","JTAo","JWD","JWV")
  for (j in 1:4){
    la <- c("sum", "count")
    for (i in 1:2){
      selection <- paste ("SELECT dateTime FROM", ka[j])
      date <- RSQLite::dbGetQuery(conn, selection)
      date <- date$dateTime + 43200
      date <- as.POSIXct(date, origin = "1970-01-01")
      selection <- paste ("SELECT", la[i],  "FROM", ka[j])
      value <- RSQLite::dbGetQuery(conn, selection)
      if (i==1) lb1 <- value[,1]
      else lb2 <- value[,1]
    }
    if(kb[j] %in% c("JTAi","JTAo"))
      value <- (lb1/lb2-32)/1.8 ; sensor <- kb[j]
    if(kb[j] == "JWV")
      value <- lb1/lb2*0.45 ; sensor <- kb[j]
    if(kb[j] == "JWD")
      value <- lb1/lb2 ; sensor <- kb[j]
    t <- tibble(date,value,sensor)
    x <- bind_rows(x,t)
  }

  # J windGust
  selection <- paste ("SELECT dateTime FROM archive_day_windGust")
  date <- RSQLite::dbGetQuery(conn, selection)
  date <- date$dateTime + 43200
  date <- as.POSIXct(date, origin = "1970-01-01")
  selection <- paste ("SELECT", "max",  "FROM archive_day_windGust")
  value <- RSQLite::dbGetQuery(conn, selection)
  value <- value[,1]
  value <- value * 0.45 ; sensor <- "JWG"
  t <- tibble(date,value,sensor)
  x <- bind_rows(x,t)

  # J rain
  selection <- paste ("SELECT dateTime FROM archive_day_rain")
  date <- RSQLite::dbGetQuery(conn, selection)
  date <- date$dateTime + 43200
  date <- as.POSIXct(date, origin = "1970-01-01")
  selection <- paste ("SELECT", "sum",  "FROM archive_day_rain")
  value <- RSQLite::dbGetQuery(conn, selection)
  value <- value[,1]
  value <- value * 25.4 ; sensor <- "JPR"
  t <- tibble(date,value,sensor)
  x <- bind_rows(x,t)

  # Deconnexion base weewx
  RSQLite::dbDisconnect(conn)

  nrx <- nrow (x)
  if (update) {
  	x <- filter(x, date > dmx)
  	nrx <- nrow (x)
  } else {
	  # Creation station
	  d_station(fsq, op = "C", sta = sta, name_st = name_st, ty_st="M", bku = FALSE)

	  # Creation des tables et des capteurs
	  l <- as.vector(c("IPA", "ITAi", "ITAo", "IHRi", "IHRo", "IWV", "IWD", "IWG", "IWGD",
	     "JTAn","JTAx","JTAi","JTAo","JWD","JWV","JWG"))
	  map(l, function(.x) d_sensor(fsq, op = "C", sta = sta, sen=.x,
	    table = "WE", bku = FALSE))
	  l <- as.vector(c("IPR", "JPR"))
	  map(l, function(.x) d_sensor(fsq, op = "C", sta = sta, sen=.x,
    table = "PR", bku = FALSE))
  }

  # Chargement des donnees weewx dans la base sqlite
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),fsq)

  l <- c("JTAn","JTAx","JTAi","JTAo","JWD","JWV","JWG","JPR","IPR","IPA", "ITAi", "ITAo",
    "IHRi", "IHRo", "IWV", "IWD", "IWG", "IWGD")
  for (i in 1:length(l)) {
    t <- dplyr::filter(x, sensor == l[i])
    date <- t$date
    value <- t$value
    if (l[i] %in% c("IPR","JPR")) {
      mt <- list(Type_Station = as.character("M"),
      Id_Station = as.character(sta), Capteur= as.character(l[i]),
      Tabl= as.character("PR"), Date = date, Valeur = value,
      Origine= as.character(NA), Qualite= as.character(NA))
      mt <- as.data.frame(mt, stringsAsFactors = FALSE)
      DBI::dbWriteTable(conn, name="PR", mt, append = TRUE)
    } else {
      mt <- list(Type_Station = as.character("M"),
      Id_Station = as.character(sta), Capteur= as.character(l[i]),
      Tabl= as.character("WE"), Date = date, Valeur = value,
      Origine= as.character(NA), Qualite= as.character(NA))
      mt <- as.data.frame(mt, stringsAsFactors = FALSE)
      DBI::dbWriteTable(conn, name="WE", mt, append = TRUE)
    }
    message("\nSensor ",l[i]," completed")
  }

  #Deconnexion base sqlite
  RSQLite::dbDisconnect(conn)
  if (update) message("\nBase ", fsq, " completed with ", nrx, " added records.")
  else message("\nBase ", fsq, " created with ", nrx, " records.")

}

#Fin
