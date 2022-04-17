#' @title Convert a weewx data base into a htsr sqlite base
#'
#' @author P. Chevallier - Feb 2018 - Jan 2022
#'
#' @description Convert a weewx data base into a tshm sqlite base
#'
#' @param fsq Full name of the htsr data base
#' @param db.weewx Full name of the weewx data base
#' @param sta Station id
#' @param name_st Station name
#' @param tzo Time zone of the weewx data (default = "CET")
#' @param bku Backup the data base (default = TRUE)
#'
#' @seealso \code{\link{d_inventory}} or \code{\link{ds_inventory}} list the content
#' of the data base ;
#'  \code{\link{d_exp_hts}} to extract time-series.
#'
#'
#' @examples \dontrun{
#'
#' d_imp_weewx("foo.sqlite","weewx.sql")
#' }
#'

d_convert_weewx <- function(fsq, db.weewx, sta, name_st, tzo ="CET",
                        bku = TRUE){

  if (bku == TRUE) d_backup(fsq)

  # Warnings
  if (!file.exists(db.weewx))
    return(warning("\nThe weewx data base doesn't exist.\n"))

    # Init
  dn <- dirname(db.weewx)
  if(is.na(fsq)) fsq <- paste0(dn,"/weewx.sqlite")
  d_create(fsq, cr_table = TRUE, bku = FALSE)


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
    date <- as.POSIXct(date, origin = "1970-01-01", tzo=tzo)
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
    date <- as.POSIXct(date, origin = "1970-01-01", tzo=tzo)
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
      date <- as.POSIXct(date, origin = "1970-01-01", tzo=tzo)
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
  date <- as.POSIXct(date, origin = "1970-01-01", tzo=tzo)
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
  date <- as.POSIXct(date, origin = "1970-01-01", tzo=tzo)
  selection <- paste ("SELECT", "sum",  "FROM archive_day_rain")
  value <- RSQLite::dbGetQuery(conn, selection)
  value <- value[,1]
  value <- value * 25.4 ; sensor <- "JPR"
  t <- tibble(date,value,sensor)
  x <- bind_rows(x,t)

  # Deconnexion base weewx
  RSQLite::dbDisconnect(conn)

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
  message("\nBase ", fsq, " created")

}

#Fin
