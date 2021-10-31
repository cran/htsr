#' @title Remove hts records from a data base
#'
#' @author P. Chevallier - jan 2019 - nov 2020
#'
#' @description Remove hst records from a Sqlite base
#'
#' @details
#' The main table where the data have to be removed must be selected with one the
#' following abbreviation: WL (water level), DI (discharge), WE (weather), PR (precipitation)
#' or QU (quality)
#'
#' @param fsq Full name of the data base
#' @param table Table
#' @param sta Station id
#' @param sen Sensor id
#' @param start Start time of removed records
#' @param end End time of removed records
#'
#' @return Actualized data base
#'


d_rem_hts <- function(fsq, table, sta, sen, start=NA, end=NA) {

  Date <- NULL

  # Warnings
  if (!file.exists(fsq))
    return(warning("\nThis data base doesn't exist, Verify!\n"))
  conn <- dbConnect(SQLite(),fsq)
  ltable <- dbListTables(conn)
  dbDisconnect(conn)
  if(!("ST") %in% ltable || !("SS") %in% ltable)
    return(warning("\nNo table 'ST' and/or 'SS in the data base.\n"))
  if(!(table %in% c("WL", "DI", "WE", "PR", "QU")))
    return(warning("\nTable name not authorized.\n"))
  if(!(table %in% ltable))
    return(warning("\nNo table", table, "in the data base.\n"))
  conn <- dbConnect(SQLite(),fsq)
  selec <- paste ("SELECT * FROM ST")
  xst <- dbGetQuery(conn, selec)
  sta1 <- paste0("'",sta,"'")
  selec <- paste ("SELECT * FROM SS WHERE Id_Station =", sta1)
  xcp <- dbGetQuery(conn, selec)
  dbDisconnect(conn)
  if(!(sta %in% xst$Id_Station))
    return(warning("\nNo station", sta, "in the data base."))
  if(!(sen %in% xcp$Capteur))
    return(warning("\nNo sensor", sen, "in the data base for the considered station."))
  if(!(is.na(start))) start <- as.POSIXct(start, origin = "1970-01-01" , tz="UTC")
  if(!(is.na(end))) end <- as.POSIXct(end, origin = "1970-01-01" , tz="UTC")
  if(!(is.na(start)) || !(is.na(end))) {
    diff <- as.numeric(end) - as.numeric(start)
    if (diff < 0)
      return(warning("\nStart time is posterior to end time."))
  }

  # Find records in the considered interval
  conn <- dbConnect(SQLite(),fsq)
  sen1 <- paste0("'",sen,"'")
  selec <- paste ("SELECT * FROM", table, "WHERE Id_Station = ", sta1,
    "AND Capteur = ", sen1)
  xrec <- dbGetQuery(conn, selec)
  dbDisconnect(conn)
  xrec <- as_tibble(xrec)
  if (nrow(xrec) == 0){
    warning("\nNo record in the considered interval. \n")
    return(nrow(xrec))
  }
  xrec$Date <- as.POSIXct(xrec$Date, origin = "1970-01-01", tz = "UTC")
  if (is.na(start)) start <- min(xrec$Date)
  if (is.na(end)) end <- max(xrec$Date)
  diff <- as.numeric(end - start)
  if (diff < 0)
    return(warning("\nStart time or end time not consistent with the time-series."))
  xrec1 <- dplyr::filter(xrec, Date >= start & Date <= end)
  nr <- nrow(xrec1)
  message("\n", nr, " records in the table for considered time interval.")
  repeat {
    message("\nConfim remove (Y/N)? ")
    resp <- readline()
    if(resp %in% c("Y","y","O","o","N","n")) break
    else warning("\nResponse", resp, "not allowed.")
  }
  if(resp %in% c("N","n")) {
    return(warning("\nNo change in the data base.\n"))
  }

  # Backup
  d_backup(fsq)

  # Remove the records from the sensor
  conn <- dbConnect(SQLite(),fsq)
  selec <- paste ("DELETE FROM", table, "WHERE Id_Station = ", sta1,
                  "AND Capteur = ", sen1)
  rs <-dbSendQuery(conn, selec)
  dbClearResult(rs)

  # Rewrite the not selected records into the sensor
  xrec1 <- as.data.frame(dplyr::filter(xrec, Date < start & Date > end))
  if(nrow(xrec1)>0){
    dbWriteTable(conn, name=table, xrec1, append = TRUE)
  }
  dbDisconnect(conn)
  message("\n",nr, " records have been removed from the table ", table)
  return(nr)
}


#Fin
