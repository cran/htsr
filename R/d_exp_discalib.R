#' @title Export discharge measurements and calibrations from data base
#'
#' @author P. Chevallier - Sep 2017 - Nov 2020
#'
#' @description Export discharge measurements and calibrations from data base
#'
#' @param fsq Full name of the data base
#' @param sta Station Id.
#' @param calib Calibration extraction TRUE (default)/FALSE
#' @param dism Discharge measurement extraction TRUE (default)/FALSE
#'
#' @seealso \code{\link{ds_exp_hts}} for export time-series
#'
#' @return a list of 2 tibbles, one with the calibration table and one with the discharge measurements
#'
#'

d_exp_discalib <- function(fsq, sta, calib=TRUE, dism=TRUE) {

  # initialisation
  tzo <- NULL
  load(file=system.file("extdata/settings.RData",package="htsr"))
#  nfse <- tools::file_path_sans_ext(fsq)
  if(calib==FALSE & dism==FALSE)
    return(warning("\nAt least one between calib and dism must be TRUE."))
  Id_Station <- Capteur <- Date <- H <- Q <- Active <- NULL

  # extraction
  # etalonnages
  if(calib==TRUE) {
    conn <- dbConnect(SQLite(),fsq)
      table <- "LC"
      sta1 <- paste("'",as.character(sta),"'",sep="")
#      sen1 <- paste("'",as.character(sen),"'",sep="")
      selection <- paste ("SELECT * FROM",
      table, " WHERE Id_Station =",sta1)
#      table, " WHERE Id_Station =",sta1, " AND Capteur =",sen1)
      xt <- dbGetQuery(conn, selection)
    dbDisconnect(conn)

  # controle
    blab <- as.character (xt$Date)
    if(is.na(blab[1])==TRUE)
      return(warning("\nNo calibration data for this station/sensor.\n"))

  # constitution du tableau de sortie
    xt$Date <- as.character(as.POSIXct(xt$Date, origin="1970-01-01"))
    calibtab <- data.frame(xt$Id_Station,xt$Capteur,xt$Capteur_Sortie,xt$Date,
      xt$H,xt$Q)
    colnames(calibtab) <-c("Id_Station","Sensor","Sen_Out","Date","H","Q")
    calibtab <- as_tibble(calibtab)
  } else calibtab <- NA

  if(dism==TRUE) {
    conn <- dbConnect(SQLite(),fsq)
      table <- "DM"
      sta1 <- paste("'",as.character(sta),"'",sep="")
#      sen1 <- paste("'",as.character(sen),"'",sep="")
      selection <- paste ("SELECT * FROM",
        table, " WHERE Id_Station =",sta1)
#      table, " WHERE Id_Station =",sta1, " AND Capteur =",sen1)
    xt <- dbGetQuery(conn, selection)
    dbDisconnect(conn)

    # controle
    blab <- as.character (xt$Date)
    if(is.na(blab[1])==TRUE)
      return(warning("\nNo discharge measurement data for this station/sensor.\n"))

    # constitution du tableau de sortie
    xt$Date <- as.character(as.POSIXct(xt$Date, origin="1970-01-01", tz=tzo))
    dismtab <- as_tibble(xt)
    dismtab <- select(dismtab, Id_Station, Capteur, Date, H, Q, Active)
    colnames(dismtab) <-c("Id_Station","Sensor","Date","H","Q","Active")
  } else dismtab <- NA

  caldis <- list(calibtab, dismtab)

# retour
  return (caldis)
}

#FIN
