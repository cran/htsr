#' @title Inventory of an htsr data base
#'
#' @author P. Chevallier - Jan 2019 - Nov 2020
#'
#' @description The function produces an inventory of the stations and of sensors of an htsr data base.
#' If only a display is needed, the function \code{\link{ds_inventory}} is more convenient.
#'
#' @param fsq Data base file
#' @param sta_sen Station_id, with its list of sensors
#' @param form.out Display option: NA (console, default) or excel (xlsx) or text (csv; ou csv,)
#'
#' @seealso
#'  \code{\link{ds_inventory}}
#'
#' @details
#' If sta_sen is NA (default), all stations and sensors are processed.
#' If sta_sen is a Station_id, only the sensors of this station are processed.
#'
#' @details
#'- form.out can take the following values : NA, "csv," text file with
#''.' as decimal separator and ',' as field separator / "csv;"
#'text file with ',' as decimal separator and ';' as field separator
#' / "xlsx" Excel file.
#'

#' @return Two tables with the inventory of stations and sensors of a data base.
#' If the output format is an excel file, they are displayed in two sheets
#' of the same excel file.


d_inventory <- function(fsq, sta_sen=NA, form.out=NA){

  #Warning
  if (!file.exists(fsq))
    return(warning("\nThis data base doesn't exist, Verify!\n"))
  if(!(form.out %in% c(NA, "xlsx", "csv,", "csv;"))) form.out <- NA
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),fsq)
  ltab <- RSQLite::dbListTables(conn)
  if(!("ST" %in% ltab)){
    RSQLite::dbDisconnect(conn)
    return(warning("\nThis data base doesn't have station table.\n"))
  }

  # Verify station list
  sel <- paste ("SELECT * FROM ST")
  lsta <- RSQLite::dbGetQuery(conn, sel)
  nsta <- nrow(lsta)
  if (nsta == 0){
    RSQLite::dbDisconnect(conn)
    return(warning("\nNo station in the data base\n"))
  }

  # Initialisation
  stac <- nom_stac <-senc <- descric <-tablec <- nbrc <- d_endc <- d_startc <- NULL
  nosensor <- nostation <- NULL
  Id_station <- Sensor <- NULL
  Altitude <- Id_Station <- Latitude <- Longitude <- Nom <- Pays <- Station_type <- NULL
  Superficie_bv <-  Type_Station <- NULL

  # tableau x1
  x1 <- dplyr::select(lsta, Id_station = Id_Station, Station_type = Type_Station,
                      Station_name = Nom, Country = Pays, Latitude,
                       Longitude, Altitude, Basin_areas = Superficie_bv)
  x1 <- dplyr::arrange(x1, Station_type, Id_station)

  # Loop Capteur
  # Cas où sta_sen est NA
  if (is.na(sta_sen)) {
    for(i in 1: nsta){
      sta <- lsta$Id_Station[i]
      nom_sta <- lsta$Nom[i]
      sta1 <- paste0("'",sta, "'")
      sel <- paste ("SELECT * FROM SS WHERE Id_Station =", sta1)
      lsen <- RSQLite::dbGetQuery(conn, sel)
      nsen <- nrow(lsen)
      if (nsen == 0) {
        message("The station ", sta, " has no sensor")
      }
      else for(j in 1: nsen) {
        sen <- lsen$Capteur[j]
        sen1 <- paste0("'",sen, "'")
        sel <- paste ("SELECT * FROM SS WHERE Id_Station =", sta1,
          "AND Capteur =", sen1)
        x <- RSQLite::dbGetQuery(conn, sel)
        descri <- x$Description
        tablex <- x$Tabl
        sel <- paste ("SELECT * FROM", tablex, "WHERE Id_Station =", paste0("'",sta, "'"),
                "AND Capteur =", paste0("'",sen, "'"))
        x <- RSQLite::dbGetQuery(conn, sel)
        nbr <- nrow(x)
        if (nbr > 0) {
          xd <- as.POSIXct(x$Date, origin = "1970-01-01", tz = "UTC")
          d_start <- min(xd)
          d_end <- max(xd)
        } else d_start <- d_end <- NA
        stac <- c(stac, sta)
        nom_stac <- c(nom_stac, nom_sta)
        senc <- c(senc, sen)
        descric <- c(descric, descri)
        tablec <- c(tablec, tablex)
        nbrc <- c(nbrc, nbr)
        d_endc <- c(d_endc, d_end)
        d_startc <- c(d_startc, d_start)
      }
    }
  } else {

    # Cas où sta_sen est une station
    # Cas où sta_sen est dans la liste des stations
    if(sta_sen %in% lsta$Id_Station) {
      k <- 0
      for (j in 1:length(lsta$Id_Station)) if(sta_sen == lsta$Id_Station[j]) k <- j
      sta <- lsta$Id_Station[k]
      nom_sta <- lsta$Nom[k]
      sta1 <- paste0("'",sta, "'")
      sel <- paste ("SELECT * FROM SS WHERE Id_Station =", sta1)
      lsen <- RSQLite::dbGetQuery(conn, sel)
      nsen <- nrow(lsen)
      if (nsen == 0) nosensor <- sta
      # if (nsen == 0) next
      else for(j in 1: nsen) {
        sen <- lsen$Capteur[j]
        sen1 <- paste0("'",sen, "'")
        sel <- paste ("SELECT * FROM SS WHERE Id_Station =", sta1,
                      "AND Capteur =", sen1)
        x <- RSQLite::dbGetQuery(conn, sel)
        descri <- x$Description
        tablex <- x$Tabl
        sel <- paste ("SELECT * FROM", tablex, "WHERE Id_Station =", paste0("'",sta, "'"),
                      "AND Capteur =", paste0("'",sen, "'"))
        x <- RSQLite::dbGetQuery(conn, sel)
        nbr <- nrow(x)
        if (nbr > 0) {
          xd <- as.POSIXct(x$Date, origin = "1970-01-01", tz = "UTC")
          d_start <- min(xd)
          d_end <- max(xd)
        } else d_start <- d_end <- NA
        stac <- c(stac, sta)
        nom_stac <- c(nom_stac, nom_sta)
        senc <- c(senc, sen)
        descric <- c(descric, descri)
        tablec <- c(tablec, tablex)
        nbrc <- c(nbrc, nbr)
        d_endc <- c(d_endc, d_end)
        d_startc <- c(d_startc, d_start)
      }

    # Cas où sta_sen n'est pas dans la liste des stations
    } else nostation <- sta_sen

  }
  # tableau x2
  if (!is.null(nostation)) x2 <- tibble (station = nostation, comment = " is not in the data base")
  else {
    if (!is.null(nosensor)) x2 <- tibble (station = nosensor, comment = " has no sensor")
    else {
      x2 <- tibble::tibble(Id_station = stac, Station_name = nom_stac, Sensor = senc, Description = descric,
          Table = tablec, Nb_Rec = nbrc, Date_start = as_datetime(d_startc), Date_end = as_datetime(d_endc))
      x2 <- dplyr::arrange(x2, Id_station, Sensor)
    }
  }

  # déconnexion et ecriture des résultats
  RSQLite::dbDisconnect(conn)
  if(is.na(form.out)== TRUE) {
    a <- list(x1, x2)
    return(a)
  } else {
    nfse <- tools::file_path_sans_ext(fsq)
    fileo1 <- paste0(nfse, "_inv-sta")
    fileo2 <- paste0(nfse, "_inv-sen")
    if(form.out== "csv,") {
      fileo1 <- paste0(fileo1,".csv")
      fileo2 <- paste0(fileo2,".csv")
      write.csv (x1,file=fileo1, row.names=FALSE)
      write.csv (x2,file=fileo2, row.names=FALSE)
      return(message("\nThe files ",fileo1," and ", fileo2," are written.\n"))
    }
    if(form.out== "csv;"){
      fileo1 <- paste0(fileo1,".csv")
      fileo2 <- paste0(fileo2,".csv")
      write.csv2 (x1,file=fileo1, row.names=FALSE)
      write.csv2 (x2,file=fileo2, row.names=FALSE)
      return(message("\nThe files ",fileo1," and ", fileo2," are written.\n"))
    }
    if(form.out== "xlsx"){
      fileo <- paste0(paste0(nfse, "_inv"),".xlsx")
      xx <- list(x1,x2)
      WriteXLS::WriteXLS (xx, ExcelFileName=fileo,SheetNames=c("Stations","Sensors"),
                          col.names=TRUE, row.names=FALSE, na="#N/A")
      return(message("\nThe file ",fileo," is written.\n"))
    }
  }
}
# FIN




