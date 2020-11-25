#' @title Inventory of an htsr data base
#'
#' @author P. Chevallier - Jan - Sep 2019
#'
#' @description The function produces an inventory of the stations and of sensors of an htsr data base.
#' If only a display is needed, the function \code{\link{d_invent}} is more convenient?
#'
#' @param db.sqlite Data base file
#' @param stalist List of station ids to inventory, NA by default
#' @param form.out Display option: NA (console, default) or excel (xlsx) or text (csv; ou csv,)
#'
#' @seealso
#'  \code{\link{d_invent}}
#'
#' @details
#'- stalist can contain a list of station ids, which will be inventoried with their sensors.
#'If stalist is NA (default), all stations containing data are processed.
#'Two tables are returned, one with the station main characteristics and the other one
#'whith the sensor data.
#'
#' @details
#'- form.out can take the following values : NA, "csv," text file with
#''.' as decimal separator and ',' as field separator / "csv;"
#'text file with ',' as decimal separator and ';' as field separator
#' / "xlsx" Excel file.
#'

#' @return Two tables with the inventory of stations and sensors of a data base.
#' If the output format is an excel file, they are displayed in two sheets
#'of the same excel file.
#'
#'


# FONCTION d_Inventory


d_inventory <- function(db.sqlite, stalist=NA, form.out=NA){

  Id_station <- Sensor <- NULL

  #Warning
  if (!file.exists(db.sqlite))
    return(warning("\nThis data base doesn't exist, Verify!\n"))
  if(!(form.out %in% c(NA, "xlsx", "csv,", "csv;"))) form.out <- NA
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),db.sqlite)
  ltab <- RSQLite::dbListTables(conn)
  RSQLite::dbDisconnect(conn)
  if(!("ST" %in% ltab))
    return(warning("\nThis data base doesn't have station table.\n"))

  # Connection
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),db.sqlite)
  sel <- paste ("SELECT * FROM ST")
  lsta <- RSQLite::dbGetQuery(conn, sel)
  RSQLite::dbDisconnect(conn)

  # Loop Id_Station
  nsta <- nrow(lsta)
  if (nsta == 0)
    return(warning("\nNo station in the data base\n"))
#  k <- 0
  for(i in 1: nsta){
    conn <- RSQLite::dbConnect(RSQLite::SQLite(),db.sqlite)
    sta <- lsta$Id_Station[i]
    if(!is.na(stalist) && sta %in% stalist == FALSE) next
    nom_sta <- lsta$Nom[i]
    type_sta <- lsta$Type_Station[i]
    pays <- lsta$Pays[i]
    lat <- lsta$Latitude[i]
    lon <- lsta$Longitude [i]
    alt <- lsta$Altitude [i]
    bv <- lsta$Superficie_bv[i]
    if (!exists("stas")) stas <- sta else stas <- c(stas, sta)
    if (!exists("nom_stas")) nom_stas <- nom_sta else nom_stas <- c(nom_stas, nom_sta)
    if (!exists("type_stas")) type_stas <- type_sta else type_stas <- c(type_stas, type_sta)
    if (!exists("payss")) payss <- pays else payss <- c(payss, pays)
    if (!exists("lats")) lats <- lat else lats <- c(lats, lat)
    if (!exists("lons")) lons <- lon else lons <- c(lons, lon)
    if (!exists("alts")) alts <- alt else alts <- c(alts, alt)
    if (!exists("bvs")) bvs <- bv else bvs <- c(bvs, bv)

    #Selection capteur
    sta1 <- paste0("'",sta, "'")
    sel <- paste ("SELECT * FROM SS WHERE Id_Station =", sta1)
    lsen <- RSQLite::dbGetQuery(conn, sel)
    RSQLite::dbDisconnect(conn)

    # Loop Capteur
    nsen <- nrow(lsen)
    for(j in 1: nsen) {
      if (nsen == 0) {
        next
      } else {
#        k <- k+1
        conn <- RSQLite::dbConnect(RSQLite::SQLite(),db.sqlite)
        sen <- lsen$Capteur[j]
        sen1 <- paste0("'",sen, "'")
        sel <- paste ("SELECT * FROM SS WHERE Id_Station =", sta1,
          "AND Capteur =", sen1)
        x <- RSQLite::dbGetQuery(conn, sel)
        descri <- x$Description
        tablex <- x$Tabl
        sel <- paste ("SELECT * FROM", tablex, "WHERE Id_Station =", sta1,
                "AND Capteur =", sen1)
        x <- RSQLite::dbGetQuery(conn, sel)
        RSQLite::dbDisconnect(conn)
        nbr <- nrow(x)
        if (nbr == 0) next
        xd <- as.POSIXct(x$Date, origin = "1970-01-01", tz = "UTC")
        d_start <- min(xd)
        d_end <- max(xd)
        if (!exists("stac")) stac <- sta else stac <- c(stac, sta)
        if (!exists("nom_stac")) nom_stac <- nom_sta else nom_stac <- c(nom_stac, nom_sta)
        if (!exists("senc")) senc <- sen else senc <- c(senc, sen)
        if (!exists("descric")) descric <- descri else descric <- c(descric, descri)
        if (!exists("tablec")) tablec <- tablex else tablec <- c(tablec, tablex)
        if (!exists("nbrc")) nbrc <- nbr else nbrc <- c(nbrc, nbr)
        if (!exists("d_endc")) d_endc <- d_end else d_endc <- c(d_endc, d_end)
        if (!exists("d_startc")) d_startc <- d_start else d_startc <- c(d_startc, d_start)
      }
    }
  }
  x1 <- tibble::tibble(Id_station = stas, Station_type = type_stas,Station_name = nom_stas,
    Country = payss, Latitude = lats,
    Longitude = lons, Altitude = alts, Basin_area = bvs)
  x1 <- dplyr::arrange(x1, Id_station)
  if (!exists("stac")) return( warning ("No sensor for the station!"))
  x2 <- tibble::tibble(Id_station = stac, Station_name = nom_stac, Sensor = senc, Description = descric,
    Table = tablec, Nb_Rec = nbrc, Date_start = d_startc, Date_end = d_endc)
  x2 <- dplyr::arrange(x2, Id_station, Sensor)

  if(is.na(form.out)== TRUE) {
#    View(x1)
#    View(x2)
    message("\nStations and sensor tables are returned in a list")
    a <- list(x1, x2)
    return(a)
  } else {
    nfse <- tools::file_path_sans_ext(db.sqlite)
    fileo1 <- paste0(nfse, "_inv-sta")
    fileo2 <- paste0(nfse, "_inv-sen")
    if(form.out== "csv,"){
      fileo1 <- paste0(fileo1,".csv")
      fileo2 <- paste0(fileo2,".csv")
      write.csv (x1,file=fileo1, row.names=FALSE)
      write.csv (x2,file=fileo2, row.names=FALSE)
      message("\nThe files ",fileo1," and ", fileo2," are written.\n")
    }
    if(form.out== "csv;"){
      fileo1 <- paste0(fileo1,".csv")
      fileo2 <- paste0(fileo2,".csv")
      write.csv2 (x1,file=fileo1, row.names=FALSE)
      write.csv2 (x2,file=fileo2, row.names=FALSE)
      message("\nThe files ",fileo1," and ", fileo2," are written.\n")
    }
    if(form.out== "xlsx"){
      fileo <- paste0(paste0(nfse, "_inv"),".xlsx")
      xx <- list(x1,x2)
      WriteXLS::WriteXLS (xx, ExcelFileName=fileo,SheetNames=c("Stations","Sensors"),
                          col.names=TRUE, row.names=FALSE, na="#N/A")
      message("\nThe file ",fileo," is written.\n")
    }
  }

  return()
}
# FIN




