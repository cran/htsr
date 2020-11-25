#' @title Create, Modify or Remove a station
#'
#' @author P. Chevallier - Jan 2018-Jan 2019
#'
#' @description
#' Create, Modify or Remove a station in a tshm data base
#'
#' @param db.sqlite  Full name of the data base
#' @param op Create (default), Modify or Remove C/M/R
#' @param sta Station id
#' @param name_st Station name
#' @param name_fld List of field names
#' @param value_fld list of field values
#' @param bku Automatic Backup TRUE (default) / FALSE
#'
#'
#' @seealso
#' \itemize{
#'  \item \code{\link{d_inventory}} or \code{\link{d_invent}} for exploring the
#'  data base content;
#'  \item \code{\link{d_exphts}} for extracting a time-series;
#'  \item \code{\link{d_table}} for creating a data base and/or creating/removing
#'  a table ;
#'  \item \code{\link{d_sensor}} for creating, modifying or removing a sensor.
#' }
#'
#' @details
#' If op is C, the fields Id_Station (sta) and
#' Nom (name_st) are compulsory. The field Nom (name_st) can be modified afterwards.
#'
#' If op is C or M, the following fields can be completed :
#' Ordre = as.character(NA), Type_Station = as.character(type_st),
#' Id_Station = as.character(sta), Id_Secondaire = as.character(NA),
#' Id_Tertiaire = as.character(NA), Type_Meteo = as.character(NA),
#' Nom = as.character(name_st), Pays = as.character(NA),
#' Zone = as.character(NA), SousZone = as.character(NA),
#' GrandBassin = as.character(NA), Bassin = as.character(NA),
#' PetitBassin = as.character(NA), Riviere = as.character(NA),
#' Gestionnaire = as.character(NA), Latitude = as.numeric(NA),
#' Longitude = as.numeric(NA), Altitude = as.numeric(NA),
#' Superficie_bv = as.numeric(NA), Mois_Debut_Hydro = as.numeric(NA),
#' Debut_Activite = as.numeric(NA), Activite = as.logical(NA),
#' Critere_OuiNon = as.logical(NA), Critere_OuiNon2 = as.logical(NA),
#' Critere_Numerique = as.numeric(NA), Critere_Texte = as.character(NA),
#' Nom_Observateur = as.character(NA), Adresse = as.character(NA),
#' Teletransmission = as.logical(NA), Enregistreur = as.logical(NA),
#' Fictive = as.logical(NA),
#' Commentaire = as.character(NA), Flag = as.logical(NA),
#' District = as.character(NA), Localite = as.character(NA)
#'
#' If op is M, station type and station id cannot be modified. The sensor
#' data corresponding to the station are conserved.
#'
#' If op is R, all data and sensors of the station are removed.
#'
#'
#' @return
#' Station created, modified ou removed from the data base

d_station <- function(db.sqlite, op = "C", sta=NA, name_st=NA,
  name_fld=NA, value_fld=NA, bku = TRUE) {

  # Warnings
  if (!file.exists(db.sqlite))
    return(warning("\nThis data base doesn't exist, Verify!\n"))
  if (!(op %in% c("C", "M", "R", "c", "m", "r")))
    return(warning("\nOperation missing or not authorized\n"))
  if((op %in% c("C", "c")) && (is.na(sta) || is.na(name_st)))
    return(warning("\nStation id and name must be completed!\n"))
  if((op %in% c("M", "R", "m", "r")) && (is.na(sta)))
    return(warning("\nStation id must be completed!\n"))

  conn <- dbConnect(SQLite(),db.sqlite)
    ltable <- dbListTables(conn)
  dbDisconnect(conn)
  if(!("ST" %in% ltable))
      return(warning("\nNo table 'ST'.\n"))

  if (op %in% c("C","c","M", "m")){
    if(length(name_fld) != length(value_fld))
      return(warning("\nThe lists field names and field values must have the same length.\n"))
    if(length(name_fld)==1 && is.na(name_fld)) name_fld <- NA
    else if(!(name_fld %in% c(
      "Ordre",  "Id_Secondaire", "Id_Tertiaire", "Type_Meteo",
      "Nom", "Pays", "Zone", "SousZone", "GrandBassin", "Bassin", "PetitBassin",
      "Riviere", "Gestionnaire", "Critere_Texte", "Nom_Observateur", "Adresse",
      "Commentaire", "District", "Localite","Latitude", "Longitude", "Altitude",
      "Superficie_bv", "Mois_Debut_Hydro", "Debut_Activite", "Critere_Numerique",
      "Activite", "Critere_OuiNon", "Critere_OuiNon2",
      "Teletransmission", "Enregistreur", "Flag")))
      return(warning("\nField name not authorized.\n"))
    if(name_fld %in% c(
      "Ordre",  "Id_Secondaire", "Id_Tertiaire", "Type_Meteo",
      "Nom", "Pays", "Zone", "SousZone", "GrandBassin", "Bassin", "PetitBassin",
      "Riviere", "Gestionnaire", "Critere_Texte", "Nom_Observateur", "Adresse",
      "Commentaire", "District", "Localite") && !is.character(value_fld))
      return(warning("\nField value of ", name_fld," must be character.\n"))
    if(name_fld %in% c(
      "Latitude", "Longitude", "Altitude", "Superficie_bv",
      "Mois_Debut_Hydro", "Debut_Activite", "Critere_Numerique") &&
      !is.numeric(value_fld))
      return(warning("\nField value of ", name_fld," must be numeric.\n"))
    if(name_fld %in% c("Activite", "Critere_OuiNon", "Critere_OuiNon2",
                       "Teletransmission", "Enregistreur", "Flag") && !is.logical(value_fld))
      return(warning("\nField value of ", name_fld," must be logical.\n"))
  }

  # Station list
  conn <- dbConnect(SQLite(),db.sqlite)
    selection <- paste ("SELECT * FROM ST")
    xxt <-dbGetQuery(conn, selection)
  dbDisconnect(conn)

  # No job cases
  if (op %in% c("C","c") && sta %in% xxt$Id_Station)
    return(warning("\nStation ", sta, " already exists and its data are conserved.\n"))
  if (op %in% c("M","m", "R", "r") && !(sta %in% xxt$Id_Station))
    return(warning("\nThe station ", sta, " doesn't exist in the station table.\n"))

  # Backup
  if (bku == TRUE) d_backup(db.sqlite)
  conn <- dbConnect(SQLite(),db.sqlite)

  # Create
  if (op %in% c("C","c")) {
    station <- list(
      Ordre = as.character(NA), Type_Station = as.character(NA),
      Id_Station = as.character(sta), Id_Secondaire = as.character(NA),
      Id_Tertiaire = as.character(NA), Type_Meteo = as.character(NA),
      Nom = as.character(name_st), Pays = as.character(NA),
      Zone = as.character(NA), SousZone = as.character(NA),
      GrandBassin = as.character(NA), Bassin = as.character(NA),
      PetitBassin = as.character(NA), Riviere = as.character(NA),
      Gestionnaire = as.character(NA), Latitude = as.numeric(NA),
      Longitude = as.numeric(NA), Altitude = as.numeric(NA),
      Superficie_bv = as.numeric(NA), Mois_Debut_Hydro = as.numeric(NA),
      Debut_Activite = as.numeric(NA), Activite = as.logical(NA),
      Critere_OuiNon = as.logical(NA), Critere_OuiNon2 = as.logical(NA),
      Critere_Numerique = as.numeric(NA), Critere_Texte = as.character(NA),
      Nom_Observateur = as.character(NA), Adresse = as.character(NA),
      Teletransmission = as.logical(NA), Enregistreur = as.logical(NA),
      Fictive = as.logical(NA),
      Commentaire = as.character(NA), Flag = as.logical(NA),
      District = as.character(NA), Localite = as.character(NA)
    )
    if(length(name_fld) == 1 && is.na(name_fld)) name_fld <- NA
    else for(i in 1:length(station)){
      j <- 0
      repeat {
        j <- j+1
        if(j > length(name_fld)) break
        if(name_fld[j] == names(station[i])) station [i] <- value_fld [j]
      }
    }
    station <- as.data.frame(station, stringsAsFactors = FALSE)
    dbWriteTable(conn, "ST", station, append = TRUE)
    message("\nStation ",station$Nom," with id ", sta, " created.")
    dbDisconnect(conn)
  }

  # Modify
  if (op %in% c("M","m")) {
    sta1 <- paste0("'",sta,"'")
    selection <- paste("SELECT * FROM ST WHERE Id_Station = ", sta1)
    station <- dbGetQuery(conn, selection)
    if(length(name_fld) != 1 || is.na(name_fld)) for(i in 1:length(station)){
      j <- 0
      repeat {
        j <- j+1
        if(j > length(name_fld)) break
        if(name_fld[j] == names(station[i])) station [i] <- value_fld [j]
      }
    }
    station <- as.data.frame(station, stringsAsFactors = FALSE)
    dbWriteTable(conn, "Stations_Base", station, overwrite = TRUE)
#    dbDisconnect(conn)
    message("\nStation ",name_st," with id ", sta, " modified.")
    dbDisconnect(conn)
  }

  # Effacer
  if (op %in% c("R","r")){
    lstab <- c("SS", "WL", "DI", "WE", "PR", "QU","ST")
    sta1 = paste0("'",as.character(sta),"'")
    for(i in 1:length(lstab)) {
      selection <- paste ("DELETE FROM", lstab[i], " WHERE Id_Station = ", sta1)
      conn <- dbConnect(SQLite(),db.sqlite)
      dbSendQuery(conn, selection)
    }
#    dbDisconnect(conn)
    message("\nStation ",name_st," with id ", sta, " is removed with all its data.")
    dbDisconnect(conn)
  }

  # st_list <- sqliteQuickColumn(conn, "ST","Id_Station")
  # message("\nStations ids in data base: ", st_list)
  # message("\n")
}

#Fin
