#' @title Create, Modify or Remove a station
#'
#' @author P. Chevallier - Jan 2018-Nov 2020
#'
#' @description
#' Create, Modify or Remove a station in a tshm data base. A shiny version of this function is
#' available: \code{\link{ds_station}}.
#'
#' @param fsq  Full name of the data base
#' @param op Create (default), Modify or Remove C/M/R
#' @param sta Station id
#' @param ty_st Station type: "H" hydro or "M" meteo
#' @param name_st Station name
#' @param name_fld List of field names
#' @param value_fld list of field values
#' @param bku Automatic Backup TRUE (default) / FALSE
#'
#'
#' @seealso
#' \itemize{
#'  \item \code{\link{d_inventory}} or \code{\link{ds_inventory}} for exploring the
#'  data base content;
#'  \item \code{\link{ds_exp_hts}} for extracting a time-series;
#'  \item \code{\link{d_table}} for creating a data base and/or creating/removing
#'  a table ;
#'  \item \code{\link{d_sensor}} for creating, modifying or removing a sensor.
#' }
#'
#' @details
#' The field names are expressed in French for compatibility reason with Hydraccess.
#' A translation is given in [].
#'
#' If op is C, the fields Id_Station (sta), Type_Station and
#' Nom (name_st) are compulsory. The field Nom (name_st) can be modified afterwards.
#'
#' @details
#' If op is C or M, the following fields can be completed :
#' \itemize{
#' \item [Order] Ordre = as.character(NA),
#' \item [Station type] Type_Station = as.character(type_st),
#' \item [Station id] Id_Station = as.character(sta),
#' \item [Second station Id] Id_Secondaire = as.character(NA),
#' \item [Third station id] Id_Tertiaire = as.character(NA),
#' \item [Meteo type] Type_Meteo = as.character(NA),
#' \item [Name] Nom = as.character(name_st),
#' \item [Country] Pays = as.character(NA),
#' \item [Zone] Zone = as.character(NA),
#' \item [Sub-zone] SousZone = as.character(NA),
#' \item [Large basin] GrandBassin = as.character(NA),
#' \item [Basin] Bassin = as.character(NA),
#' \item [Small basin] PetitBassin = as.character(NA),
#' \item [River] Riviere = as.character(NA),
#' \item [Manager] Gestionnaire = as.character(NA),
#' \item [Latitude] Latitude = as.numeric(NA),
#' \item [Longitude] Longitude = as.numeric(NA),
#' \item [Altitude] Altitude = as.integer(NA),
#' \item [Basin area] Superficie_bv = as.numeric(NA),
#' \item [Starting month of hydro year] Mois_Debut_Hydro = as.numeric(NA),
#' \item [Starting activity date] Debut_Activite = as.numeric(NA),
#' \item [Activity] Activite = as.logical(NA),
#' \item [Yes/No criterion] Critere_OuiNon = as.logical(NA),
#' \item [Yes/No second criterion] Critere_OuiNon2 = as.logical(NA),
#' \item [Numeric criterion] Critere_Numerique = as.numeric(NA),
#' \item [Text criterion] Critere_Texte = as.character(NA),
#' \item [Observer name] Nom_Observateur = as.character(NA),
#' \item [Address] Adresse = as.character(NA),
#' \item [Teletransmission] Teletransmission = as.logical(NA),
#' \item [Recorder] Enregistreur = as.logical(NA),
#' \item [Fictive] Fictive = as.logical(NA),
#' \item [Comment] Commentaire = as.character(NA),
#' \item [Flag] Flag = as.logical(NA),
#' \item [District] District = as.character(NA),
#' \item [Place] Localite = as.character(NA)
#' }
#'
#' If op is M, station type and station id cannot be modified. The sensor
#' data corresponding to the station are conserved.
#'
#' If op is R, all data and sensors of the station are removed.
#'
#'
#' @return
#' Station created, modified ou removed from the data base

d_station <- function(fsq, op = "C", sta=NA, ty_st = NA, name_st=NA,
  name_fld=NA, value_fld=NA, bku = TRUE) {

  # Warnings
  if (!file.exists(fsq))
    return(warning("\nThis data base doesn't exist, Verify!\n"))
  if (!(op %in% c("C", "M", "R", "c", "m", "r")))
    return(warning("\nOperation missing or not authorized\n"))
  if((op %in% c("C", "c")) && (is.na(sta) || is.na(name_st) || is.na(ty_st)))
    return(warning("\nStation id, type and name must be completed!\n"))
  if((op %in% c("C", "c")) && !(ty_st %in% c("M","H")))
    return(warning("\nStation type and name must be H (hydro) or M (Meteo)\n"))
  if((op %in% c("M", "R", "m", "r")) && (is.na(sta)))
    return(warning("\nStation id must be completed!\n"))

  conn <- dbConnect(SQLite(),fsq)
    ltable <- dbListTables(conn)
  dbDisconnect(conn)
  if(!("ST" %in% ltable))
      return(warning("\nNo table 'ST'.\n"))

  if (op %in% c("C","c","M", "m")){
    if(length(name_fld) != length(value_fld))
      return(warning("\nThe lists field names and field values must have the same length.\n"))
    if(length(name_fld)==1 && is.na(name_fld)) name_fld <- NA
    else {
      for (i in 1:length(name_fld)){
        if(!(name_fld[i] %in% c(
        "Ordre",  "Id_Secondaire", "Id_Tertiaire", "Type_Meteo",
        "Nom", "Pays", "Zone", "SousZone", "GrandBassin", "Bassin", "PetitBassin",
        "Riviere", "Gestionnaire", "Critere_Texte", "Nom_Observateur", "Adresse",
        "Commentaire", "District", "Localite","Latitude", "Longitude", "Altitude",
        "Superficie_bv", "Mois_Debut_Hydro", "Debut_Activite", "Critere_Numerique",
        "Activite", "Critere_OuiNon", "Critere_OuiNon2",
        "Teletransmission", "Enregistreur", "Flag")))
        return(warning("\nField name not authorized.\n"))
      if(name_fld[i] %in% c(
        "Ordre",  "Id_Secondaire", "Id_Tertiaire", "Type_Meteo",
        "Nom", "Pays", "Zone", "SousZone", "GrandBassin", "Bassin", "PetitBassin",
        "Riviere", "Gestionnaire", "Critere_Texte", "Nom_Observateur", "Adresse",
        "Commentaire", "District", "Localite"))
        value_fld[i] <- as.character(value_fld[i])
      if(name_fld[i] %in% c(
        "Latitude", "Longitude", "Altitude", "Superficie_bv", "Debut_Activite", "Critere_Numerique"))
        value_fld[i] <- as.numeric(value_fld[i])
      if(name_fld[i] %in% c("Altitude", "Mois_Debut_Hydro"))
        value_fld[i] <- as.integer(value_fld[i])
      if(name_fld[i] %in% c("Activite", "Critere_OuiNon", "Critere_OuiNon2",
                         "Teletransmission", "Enregistreur", "Flag"))
        value_fld[i] <- as.logical(value_fld[i])
      }
    }
  }

  # Station list
  conn <- dbConnect(SQLite(),fsq)
    selection <- paste ("SELECT * FROM ST")
    xxt <-dbGetQuery(conn, selection)
  dbDisconnect(conn)

  # No job cases
  if (op %in% c("C","c") && sta %in% xxt$Id_Station)
    return(warning("\nStation ", sta, " already exists and its data are conserved.\n"))
  if (op %in% c("M","m", "R", "r") && !(sta %in% xxt$Id_Station))
    return(warning("\nThe station ", sta, " doesn't exist in the station table.\n"))

  # Backup
  if (bku == TRUE) d_backup(fsq)

  # Create
  if (op %in% c("C","c")) {
    conn <- dbConnect(SQLite(),fsq)
    station <- list(
      Ordre = as.character(NA), Type_Station = as.character(ty_st),
      Id_Station = as.character(sta), Id_Secondaire = as.character(NA),
      Id_Tertiaire = as.character(NA), Type_Meteo = as.character(NA),
      Nom = as.character(name_st), Pays = as.character(NA),
      Zone = as.character(NA), SousZone = as.character(NA),
      GrandBassin = as.character(NA), Bassin = as.character(NA),
      PetitBassin = as.character(NA), Riviere = as.character(NA),
      Gestionnaire = as.character(NA), Latitude = as.numeric(NA),
      Longitude = as.numeric(NA), Altitude = as.integer(NA),
      Superficie_bv = as.numeric(NA), Mois_Debut_Hydro = as.integer(NA),
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
    conn <- dbConnect(SQLite(),fsq)
    sta1 <- paste0("'",sta,"'")
     selection <- paste("SELECT * FROM ST WHERE Id_Station = ", sta1)
    station <- dbGetQuery(conn, selection)
    if (!is.na(name_st)) station[7] <- name_st
    if(length(name_fld) != 1) {
      for (j in 1:length(value_fld)) {
        for (i in 1:length(station)) {
          if(name_fld[j] == names(station[i])) k <- i
        }
        station[1, k] <- value_fld[j]
      }
    } else {
      if (!is.na(name_fld)){
        for (i in 1:length(station)) {
          if(name_fld == names(station[i])) k <- i
        }
        station[1, k] <- value_fld
      }
    }
    selection <- paste ("DELETE FROM ST WHERE Id_Station = ", sta1)
    rs <- dbSendQuery(conn, selection)
    dbClearResult(rs)
    dbWriteTable(conn, "ST", station, append = TRUE)
    message("\nStation ",name_st," with id ", sta, " modified.")
  }

  # Effacer
  if (op %in% c("R","r")){
    conn <- dbConnect(SQLite(),fsq)
    lstab <- c("SS", "WL", "DI", "WE", "PR", "QU","ST")
    sta1 = paste0("'",as.character(sta),"'")
    for(i in 1:length(lstab)) {
      selection <- paste ("DELETE FROM", lstab[i], " WHERE Id_Station = ", sta1)
      rs <- dbSendQuery(conn, selection)
      dbClearResult(rs)
    }
    message("\nStation ",name_st," with id ", sta, " is removed with all its data.")
    dbDisconnect(conn)
  }

}

#Fin
