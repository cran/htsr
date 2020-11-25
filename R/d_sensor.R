#' @title Create, Modify or Remove a sensor
#'
#' @author P. Chevallier - Feb 2018-Jan 2019
#'
#' @description Create, Modify or Remove a sensor
#'
#' @param db.sqlite Full name of the data base
#' @param op Create, modify or remove C/M/R
#' @param sta station id
#' @param sen sensor id
#' @param table table of the sensor
#' @param name_fld list of field names
#' @param value_fld list of field values
#' @param bku Automatic Backup TRUE (default) / FALSE
#'
#' @seealso
#' \itemize{
#'  \item \code{\link{d_inventory}} or \code{\link{d_invent}} to explore the
#'  data base content;
#'  \item \code{\link{d_exphts}} to extract a time-series;
#'  \item \code{\link{d_create},\link{d_table}} to create a data base and/or create/remove
#'  a table ;
#'  \item \code{\link{d_station}} for create/remove a station.
#' }

#' @details
#' If op is C, the fields sta, table and
#' sen are compulsory and cannot be modified afterwards.
#'
#' Allowed entries for table are: WL (water levels), DI (discharges),
#' QU (Quality), PR (precipitations), WE (weather).
#'
#' If op is C or M, the following fields can be completed :
#' Nature= as.character(NA), Description = as.character(NA),
#' Commentaire= as.character(NA), Code_Limni= as.character(NA),
#' Principal = as.logical(NA), Fictif = as.logical(NA),
#' Maj_Journaliers = as.logical(NA), Maj_Traduction = as.logical(NA),
#' Acquisition_Auto = as.logical(NA), Operationnel = as.logical(NA),
#' Liste_Inst = as.character(NA), Liste_Jour = as.character(NA),
#' Liste_Mois = as.character(NA), Agregation = as.character(NA),
#' Decalage_Temps = as.numeric(NA), Mini = as.numeric(NA),
#' Maxi = as.numeric(NA), Gradient_Maxi = as.numeric(NA),
#' Precision = as.numeric(NA), Decimales = as.numeric(NA),
#' Pente = as.numeric(NA))
#'
#' If op is R, all data corresponding to the sensor of the selected
#' station are removed.
#'
#'
#' @return
#' Sensor created, modified ou removed from the data base
#'
#'

d_sensor <- function(db.sqlite, op = "C", sta, sen, table = NA,
  name_fld=NA, value_fld=NA, bku = TRUE) {

  # Warnings
  if (!file.exists(db.sqlite))
    return(warning("\nThis data base doesn't exist, Verify!\n"))
  if (!(op %in% c("C", "M", "R", "c", "m", "r")))
    return(warning("\nOperation not authorized\n"))
  if(op %in% c("C", "R", "c", "r") && (is.na(sta) || is.na(sen) || is.na(table)))
    return(warning("\nStation and sensor id and table must be completed. \n"))
  if(op %in% c("M", "m") && (is.na(sta) || is.na(sen)))
    return(warning("\nStation and sensor id must be completed. \n"))

  conn <- dbConnect(SQLite(),db.sqlite)
    ltable <- dbListTables(conn)
  dbDisconnect(conn)
  if(!("ST" %in% ltable))
    return(warning("\nNo table ST in the data base..\n"))
  if(!(is.na(table)) && !(table %in% c("WL", "DI", "WE", "PR", "QU")))
    return(warning("\nTable name not authorized.\n"))
  if (op %in% c("C", "c") && (!("SS") %in% ltable))
    return(warning("\nNo table SS in the data base.\n"))
  conn <- dbConnect(SQLite(),db.sqlite)
    selection <- paste ("SELECT * FROM ST")
    xxt <-dbGetQuery(conn, selection)
  dbDisconnect(conn)
  if(!(sta %in% xxt$Id_Station))
    return(warning("\nThe station ", sta, " doesn't exist in the ST table.\n"))

  #Warnings Create & Modify
  if (op %in% c("C","c","M", "m")){
    if (length(name_fld) != length(value_fld))
      return(warning("\nBoth lists field names and field values must have the same length.\n"))
    if(length(name_fld)==1 && is.na(name_fld)) name_fld <- NA
    else if(!(name_fld %in% c("Tabl",
      "Nature", "Description", "Commentaire", "Code_Limni", "Liste_Inst",
      "Liste_Jour", "Liste_Mois", "Agregation","Decalage_Temps", "Mini", "Maxi",
      "Gradient_Maxi", "Precision", "Decimales", "Pente","Principal", "Fictif",
      "Maj_Journaliers", "Maj_Traduction", "Acquisition_Auto", "Operationnel")))
      return(warning("\nField name not authorized.\n"))
    if(name_fld %in% c("Tabl",
      "Nature", "Description", "Commentaire", "Code_Limni", "Liste_Inst",
      "Liste_Jour", "Liste_Mois", "Agregation") && !is.character(value_fld))
      return(warning("\nField value of ", name_fld," must be character.\n"))
    if(name_fld %in% c(
      "Decalage_Temps", "Mini", "Maxi", "Gradient_Maxi", "Precision",
      "Decimales", "Pente") && !is.numeric(value_fld))
      return(warning("\nField value of ", name_fld," must be numeric.\n"))
    if(name_fld %in% c("Principal", "Fictif", "Maj_Journaliers",
                       "Maj_Traduction", "Acquisition_Auto", "Operationnel") &&
       !is.logical(value_fld))
      return(warning("\nField value of ", name_fld," must be logical.\n"))
  }

  # Connexion
  conn <- dbConnect(SQLite(),db.sqlite)
  sta1 <- paste0("'",sta,"'")
  selection <- paste ("SELECT * FROM SS WHERE Id_Station =", sta1)
  listcapt <-dbGetQuery(conn, selection)
#  lcap <- as.vector(listcapt$Capteur)
  dbDisconnect(conn)

  # No job cases
  if (op %in% c("C","c")){
    if (sen %in% listcapt$Capteur) {
      ltab <- listcapt$Tabl[listcapt$Id_Station == sta]
      if(table %in% ltab || length(ltab == 1))
        return(warning("\nStation ",sta,": A Sensor ", sen, " already exists and its data are conserved.\n"))
      else return(warning("\nStation ",sta,": A Sensor ", sen, " already exists with the table name ",ltab,".
       If necessary, remove the sensor, or modify the table name."))
    }
  }
  if (op %in% c("M","m", "R", "r") && !(sen %in% listcapt$Capteur))
    return(warning("\nNo sensor ", sen, " for station ", sta, " and table ", table,"\n"))

  # Backup
  if(bku == TRUE) d_backup(db.sqlite)
  conn <- dbConnect(SQLite(),db.sqlite)

  # Create
  if (op %in% c("C","c")) {
    capteur <- list(Type_Station = as.character(NA), Id_Station = as.character(sta),
                    Capteur= as.character(sen), Tabl= as.character(table),
                    Nature= as.character(NA), Description = as.character(NA),
                    Commentaire= as.character(NA), Code_Limni= as.character(NA),
                    Principal = as.logical(NA), Fictif = as.logical(NA),
                    Maj_Journaliers = as.logical(NA), Maj_Traduction = as.logical(NA),
                    Acquisition_Auto = as.logical(NA), Operationnel = as.logical(NA),
                    Liste_Inst = as.character(NA), Liste_Jour = as.character(NA),
                    Liste_Mois = as.character(NA), Agregation = as.character(NA),
                    Decalage_Temps = as.numeric(NA), Mini = as.numeric(NA),
                    Maxi = as.numeric(NA), Gradient_Maxi = as.numeric(NA),
                    Precision = as.numeric(NA), Decimales = as.numeric(NA),
                    Pente = as.numeric(NA))
    if(length(name_fld) == 1 && is.na(name_fld)) name_fld <- NA
    else for(i in 1:length(capteur)){
      j <- 0
      repeat {
        j <- j+1
        if(j > length(name_fld)) break
        if(name_fld[j] == names(capteur[i])) capteur [i] <- value_fld [j]
      }
    }
    capteur <- as.data.frame(capteur, stringsAsFactors = FALSE)
    dbWriteTable(conn, "SS", capteur, append = TRUE)
    message("\nSensor ", sen, " for station ", sta, " and table ",table," created")
    dbDisconnect(conn)
  }

  # Modify
  if (op %in% c("M","m")){
    sen1 <- paste0("'",sen,"'")
    selection <- paste("SELECT * FROM SS WHERE Capteur =", sen1 )
    capteur <- dbGetQuery(conn, selection)
    if(length(name_fld) != 1 || is.na(name_fld)) for(i in 1:length(capteur)){
      j <- 0
      repeat {
        j <- j+1
        if(j > length(name_fld)) break
        if(name_fld[j] == names(capteur[i])) capteur [i] <- value_fld [j]
      }
    }
    capteur <- as.data.frame(capteur, stringsAsFactors = FALSE)
    dbWriteTable(conn, "SS", capteur, overwrite = TRUE)
    message("\nSensor ", sen, " for station ", sta,  " modified.")
    dbDisconnect(conn)
  }

  # Remove
  if (op %in% c("R","r")){
    sta1 = paste0("'",sta,"'")
    sen1 <- paste0("'",sen,"'")
    table1 <- paste0("'",table,"'")
    selection <- paste ("DELETE FROM", table1, " WHERE Id_Station = ", sta1,
      "AND Capteur = ", sen1)
    dbSendQuery(conn, selection)
    selection <- paste ("DELETE FROM SS WHERE Id_Station = ", sta1,
      # "AND Tabl = ", table1, "AND Tabl = ", sen1)
      "AND Tabl = ", table1)
dbSendQuery(conn, selection)
    message("\nSensor ", sen, " for station ", sta, " and table ", table, " is removed with all its data.")
  dbDisconnect(conn)
  }

}
