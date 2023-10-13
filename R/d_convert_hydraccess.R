#' @title Convert a full Hydraccess database into a new htsr sqlite database (Windows only)
#'
#' @author P. Chevallier - Nov 2018-Nov 2020
#'
#' @description Because the Hydraccess application only works into a Windows environment,
#' this function cannot be applied on other platforms (Mas OS or Linux). Additionally,
#' the R session must be configured in 32b (see the htsr-package vignette).
#'
#' @param fsq Full name of the sqlite data base
#' @param db.hydraccess Full name of the hydraccess data base
#'
#' @seealso \code{\link{ds_inventory}} for displaying
#' the content of the sqlite data base;
#'  \code{\link{ds_exp_hts}} for extracting a time-series.
#'
#' @details
#' If the specified sqlite data base already exists, a confirmation
#' is requested to overwrite it.
#'
#' @details
#' An 32b ODBC Microsoft driver must be configured
#' in the "administrative tools" and installed for the
#' hydraccess data base. The correct functioning can be verified
#' using the sub-function u_test_rodbc(db.hydraccess),
#' which must be successful.
#'
#' @return A new or a replaced sqlite htsr data base.

#' @examples \dontrun{
#'
#' d_import_hydraccess("foo.sqlite","foo.mdb")
#' }
#'
#'

d_convert_hydraccess <- function(fsq,db.hydraccess){

	requireNamespace("RODBC", quietly = TRUE)

  # fonction u_newnomtable
  u_newnomtable <- function (nomtable) {
    nt <- c("Bassins", "Bassins_Grands", "Bassins_Petits", "Capteurs",
            "Capteurs_Comm", "Capteurs_HistApp", "Codes_Nature", "Codes_Origine",
            "Codes_Qualite", "Cotes", "Debits", "Dossiers_Stations",
            "Equipements", "Etal_Dates", "Etal_HK", "Etal_HQ", "Evenements",
            "Gestionnaires", "Helices", "Jaugeages", "Jaugeages_Dep", "Meteo",
            "Modes_Operatoires", "Parametrage", "Pluies", "Profils",
            "Profils_Data", "Qualite", "Regions", "Rivieres", "Stations_Base",
            "Stations_Equipement", "Temp_Stations2", "Vannes", "Zeros_NG",
            "Zones", "Zones_Pays", "Zones_Sous")
    nnt <- c("BA", "LB", "SB", "SS", "SM", "SH", "NC", "OC", "QC", "WL",
             "DI", "SF", "EQ", "CD", "SC", "LC", "EV", "MG", "PP", "DM", "DP",
             "WE", "OM", "SE", "PR", "PF", "PD", "QU", "RE", "RV", "ST", "SQ",
             "TS", "VA", "EZ", "ZO", "CO", "SZ")
    for (i in 1:38){
      if(nomtable != nt[i]) next
      else {
        k <- i
        break
      }
    }
    return (nnt[k])
  }

  # fonction u_test_robdc
  u_test_robdc <- function(db.hydraccess){
    if(Sys.info()[['sysname']] != "Windows") {
      warning ("STOP!")
      warning ("\nThis function must be used in a Windows Operating System !")
      return(warning (""))
    }
    if(Sys.info()[['machine']] != "x86") {
      warning ("STOP!")
      warning ("\nThe obdc connection must be configured in 32b!")
      return(warning (""))
    }
    channel <- RODBC::odbcConnectAccess2007(F)
    RODBC::odbcClose(channel)
    return(warning ("\nSuccessful!"))

  }



  # Initialisation et ouverture fsq
  TABLE_TYPE <- Table <- NULL
  if(Sys.info()[['sysname']] != "Windows") {
    warning ("STOP!")
    warning ("\nThis function must be used in a Windows Operating System!")
    return(warning (""))
  }
  if(Sys.info()[['machine']] != "x86") {
    warning ("STOP!")
    warning ("\nThe obdc connection must be configured in 32b!")
    return(warning (""))
  }

  Sys.setenv(TZ='UTC')
  ex <- strsplit(basename(db.hydraccess), split="\\.")[[1]][-1]

  # creation du fichier sqlite
  f <- fsq
  if(file.exists(f)== TRUE) {
    d_backup(f)
    file.remove(f)
  }
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), fsq)
  RSQLite::dbDisconnect(conn)

  nfse <- tools::file_path_sans_ext(fsq)
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), fsq)

  # ouverture fichier Hydraccess
  fh <- db.hydraccess
  channel <- RODBC::odbcConnectAccess2007(fh)

  ptm <- proc.time()

  # lecture hydraccess
  tx <- RODBC::sqlTables(channel)
  tx <- tibble::as_tibble(tx)
  tx0 <- dplyr::filter(tx, TABLE_TYPE=="TABLE")
  tn <- as.vector(tx0$TABLE_NAME)
  l_tn <- length(tn)

  # lecture et ecriture des tables
  for (i in 1:l_tn){
    nomtable <- tn[i]
    new.nomtable <- u_newnomtable(nomtable)
    x <- RODBC::sqlFetch(channel,nomtable)
    RSQLite::dbWriteTable(conn, nomtable, x)
    nomtable1 <- paste0("'",nomtable,"'")
    new.nomtable1 <- paste0("'",new.nomtable,"'")
    sel <- paste ("ALTER TABLE",nomtable1, "RENAME TO", new.nomtable1)
    rs <- RSQLite::dbSendQuery(conn, sel)
    dbClearResult(rs)
    txt <- paste("\nWriting table; old:", nomtable, "new:", new.nomtable)
    message(txt)
  }

  # rename in table columns
  RODBC::odbcClose(channel)

  tn <- c("SS", "SM", "SH", "WL", "DI", "CD", "SC", "LC", "EV",
    "DM", "DP", "WE", "PR", "PF", "PD", "QU", "EZ")
  for(i in 1:length(tn)){
    css <- RSQLite::dbReadTable(conn,tn[i])
#    if(nrow(css)==0) next
    RSQLite::dbRemoveTable(conn, tn[i])
    css$Table[css$Table=="C"] <- "WL"
    css$Table[css$Table=="D"] <- "DI"
    css$Table[css$Table=="Q"] <- "QU"
    css$Table[css$Table=="P"] <- "PR"
    css$Table[css$Table=="M"] <- "WE"
    css <- dplyr::rename(css, Tabl = Table)
    RSQLite::dbWriteTable(conn, tn[i], css)
    txt <- paste("\nThe column Tabl of", tn[i], "has been updated.")
    message(txt)
  }

  # finalisation
  texte <- proc.time()-ptm
  texte <- round(texte[1],1)
  message("\nExecution time: ", texte, " seconds\n")
  RSQLite::dbDisconnect(conn)
  message("\nBase ",fsq, " converted.\n")
}

