#' @title Create or remove a table of a htsr sqlite base
#'
#' @author P. Chevallier - Jan-Feb 2018
#'
#' @description The function allows to create or remove of a tshm sqlite base. If
#' the base doesn't exist, it is created.
#'
#' @param fsq Full name of the data base
#' @param table Table name
#' @param op Create (default) or Remove C/R
#' @param bku Automatic Backup TRUE (default) / FALSE
#'
#' @seealso
#' \itemize{
#'  \item \code{\link{ds_inventory_station}} and \code{\link{ds_inventory_station}} to list the content of the base ;
#'  \item \code{\link{ds_exp_hts}} to extract a time-series
#' }
#'
#' @details
#' Possible table names : ST (Stations), SS (Sensors), WL (Water levels), DI (Discharges),
#' WE, (Weather), PR (Precipitations), QU (Quality)
#'
#'
#' @return
#' Table created or removed
#'

d_table <- function(fsq, table, op = "C", bku = TRUE) {

  if (!file.exists(fsq))
    return(warning("\nThis data base doesn't exist, Verify!\n"))
  if (!(op %in% c("C", "R", "c", "r")))
    return(warning("\nOperation not authorized!\n"))
  if (!(table %in% c(NA, "WL", "DI", "WE", "PR", "QU", "SS", "ST")))
    return(warning("\nTable name not authorized!\n"))

  conn <- dbConnect(SQLite(),fsq)
    ltable <- dbListTables(conn)
  dbDisconnect(conn)

  # No job cases
  if(table %in%  ltable && op %in% c("C","c"))
    return(warning("\nTable ", table, " already exists. Nothing is done."))
  if(!(table %in% ltable) && op %in% c("R","r"))
    return(warning("\nTable ", table, " doesn't exist and cannot be removed."))

  if (bku == TRUE) d_backup(fsq)
  conn <- dbConnect(SQLite(),fsq)

  # Create
  if (op %in% c("C","c")) {
      # cotes, debits, meteo, qualite
      df1 <- data.frame(Type_Station = character(), Id_Station = character(),
                        Capteur = character(), Tabl = character(), Date=as.Date(character()),
                        Valeur = numeric(), Origine = character(), Qualite = character())
      # pluies
      df2 <- data.frame(df1, Nature = character())
      # stations
      df3 <- data.frame(Ordre = character(), Type_Station = character(),
                        Id_Station = character(), Id_Secondaire = character(), Id_Tertiaire = character(),
                        Type_Meteo = character(),	Nom = character(), Pays = character(),
                        Zone = character(), SousZone = character(), GrandBassin = character(),
                        Bassin = character(), PetitBassin = character(), Riviere = character(),
                        Gestionnaire = character(), Latitude = numeric(),	Longitude = numeric(),
                        Altitude = numeric(), Superficie_bv = numeric(), Mois_Debut_Hydro = numeric(),
                        Debut_Activite = numeric(), Activite = logical(), 	Critere_OuiNon = logical(),
                        Critere_OuiNon2 = logical(), 	Critere_Numerique = numeric(),
                        Critere_Texte = character(), Nom_Observateur = character(), Adresse = character(),
                        Teletransmission = logical(), Enregistreur = logical(), Fictive = logical(),
                        Commentaire = character(), Flag = logical(), District = character(), Localite = character())
      # capteurs
      df4 <- data.frame(Type_Station = character(), Id_Station = character(),
                        Capteur = character(), Tabl = character(), Nature = character(),
                        Description = character(), Commentaire = character(), Code_Limni = character(),
                        Principal = logical(), Fictif = logical(), 	Maj_Journaliers = logical(),
                        Maj_Traduction = logical(), Acquisition_Auto = logical(), Operationnel = logical(),
                        Liste_Inst = character(), Liste_Jour = character(), Liste_Mois = character(),
                        Agregation = character(), 	Decalage_Temps = numeric(), Mini = numeric(),
                        Maxi = numeric(), Gradient_Maxi = numeric(), Precision = numeric(),
                        Decimales = numeric(), Pente = numeric())
      if(table %in% c("WL", "DI", "WE", "QU"))
        dbWriteTable(conn, table, df1)
      if(table %in% c("PR")) dbWriteTable(conn, table, df2)
      if(table %in% c("ST")) dbWriteTable(conn, table, df3)
      if(table %in% c("SS")) dbWriteTable(conn, table, df4)
      message("\nTable created: ",table, "\n")
  }

  # Effacer
  if (op %in% c("R","r")){
    dbRemoveTable(conn,table)
    message("\nTable removed ",table, "\n")
  }

# End
  tb_list <- dbListTables(conn)
  dbDisconnect(conn)
  return(message("\nTables in data base: ", tb_list))

}

#Fin
