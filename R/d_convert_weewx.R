#' @title Convert a weewx data base into a htsr sqlite base
#'
#' @author P. Chevallier - Feb 2018 - Sep 2023
#'
#' @description Convert (or update) a weewx data base into a htsr sqlite base
#'
#' @param db.weewx Full name of the weewx data base
#' @param fsq Full name of the htsr data base
#' @param update (default = TRUE)
#' @param sta Station id (default = NA)
#' @param name_st Station name (default = NA)
#'
#' @details If update is TRUE, sta and name_st are unnecessary. I update is FALSE and
#' fsq is NA, fsq is named "weewx.sqlite".
#'
#'
#'
#' @examples \dontrun{
#'
#' d_convert_weewx("weewx.sql", "foo.sqlite")
#' }
#'

d_convert_weewx <- function(db.weewx, fsq = NA, update=TRUE, sta = NA, name_st = NA){

	# function d_station
	d_station <- function(fsq, op = "C", sta, ty_st = NA, name_st=NA,
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
			# selection <- paste ("DELETE FROM SS WHERE Id_station = ", sta1, "AND Capteur = ", sen1)
			# rs <- dbSendQuery(conn, selection)
			# dbClearResult(rs)
			selection <- paste ("DELETE FROM ST WHERE Id_station = ", sta1)
			rs <- dbSendQuery(conn, selection)
			dbClearResult(rs)

			message("\nStation ",name_st," with id ", sta, " is removed with all its data.")
			dbDisconnect(conn)
		}
	}

	# function d_sensor
	d_sensor <- function(fsq, op = "C", sta, sen, table,
											 name_fld=NA, value_fld=NA, bku = TRUE) {

		Id_Station <- NULL

		# Warnings and return
		if (!file.exists(fsq))
			return(warning("\nThis data base doesn't exist, Verify!\n"))

		if (!(op %in% c("C", "M", "R", "c", "m", "r")))
			return(warning("\nOperation not authorized\n"))

		if(op %in% c("C", "R", "c", "r")) {
			if ((is.na(sta) || is.na(sen) || is.na(table)))
				return(warning("\nStation and sensor id and table must be completed. \n"))
		}
		if(op %in% c("M", "m")) {
			if((is.na(sta) || is.na(sen)))
				return(warning("\nStation and sensor id must be completed. \n"))
		}
		if(op %in% c("C", "c")) {
			if (!(table %in% c("WL", "DI", "QU", "PR", "WE")))
				return(warning("\nTable not authorized. \n"))
		}
		conn <- dbConnect(SQLite(),fsq)
		ltable <- dbListTables(conn)
		dbDisconnect(conn)
		if(!("ST" %in% ltable))
			return(warning("\nNo table ST in the data base..\n"))
		if (op %in% c("C", "c") && (!("SS") %in% ltable))
			return(warning("\nNo table SS in the data base.\n"))
		conn <- dbConnect(SQLite(),fsq)
		selection <- paste ("SELECT * FROM ST")
		xxt <-dbGetQuery(conn, selection)
		dbDisconnect(conn)
		if(!(sta %in% xxt$Id_Station))
			return(warning("\nThe station ", sta, " doesn't exist in the ST table.\n"))

		# recherche de ty_st
		xxt <- dplyr::filter(xxt, Id_Station == sta)
		ty_st <- xxt$Type_Station

		#Warnings Create & Modify

		if (op %in% c("C","c","M", "m")){
			if (length(name_fld) != length(value_fld))
				return(warning("\nBoth lists field names and field values must have the same length.\n"))
			if(length(name_fld)==1 && is.na(name_fld)) name_fld <- NA
			else {
				for(i in 1:length(name_fld)) {
					if(!(name_fld[i] %in% c("Tabl",
																	"Nature", "Description", "Commentaire", "Code_Limni", "Liste_Inst",
																	"Liste_Jour", "Liste_Mois", "Agregation","Decalage_Temps", "Mini", "Maxi",
																	"Gradient_Maxi", "Precision", "Decimales", "Pente","Principal", "Fictif",
																	"Maj_Journaliers", "Maj_Traduction", "Acquisition_Auto", "Operationnel")))
						return(warning("\nField name not authorized.\n"))
					if(name_fld[i] %in% c("Tabl",
																"Nature", "Description", "Commentaire", "Code_Limni", "Liste_Inst",
																"Liste_Jour", "Liste_Mois", "Agregation") && !is.character(value_fld))
						return(warning("\nField value of ", name_fld[i]," must be character.\n"))
					if(name_fld[i] %in% c(
						"Decalage_Temps", "Mini", "Maxi", "Gradient_Maxi", "Precision",
						"Decimales", "Pente") && !is.numeric(value_fld[i]))
						return(warning("\nField value of ", name_fld[i]," must be numeric.\n"))
					if(name_fld[i] %in% c("Principal", "Fictif", "Maj_Journaliers",
																"Maj_Traduction", "Acquisition_Auto", "Operationnel") &&
						 !is.logical(value_fld[i]))
						return(warning("\nField value of ", name_fld[i]," must be logical.\n"))
				}
			}
		}

		# Sensor list
		conn <- dbConnect(SQLite(),fsq)
		sta1 <- paste0("'",sta,"'")
		selection <- paste ("SELECT * FROM SS WHERE Id_Station =", sta1)
		listcapt <-dbGetQuery(conn, selection)
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
		if(bku == TRUE) d_backup(fsq)

		# Create
		if (op %in% c("C","c")) {
			conn <- dbConnect(SQLite(),fsq)
			capteur <- list(Type_Station = ty_st, Id_Station = as.character(sta),
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
		}

		# Modify
		if (op %in% c("M","m")){
			conn <- dbConnect(SQLite(),fsq)
			sta1 <- paste0("'",sta,"'")
			sen1 <- paste0("'",sen,"'")
			selection <- paste("SELECT * FROM SS WHERE Id_station = ", sta1, "AND Capteur =", sen1 )
			capteur <- dbGetQuery(conn, selection)
			if(length(name_fld) != 1) {
				for( j in 1:length(value_fld)) {
					for (i in 1:length(capteur)) {
						if(name_fld[j] == names(capteur[i])) k <- i
					}
					capteur[1, k] <- value_fld[j]
				}
			} else {
				if (!is.na(name_fld)) {
					for (i in 1:length(capteur)) {
						if(name_fld == names(capteur[i])) k <- i
					}
					capteur[1, k] <- value_fld
				}
			}
			selection <- paste ("DELETE FROM SS WHERE Id_station = ", sta1, "AND Capteur = ", sen1)
			rs <- dbSendQuery(conn, selection)
			dbClearResult(rs)
			dbWriteTable(conn, "SS", capteur, append = TRUE)
			message("\nSensor ", sen, " for station ", sta,  " modified.")
		}

		# Remove
		if (op %in% c("R","r")){
			conn <- dbConnect(SQLite(),fsq)
			sta1 = paste0("'",sta,"'")
			sen1 <- paste0("'",sen,"'")
			table1 <- paste0("'",table,"'")
			selection <- paste ("DELETE FROM", table1, " WHERE Id_Station = ", sta1,
													"AND Capteur = ", sen1)
			rs <-dbSendQuery(conn, selection)
			dbClearResult(rs)
			selection <- paste ("DELETE FROM SS WHERE Id_station = ", sta1, "AND Capteur = ", sen1)
			rs <- dbSendQuery(conn, selection)
			dbClearResult(rs)

			message("\nSensor ", sen, " for station ", sta, " and table ", table, " is removed with all its data.")
		}
		dbDisconnect(conn)
	}
	# ------------------------------------------------


  # Warnings
  dn <- dirname(db.weewx)
  if(is.na(fsq) && !(update)) fsq <- paste0(dn,"/weewx.sqlite")
  if(update) {
  	if ((!file.exists(db.weewx) || !file.exists(fsq)))
    return(warning("\nWeewx and/or htsr database(s) don't exist, when an update is requested."))
	 } else {
	 	if((is.na(sta) || is.na(name_st)))
		return(warning("\nWhen update is false, station id and name are requested."))
	 }

  # Init ou recup fsq data base
  if(update) {
  	conn <- RSQLite::dbConnect(RSQLite::SQLite(), fsq)
  	selection <- paste ("SELECT Id_Station FROM ST" )
  	staa <- RSQLite::dbGetQuery(conn, selection)
  	sta <- staa[[1]]
  	selection <- paste ("SELECT Nom FROM ST" )
  	name_sta <- RSQLite::dbGetQuery(conn, selection)
  	name_st <- name_sta[[1]]
  	selection <- paste ("SELECT Date FROM WE" )
  	date <- RSQLite::dbGetQuery(conn, selection)
  	dmxwe <- max(date)
  	selection <- paste ("SELECT Date FROM PR" )
  	date <- RSQLite::dbGetQuery(conn, selection)
  	dmxpr <- max(date)
  	dmx <- max(dmxwe, dmxpr)
  	RSQLite::dbDisconnect(conn)
  } else d_create(fsq, cr_table = TRUE, bku = FALSE)


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
    date <- as.POSIXct(date, origin = "1970-01-01")
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
    date <- as.POSIXct(date, origin = "1970-01-01")
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
      date <- as.POSIXct(date, origin = "1970-01-01")
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
  date <- as.POSIXct(date, origin = "1970-01-01")
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
  date <- as.POSIXct(date, origin = "1970-01-01")
  selection <- paste ("SELECT", "sum",  "FROM archive_day_rain")
  value <- RSQLite::dbGetQuery(conn, selection)
  value <- value[,1]
  value <- value * 25.4 ; sensor <- "JPR"
  t <- tibble(date,value,sensor)
  x <- bind_rows(x,t)

  # Deconnexion base weewx
  RSQLite::dbDisconnect(conn)

  nrx <- nrow (x)
  if (update) {
  	x <- filter(x, date > dmx)
  	nrx <- nrow (x)
  } else {
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
  }

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
  if (update) message("\nBase ", fsq, " completed with ", nrx, " added records.")
  else message("\nBase ", fsq, " created with ", nrx, " records.")

}

#Fin
