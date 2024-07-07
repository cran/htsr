#' @title Convert Hubeau station files into a htsr sqlite base
#'
#' @author P. Chevallier - jul 2024
#'
#' @description Convert a Hubeau  hydrological file into a htsr sqlite base. It regards
#' the "basic" data file, which includes water level and discharge data. .
#'
#' @details
#' The data base is build from selected stations in the "stations.tar" file available on the data.eau.france web site.
#' This file must be first downloaded and extracted in the folder hubeau.dir. For the extraction the R
#' function untar() can be used.
#' Secondly Within the hubeau.dir, the file stations/stations.csv give the full list of the available stations. One or more station ids
#' must be chosen and included in the station.id list parameter.
#'
#' @details
#' The units of water level data is cm and of discharge data is m3/s.
#'
#' @param hubeau.dir Full path of the hubeau folder (character)
#' @param station.id Id list of the stations to convert (character)
#' @param fsqname Name of the sqlite data base without extension (default = "hubeau")


# function d_convert_hubeau
	d_convert_hubeau <- function (hubeau.dir, station.id, fsqname = "hubeau") {

		# function d_station
		d_station <- function(fsq, op = "C", sta, ty_st = NA, name_st=NA,
													name_fld=NA, value_fld=NA, bku = FALSE) {

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
							"Ordre", "Id_Secondaire", "Id_Tertiaire", "Type_Meteo",
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
		#---------------------------

		# function d_sensor
		d_sensor <- function(fsq, op = "C", sta, sen, table,
												 name_fld=NA, value_fld=NA, bku = FALSE) {

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

		#---------------------------

		# library(tidyverse)
		# library(htsr)
		# library(RSQLite)

		requireNamespace("RSQLite", quietly = TRUE)

		# creation base de données
		# fsq <- paste0(station.id,".sqlite")
		fsq <- paste0(hubeau.dir, "/",fsqname, ".sqlite")
		d_create(fsq)

		cdentite <- dtmesure <- hauteur <- qualh <- debit <- qualq <- NULL

		# creation stations
		x <- read_delim(file = paste0(hubeau.dir,"/stations/stations.csv"), delim = ";", col_names = TRUE, col_types = cols(.default = col_character()))
		x <- filter (x, cdentite %in% station.id)
		for (i in 1:length(station.id)) {
				d_station (fsq, op = "C", sta = x$cdentite[i], ty_st = "H",
																						name_st = x$lbstationhydro[i],
																						name_fld= c("Latitude", "Longitude", "Debut_Activite", "Pays"),
																						value_fld= c(x$latitude[i], x$longitude[i], x$dtmiseservice[i], "France"),
																						bku = TRUE)
		}

		# constitution du tableau de données
		for (i in 1:length(station.id)) {
			folder <- paste0(hubeau.dir,"/stations/data/",station.id[i])
			files <-  list.files(folder, pattern = ".", all.files = FALSE, recursive = TRUE, full.names = TRUE)
			for (j in 1:length(files)) {
				if (i==1 && j==1) x <- tibble(read.csv2(gzfile(files[1])))
				else x <- bind_rows (x, read.csv2(gzfile(files[j])))}
		}
		x$dtmesure <- force_tz(ymd_hms(x$dtmesure), tzone = "Europe/Paris")
		x$hauteur <- as.numeric(x$hauteur)
		x$debit <- as.numeric(x$debit) / 1000

		# cas hauteur
		for (i in 1:length(station.id)) d_sensor(fsq, op = "C", sta = station.id[i], sen = "IH", table = "WL", bku = FALSE)
		xx <- transmute(x,Type_Station="H",Id_Station=cdentite, Capteur="IH", Date = dtmesure, Valeur = hauteur, Tabl = "WL", Qualite = qualh)
		conn <- dbConnect(SQLite(),fsq)
		dbWriteTable(conn, name="WL", xx, overwrite = TRUE)
		dbDisconnect(conn)

		# cas debit
		for (i in 1:length(station.id)) d_sensor(fsq, op = "C", sta = station.id[i], sen = "IQ", table = "DI", bku = FALSE)
		xx <- transmute(x,Type_Station="H",Id_Station=cdentite, Capteur="IQ", Date = dtmesure, Valeur = debit, Tabl = "DI", Qualite = qualq)
		conn <- dbConnect(SQLite(),fsq)
		dbWriteTable(conn, name="DI", xx, overwrite = TRUE)
		dbDisconnect(conn)

	return(message("Data base", fsq, " created."))
	}





