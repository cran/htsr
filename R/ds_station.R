#' @title Create, Modify or Remove a station
#'
#' @author P. Chevallier - Jan 2018 - Sep 2023
#'
#' @description
#' Create, Modify or Remove a station.
#'
#' @details
#' If operation is Create, the fields Id_Station, Type_Station and
#' Name (name_st) are compulsory. The field Name can be modified afterwards.
#'
#' @details
#' If op is Create or Modify the following fields can be completed optionnaly: Country,
#' Zone, Sub-zone, Large basin, Basin, Small basin, River, Longitude, Latitude,
#' Altitude, Basin area, Manager.
#'
#' If op is Modify, station type and station id cannot be modified. The sensors and
#' data corresponding to the station are conserved.
#'
#' If op is Remove, all data and sensors of the station are removed.
#'
#'
#' @return
#' Station created, modified ou removed from the data base

ds_station <- function(){

	requireNamespace("shiny", quietly = TRUE)
	requireNamespace("shinyFiles", quietly = TRUE)
	requireNamespace("RSQLite", quietly = TRUE)

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

	# fonction exsta
	exsta <- function (fsq, stationID){
		conn <- RSQLite::dbConnect(RSQLite::SQLite(),fsq)
		sel <- paste0 ("SELECT * FROM ST  WHERE Id_Station = '", stationID,"'")
		lsta <- RSQLite::dbGetQuery(conn, sel)
		nom_sta <- lsta$Nom
		pays <- lsta$Pays
		zon <- lsta$Zone
		sszon <- lsta$SousZone
		grbas <- lsta$GrandBassin
		bas <- lsta$Bassin
		ptbas <- lsta$PetitBassin
		riv <- lsta$Riviere
		lon <- lsta$Longitude
		lat <- lsta$Latitude
		alt <- lsta$Altitude
		bv <- lsta$Superficie_bv
		ges <- lsta$Gestionnaire
		typ <- lsta$Type_Station
		x <- c(nom_sta, pays, zon, sszon, grbas, bas, ptbas, riv, lat, lon, alt, bv, ges, typ)
		RSQLite::dbDisconnect(conn)
		return(x)
	}

	# Define UI
	ui <- fluidPage(

		titlePanel("Create, remove or modify a station"),

		fluidRow(
			shinyFilesButton("file", "Sqlite File select", "Please select a sqlite data base",
											 multiple = FALSE, viewtype = "detail", class ="btn btn-primary"),
			textOutput("fsq"),
			hr()
		),

		sidebarLayout(
			sidebarPanel(width= 5,
									 textInput("Id_Station", "Station ID*"),
									 actionButton("confirm1", "Confirm station", class = "btn btn-info"), br(),
									 div("Confirm station before ANY operation!"), br(),
									 textInput("Nom", "Name*"),

									 conditionalPanel(
									 	condition = ("input.op != 'Create'"),
									 	textOutput("Nom1"),
									 	textOutput("typ1"),
									 ),

									 radioButtons('op', "Operation", c("Create", "Modify", "Remove")),

									 conditionalPanel(
									 	condition = ("input.op == 'Create'"),
									 	radioButtons('typ', "Type", c("Hydro", "Meteo"))
									 ),


									 actionButton("confirm2", "Confirm operation", class = "btn btn-warning"),
									 br(),br(),
									 actionButton("close", "Done", class = "btn btn-danger")
			),
			mainPanel(width= 7,
								textOutput("Message"), br(),
								# verbatimTextOutput("Resultat"),
								hr(),
								h3("Optional information fields"),
								splitLayout(
									textInput("Pays", "Country"),
									textOutput("Pays1")),
								splitLayout(
									textInput("Zone", "Zone"),
									textOutput("Zone1"),
									textInput("SousZone", "Sub-zone"),
									textOutput("SousZone1")),
								splitLayout(
									textInput("GrandBassin", "Large Basin"),
									textOutput("GrandBassin1"),
									textInput("Bassin", "Basin"),
									textOutput("Bassin1")),
								splitLayout(
									textInput("PetitBassin", "Small basin"),
									textOutput("PetitBassin1"),
									textInput("Riviere", "River"),
									textOutput("Riviere1")),
								splitLayout(
									textInput("Longitude", "Longitude  (deg.)", value = NA),
									textOutput("Longitude1"),
									textInput("Latitude", "Latitude (deg.)", value = NA),
									textOutput("Latitude1")),
								splitLayout(
									textInput("Altitude", "Altitude (m)", value = NA),
									textOutput("Altitude1"),
									textInput("Superficie_bv", "Basin area (km2)", value = NA),
									textOutput("Superficie_bv1")),
								splitLayout(
									textInput("Gestionnaire", "Manager"),
									textOutput("Gestionnaire1")),
								br(),
			)
		)
	)

	# Define server
	server <- function(input, output, session) {

		options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
		volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
		shinyFileChoose(input, "file", roots = volumes, session = session,
										filetypes="sqlite")

		# Upload files
		observeEvent(input$file, {
			tabfile <- parseFilePaths(volumes, input$file)
			fsq <- as.character(tabfile[1,4])
			output$fsq <- renderText({fsq})

			# confirm station
			observeEvent(input$confirm1, ({
				req(input$file)
				tabfile <- parseFilePaths(volumes, input$file)
				fsq <- as.character(tabfile[1,4])
				sta = input$Id_Station

				x <- exsta(fsq=fsq , sta)

				if(is.na(x[1])) {
					output$Message <- renderText({
						"The station not exists in the data base, but it can be created
        and a name is requested!"})
				} else {
					output$Message <- renderText({
						"The station exists in the data base"})
				}

				output$Nom1 <- renderText({x[1]})
				output$Pays1 <- renderText({x[2]})
				output$Zone1 <- renderText({x[3]})
				output$SousZone1 <- renderText({x[4]})
				output$GrandBassin1 <- renderText({x[5]})
				output$Bassin1 <- renderText({x[6]})
				output$PetitBassin1 <- renderText({x[7]})
				output$Riviere1 <- renderText({x[8]})
				lati <- round(as.numeric(x[9],5))
				long <- round(as.numeric(x[10],5))
				alti <- round(as.numeric(x[11],0))
				area <- round(as.numeric(x[12],0))
				output$Longitude1 <- renderText({as.character(long)})
				output$Latitude1 <- renderText({as.character(lati)})
				output$Altitude1 <- renderText({as.character(alti)})
				output$Superficie_bv1 <- renderText({as.character(area)})
				output$Gestionnaire1 <- renderText({x[13]})
				output$typ1 <- renderText({paste ("type :", x[14])})
			}))

			# confirm operation
			observeEvent(input$confirm2, ({
				req(input$file, input$confirm1)
				tabfile <- parseFilePaths(volumes, input$file)
				fsq <- as.character(tabfile[1,4])
				sta = input$Id_Station

				x <- exsta(fsq=fsq , sta)

				name_fld = c("Pays", "Zone", "SousZone",
										 "GrandBassin", "Bassin", "PetitBassin", "Riviere",
										 "Longitude", "Latitude", "Altitude", "Superficie_bv",
										 "Gestionnaire")

				# Create
				if(input$op == "Create") {
					ope <- "C"
					name_st <- input$Nom
					if(input$typ == "Hydro") ty_st <- "H" else ty_st <- "M"
					value_fld = c(input$Pays,
												input$Zone, input$SousZone,
												input$GrandBassin, input$Bassin,
												input$PetitBassin, input$Riviere,
												as.numeric(input$Longitude),
												as.numeric(input$Latitude),
												as.numeric(input$Altitude),
												as.numeric(input$Superficie_bv),
												input$Gestionnaire)
					for (i in 1:12)
						if (is.null (value_fld[i])) value_fld[i] <- NA
				}

				# Remove
				if(input$op == "Remove") {
					name_st <- input$Nom
					ope <- "R"
				}

				# Modify
				if(input$op == "Modify") {
					ope <- "M"
					for (i in 2:13) if (is.null (x[i])) x[i] <- NA
					if(input$Nom != "") name_st <- input$Nom else name_st <- x[1]
					if(input$Pays != "") pays1 <- input$Pays else pays1 <- x[2]
					if(input$Zone != "") zone1 <- input$Zone else zone1 <- x[3]
					if(input$SousZone != "") souszone1 <- input$SousZone else souszone1 <- x[4]
					if(input$GrandBassin != "") grandbassin1 <- input$GrandBassin else grandbassin1 <- x[5]
					if(input$Bassin != "") bassin1 <- input$Bassin else bassin1 <- x[6]
					if(input$PetitBassin != "") petitbassin1 <- input$PetitBassin else petitbassin1 <- x[7]
					if(input$Riviere != "") riviere1 <- input$Riviere else riviere1 <- x[8]
					if(input$Longitude != "") longitude1 <- as.numeric(input$Longitude) else longitude1 <- x[9]
					if(input$Latitude != "") latitude1 <- as.numeric(input$Latitude) else latitude1 <- x[10]
					if(input$Altitude != "") altitude1 <- as.numeric(input$Altitude) else altitude1 <- x[11]
					if(input$Superficie_bv != "") superficie1 <- as.numeric(input$Superficie_bv) else superficie1 <- x[12]
					if(input$Gestionnaire != "") gestionnaire1 <- input$Gestionnaire else gestionnaire1 <- x[13]
					value_fld = c(pays1, zone1, souszone1, grandbassin1, bassin1, petitbassin1,
												riviere1, longitude1, latitude1, altitude1, superficie1, gestionnaire1)
				}

				# Operation

				result <- d_station(fsq=fsq , op = ope, sta = sta,
														ty_st = ty_st, name_st = name_st, name_fld = name_fld,
														value_fld = value_fld, bku = TRUE)

				output$Resultat <- renderPrint({"Done !"})

			}))
		})

		#STOP
		observeEvent(input$close, stopApp())
	}

	# Run the application
	shinyApp(ui = ui, server = server)
}
