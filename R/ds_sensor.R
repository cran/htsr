#' @title Create, Modify or Remove a sensor
#'
#' @author P. Chevallier - Feb 2018-Sep 2023
#'
#' @description Create, Modify or Remove a sensor.
#'
#' @details
#' If operation is Create, the fields Station, Table and Sensor are compulsory and cannot be modified afterwards.
#'
#' Allowed entries for table are: WL (water levels), DI (discharges),
#' QU (Quality), PR (precipitations), WE (weather).
#'
#' If op is Create or Modify, the following text fields can be completed optionally: Nature,
#' Description, Comment.
#'
#' If op is Remove, all data corresponding to the sensor of the selected
#' station are removed.
#'
#' The data base is automatically backuped before any operation.
#'
#'
#' @return
#' Sensor created, modified or removed from the data base


ds_sensor <- function(){

	requireNamespace("shiny", quietly = TRUE)
	requireNamespace("shinyFiles", quietly = TRUE)
	requireNamespace("RSQLite", quietly = TRUE)
	requireNamespace("DBI", quietly = TRUE)

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


	# function confsta
	confsta <- function (fsq, sta) {
		conn <- dbConnect(SQLite(),fsq)
		sel <- paste0 ("SELECT * FROM ST  WHERE Id_Station = '", sta, "'")
		lsta <- RSQLite::dbGetQuery(conn, sel)
		stID <- (nrow(lsta) != 0)
		if(stID) {
			nom <- lsta$Nom
			rep <- "The station exists in the data base"
		} else {
			rep <- "The station not exists in the data base, but it can be created
    with the function ds_station (or d_station)."
		}
		dbDisconnect(conn)
		return(rep)
	}

	# function confsen
	confsen <- function (fsq, sta, sen) {
		conn <- dbConnect(SQLite(),fsq)
		sel <- paste0 ("SELECT * FROM SS  WHERE Id_Station = '", sta,"' AND Capteur = '",
									 sen, "'")
		lsen <- RSQLite::dbGetQuery(conn, sel)
		seID <- (sen %in% lsen$Capteur)
		if (seID) {
			Message2 <- "The sensor exists in the data base"
			MessageTab <- as.character(lsen$Tabl)
			MessageNat <- as.character(lsen$Nature)
			MessageDes <- as.character(lsen$Description)
			MessageCom <- as.character(lsen$Commentaire)
		} else {
			Message2 <- "The sensor not exists in the database for the station,
        but it can be created."
			MessageTab <- MessageNat <- MessageDes <- MessageCom <- NA
		}
		rep <- c(Message2, MessageTab, MessageNat, MessageDes, MessageCom)
		dbDisconnect(conn)
		return(rep)
	}

	# Define UI
	ui <- fluidPage(

		titlePanel("Create, remove or modify a sensor"),

		fluidRow(
			shinyFilesButton("file", "Select database", "Please select a sqlite data base",
											 multiple = FALSE, viewtype = "detail", class ="btn btn-primary"),
			textOutput("fsq"),
			hr()
		),

		sidebarLayout(
			sidebarPanel(width= 5,
									 splitLayout(
									 	textInput("Id_Station", "Station ID*"),
									 	textInput("Id_Sensor", "Sensor ID*")),
									 splitLayout(
									 	actionButton("confirmst", "Confirm station", class = "btn btn-info"),
									 	actionButton("confirmss", "Confirm sensor", class = "btn btn-info")
									 ),
									 br(),
									 div("Confirm station and sensor before ANY operation!"), br(),

									 radioButtons("op", "Operation (CAUTION with REMOVE, it is definitive !)",
									 						 c("Create", "Modify", "Remove")),

									 conditionalPanel(
									 	condition = "input.op != 'Create'",
									 	textOutput("tab1")
									 ),

									 conditionalPanel(
									 	condition = "input.op == 'Create'",
									 	radioButtons("tab", "Table (cannot be modified after creation)", c("Water levels", "Discharges",
									 																																		 "Quality", "Precipitation", "Weather")),
									 ),
									 br(),

									 actionButton("confirm2", "Confirm operation", class = "btn btn-warning"), br(),
									 hr(),
									 actionButton("close", "Done", class = "btn btn-danger")
			),

			mainPanel(width= 7,
								textOutput("Message1"),
								textOutput("Message2"),
								hr(),
								fluidRow(
									h3("Optional information fields"),
									column(6,
												 textInput("nature", "Nature"),
												 textOutput("nature1"),
												 br(),
									),
									column(6,
												 textInput("description", "Description"),
												 textOutput("description1")
									)
								),
								fluidRow(
									textAreaInput("comment", "Comment", rows = 3),
									textOutput("comment1")
								)
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

			# Confirm station
			observeEvent(input$confirmst, ({
				req(input$file)
				tabfile <- parseFilePaths(volumes, input$file)
				fsq <- as.character(tabfile[1,4])
				sta <- input$Id_Station
				output$Message1 <- renderText({confsta (fsq, sta)})
			}))

			# Confirm sensor
			observeEvent(input$confirmss, ({
				req(input$file, input$confirmst)
				tabfile <- parseFilePaths(volumes, input$file)
				fsq <- as.character(tabfile[1,4])
				sta <- input$Id_Station
				sen <- input$Id_Sensor
				rep <- confsen (fsq, sta, sen)
				output$Message2 <- renderText({rep[[1]]})
				output$tab1 <- renderText({paste("Table :",rep[[2]])})
				output$nature1 <- renderText({rep[[3]]})
				output$description1 <- renderText({rep[[4]]})
				output$comment1 <- renderText({rep[[5]]})
			}))

			# confirm operation
			observeEvent(input$confirm2, ({
				req(input$file, input$confirmst, input$confirmss)
				tabfile <- parseFilePaths(volumes, input$file)
				fsq <- as.character(tabfile[1,4])
				sta <- input$Id_Station
				sen <- input$Id_Sensor
				name_fld <- c("Nature", "Description", "Commentaire")

				nature1 <- description1 <- comment1 <- as.character(NA)

				if(input$tab == "Water levels") tab <- "WL"
				if(input$tab == "Discharges") tab <- "DI"
				if(input$tab == "Quality") tab <- "QU"
				if(input$tab == "Precipitation") tab <- "PR"
				if(input$tab == "Weather") tab <- "WE"
				if(input$nature != "") nature1 <- input$nature
				if(input$description != "") description1 <- input$description
				if(input$comment != "") comment1 <- input$comment
				value_fld <- c(nature1, description1, comment1)


				# Create
				if(input$op == "Create") {
					ope <- "C"
				}

				# Remove
				if(input$op == "Remove") {
					ope <- "R"
				}

				# Modify
				if(input$op == "Modify") {
					ope <- "M"
				}

				# Operation
				d_sensor(fsq=fsq, op=ope, sta=sta, sen=sen, table = tab, name_fld=name_fld,
								 value_fld=value_fld, bku = TRUE)
			}))
		})

		#STOP
		observeEvent(input$close, stopApp())
	}

	# Run the application
	shinyApp(ui = ui, server = server)
}

