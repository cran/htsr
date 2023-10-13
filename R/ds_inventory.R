#' @title Inventory of an htsr data base
#'
#' @author P. Chevallier - Jan 2019 - Sep 2023
#'
#' @description The function display a web page in order to produce an inventory of the
#' stations and of sensors included in an htsr data base.
#'
#' @details
#'  Complete the requested information in the left panel, then press the submit button.
#'  If the station field is empty, the function will return the list of the stations
#'  in the data base. If the station field is filled, the function will return the
#'  list of the station sensors in the data base. When finished press "done".
#'
#'  If "Output format" is "none", the results are displayed on the screen, If it is "xlsx", an Excel
#'  file with two sheets is produced. If it is "csv" (, as separator) or "csv2" (; as separator),
#'  two csv files are produced with station and sensor lists.
#'

#' @return
#' Two tables with the inventory of stations and sensors of a data base.

ds_inventory <- function (){

	requireNamespace("shiny", quietly = TRUE)
	requireNamespace("shinyFiles", quietly = TRUE)
	requireNamespace("waiter", quietly = TRUE)

	# Define UI
	ui <- fluidPage(
		theme = NULL,
		waiter::use_waiter(),

		titlePanel("Data base inventory"),

		sidebarLayout(
			sidebarPanel(width = 5,
									 shinyFilesButton("file", "File select", "Please select a sqlite data base",
									 								 multiple = FALSE, viewtype = "detail", class="btn btn-primary"),
									 br(), br(),
									 div("Station_id blank: all stations are displayed"),
									 div("Station_id filled: its sensors are displayed"),

									 textInput("Station_id", "Station ID"),
									 selectInput("filetyp", "Output format" , choices =
									 						c("none", "xlsx", "csv", "csv2")),
									 actionButton("station", "Submit", class="btn btn-warning"),
									 br(),br(),
									 actionButton("close", "Done", class="btn btn-danger")
			),

			mainPanel(width =7,
								textOutput("fsq"),
								br(),
								tableOutput("stationtable")
			)
		)
	)

	# Define server
	server <- function(input, output, session) {
		options(shiny.maxRequestSize = 1000 * 1024 ^ 2)

		volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
		shinyFileChoose(input, "file", roots = volumes, session = session, filetypes="sqlite")

		observeEvent(input$file, {
			tab <- parseFilePaths(volumes, input$file)
			fsq <- as.character(tab[1,4])
			output$fsq <- renderText({paste("Selected sqlite data base:",fsq)})
		})

		observeEvent(input$station, {
			req(input$file)
			tab <- parseFilePaths(volumes, input$file)
			fsq <- as.character(tab[1,4])

			st <- input$Station_id
			if (st == "") st <- NA
# browser()
			form.out <- input$filetyp
			if (form.out == "none") form.out <- NA
			if (!is.na(form.out)){
				if (form.out == "csv") form.out <- "csv,"
				if (form.out == "csv2") form.out <- "csv;"
				if (form.out == "xlsx") form.out <- "xlsx"
			}

			waiter <- waiter::Waiter$new()
			waiter$show()
			on.exit(waiter$hide())

			d_inventory <- function(fsq, sta_sen=NA, form.out=NA){

				#Warning
				if (!file.exists(fsq))
					return(warning("\nThis data base doesn't exist, Verify!\n"))
				if(!(form.out %in% c(NA, "xlsx", "csv,", "csv;"))) form.out <- NA
				conn <- RSQLite::dbConnect(RSQLite::SQLite(),fsq)
				ltab <- RSQLite::dbListTables(conn)
				if(!("ST" %in% ltab)){
					RSQLite::dbDisconnect(conn)
					return(warning("\nThis data base doesn't have station table.\n"))
				}

				# Verify station list
				sel <- paste ("SELECT * FROM ST")
				lsta <- RSQLite::dbGetQuery(conn, sel)
				nsta <- nrow(lsta)
				if (nsta == 0){
					RSQLite::dbDisconnect(conn)
					return(warning("\nNo station in the data base\n"))
				}

				# Initialisation
				stac <- nom_stac <-senc <- descric <-tablec <- nbrc <- d_endc <- d_startc <- NULL
				nosensor <- nostation <- NULL
				Id_station <- Sensor <- NULL
				Altitude <- Id_Station <- Latitude <- Longitude <- Nom <- Pays <- Station_type <- NULL
				Superficie_bv <-  Type_Station <- NULL

				# tableau x1
				x1 <- dplyr::select(lsta, Id_station = Id_Station, Station_type = Type_Station,
														Station_name = Nom, Country = Pays, Latitude,
														Longitude, Altitude, Basin_areas = Superficie_bv)
				x1 <- dplyr::arrange(x1, Station_type, Id_station)

				# Loop Capteur
				# Cas où sta_sen est NA
				if (is.na(sta_sen)) {
					for(i in 1: nsta){
						sta <- lsta$Id_Station[i]
						nom_sta <- lsta$Nom[i]
						sta1 <- paste0("'",sta, "'")
						sel <- paste ("SELECT * FROM SS WHERE Id_Station =", sta1)
						lsen <- RSQLite::dbGetQuery(conn, sel)
						nsen <- nrow(lsen)
						if (nsen == 0) {
							message("The station ", sta, " has no sensor")
						}
						else for(j in 1: nsen) {
							sen <- lsen$Capteur[j]
							sen1 <- paste0("'",sen, "'")
							sel <- paste ("SELECT * FROM SS WHERE Id_Station =", sta1,
														"AND Capteur =", sen1)
							x <- RSQLite::dbGetQuery(conn, sel)
							descri <- x$Description
							tablex <- x$Tabl
							sel <- paste ("SELECT * FROM", tablex, "WHERE Id_Station =", paste0("'",sta, "'"),
														"AND Capteur =", paste0("'",sen, "'"))
							x <- RSQLite::dbGetQuery(conn, sel)
							nbr <- nrow(x)
							if (nbr > 0) {
								xd <- as.POSIXct(x$Date, origin = "1970-01-01", tz = "UTC")
								d_start <- min(xd)
								d_end <- max(xd)
							} else d_start <- d_end <- NA
							stac <- c(stac, sta)
							nom_stac <- c(nom_stac, nom_sta)
							senc <- c(senc, sen)
							descric <- c(descric, descri)
							tablec <- c(tablec, tablex)
							nbrc <- c(nbrc, nbr)
							d_endc <- c(d_endc, d_end)
							d_startc <- c(d_startc, d_start)
						}
					}
				} else {

					# Cas où sta_sen est une station
					# Cas où sta_sen est dans la liste des stations
					if(sta_sen %in% lsta$Id_Station) {
						k <- 0
						for (j in 1:length(lsta$Id_Station)) if(sta_sen == lsta$Id_Station[j]) k <- j
						sta <- lsta$Id_Station[k]
						nom_sta <- lsta$Nom[k]
						sta1 <- paste0("'",sta, "'")
						sel <- paste ("SELECT * FROM SS WHERE Id_Station =", sta1)
						lsen <- RSQLite::dbGetQuery(conn, sel)
						nsen <- nrow(lsen)
						if (nsen == 0) nosensor <- sta
						# if (nsen == 0) next
						else for(j in 1: nsen) {
							sen <- lsen$Capteur[j]
							sen1 <- paste0("'",sen, "'")
							sel <- paste ("SELECT * FROM SS WHERE Id_Station =", sta1,
														"AND Capteur =", sen1)
							x <- RSQLite::dbGetQuery(conn, sel)
							descri <- x$Description
							tablex <- x$Tabl
							sel <- paste ("SELECT * FROM", tablex, "WHERE Id_Station =", paste0("'",sta, "'"),
														"AND Capteur =", paste0("'",sen, "'"))
							x <- RSQLite::dbGetQuery(conn, sel)
							nbr <- nrow(x)
							if (nbr > 0) {
								xd <- as.POSIXct(x$Date, origin = "1970-01-01", tz = "UTC")
								d_start <- min(xd)
								d_end <- max(xd)
							} else d_start <- d_end <- NA
							stac <- c(stac, sta)
							nom_stac <- c(nom_stac, nom_sta)
							senc <- c(senc, sen)
							descric <- c(descric, descri)
							tablec <- c(tablec, tablex)
							nbrc <- c(nbrc, nbr)
							d_endc <- c(d_endc, d_end)
							d_startc <- c(d_startc, d_start)
						}

						# Cas où sta_sen n'est pas dans la liste des stations
					} else nostation <- sta_sen

				}
				# tableau x2
				if (!is.null(nostation)) x2 <- tibble (station = nostation, comment = " is not in the data base")
				else {
					if (!is.null(nosensor)) x2 <- tibble (station = nosensor, comment = " has no sensor")
					else {
						x2 <- tibble::tibble(Id_station = stac, Station_name = nom_stac, Sensor = senc, Description = descric,
																 Table = tablec, Nb_Rec = nbrc, Date_start = as_datetime(d_startc), Date_end = as_datetime(d_endc))
						x2 <- dplyr::arrange(x2, Id_station, Sensor)
					}
				}

				# déconnexion et ecriture des résultats
				RSQLite::dbDisconnect(conn)
				if(is.na(form.out)== TRUE) {
					a <- list(x1, x2)
					return(a)
				} else {
					nfse <- tools::file_path_sans_ext(fsq)
					fileo1 <- paste0(nfse, "_inv-sta")
					fileo2 <- paste0(nfse, "_inv-sen")
					if(form.out== "csv,") {
						fileo1 <- paste0(fileo1,".csv")
						fileo2 <- paste0(fileo2,".csv")
						write.csv (x1,file=fileo1, row.names=FALSE)
						write.csv (x2,file=fileo2, row.names=FALSE)
						return(message("\nThe files ",fileo1," and ", fileo2," are written.\n"))
					}
					if(form.out== "csv;"){
						fileo1 <- paste0(fileo1,".csv")
						fileo2 <- paste0(fileo2,".csv")
						write.csv2 (x1,file=fileo1, row.names=FALSE)
						write.csv2 (x2,file=fileo2, row.names=FALSE)
						return(message("\nThe files ",fileo1," and ", fileo2," are written.\n"))
					}
					if(form.out== "xlsx"){
						fileo <- paste0(paste0(nfse, "_inv"),".xlsx")
						xx <- list(x1,x2)
						WriteXLS::WriteXLS (xx, ExcelFileName=fileo,SheetNames=c("Stations","Sensors"),
																col.names=TRUE, row.names=FALSE, na="#N/A")
						return(message("\nThe file ",fileo," is written.\n"))
					}
				}
			}


			a <- d_inventory(fsq = fsq, sta_sen = st, form.out = form.out)

			if(!(is.null(a))){
				if (is.na(st)) {
					rep <- a[[1]]
				} else {
					rep <- a[[2]]
					if (ncol(rep) > 2) {
						rep$Date_start <- as.character(rep$Date_start)
						rep$Date_end <- as.character(rep$Date_end)
					}
				}
			}
			else {
				if(form.out == "xlsx")
					fw <- paste0(tools::file_path_sans_ext(fsq),"_inv.xlsx")
				else
					fw <- c(paste0(tools::file_path_sans_ext(fsq),"_inv-sta.csv"),
									paste0(tools::file_path_sans_ext(fsq),"_inv-sen.csv"))
				rep <- tibble('file(s)_written'=fw)
			}

			output$stationtable <- renderTable ({rep})
		})
		observeEvent(input$close, stopApp())
	}

	# Run the application
	shinyApp(ui = ui, server = server)
}

