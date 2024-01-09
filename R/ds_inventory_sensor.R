#' @title Inventory of a station sensors of an htsr data base
#'
#' @author P. Chevallier  - Dec 2023
#'
#' @description The function display a web page in order to produce an inventory of the
#' sensors for a selected station in an htsr data base.
#'
#' @details
#'  Complete the requested information in the left panel, then press the submit button.
#'  When finished press "done".
#'
#'  If "Output format" is "none", the results are displayed on the screen, If it is "xlsx",
#'  or "csv" (, as separator) or "csv2" (; as separator), the corresponding file
#'  with a sensor list is written.
#'
#' @return
#' A table with the inventory of sensors of a selected station in the data base.

ds_inventory_sensor <- function (){

	requireNamespace("shiny", quietly = TRUE)
	requireNamespace("shinyFiles", quietly = TRUE)
	requireNamespace("waiter", quietly = TRUE)

	# Define UI
	ui <- fluidPage(
		theme = NULL,

		titlePanel("Inventory of the sensors of a station in a data base"),

		sidebarLayout(
			sidebarPanel(width = 5,
									 shinyFilesButton("file", "Select database", "Please select a sqlite data base",
									 								 multiple = FALSE, viewtype = "detail", class="btn btn-primary"),
									 br(), br(),
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
								textOutput("utc"),
								textOutput("nasta"),
								textOutput("nosta"),
								textOutput("nosen"),
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

			form.out <- input$filetyp
			if (form.out == "none") form.out <- NA
			if (!is.na(form.out)){
				if (form.out == "csv") form.out <- "csv,"
				if (form.out == "csv2") form.out <- "csv;"
				if (form.out == "xlsx") form.out <- "xlsx"
			}

			d_inventory_sensor <- function(fsq, sta, form.out) {

				requireNamespace("RSQLite", quietly = TRUE)
				Capteur <- NULL

				# lecture de la base de données
				conn <- dbConnect(SQLite(),fsq)
				selection <- paste ("SELECT * FROM PR WHERE Id_Station = ", sta)
				xxp <-tibble(dbGetQuery(conn, selection))
				selection <- paste ("SELECT * FROM WE WHERE Id_Station = ", sta)
				xxw <-tibble(dbGetQuery(conn, selection))
				dbDisconnect(conn)

				# xxp$Date <- as_date(xxp$Date)
				# xxw$Date <- as_date(xxw$Date)

				k <- 0
				# cas des precipitations
				xxp$Capteur <- as.factor(xxp$Capteur)
				list_capt <- levels(xxp$Capteur)
				nrec <- datedeb <- datefin <- vector(mode="numeric", length = length(list_capt))
				if (length(list_capt) !=0)
					for (i in 1:length(list_capt)){
						k <- k+1
						xx <- filter(xxp, Capteur == list_capt[i])
						nrec[i] <- nrow(xx)
						datedeb [i] <- min(xx$Date)
						datefin [i] <- max(xx$Date)
						if (k == 1) a <- tibble(
							"Station"= sta,"Sensor"=list_capt[i],"Records"=nrec[i],"Date_init"=datedeb[i], "Date_end"=datefin[i])
						else {
							b <- tibble(
								"Station"= sta,"Sensor"=list_capt[i],"Records"=nrec[i],"Date_init"=datedeb[i], "Date_end"=datefin[i])
							a <- bind_rows(a, b)
						}
					}

				# cas des capteurs meteo
				xxw$Capteur <- as.factor(xxw$Capteur)
				list_capt <- levels(xxw$Capteur)
				nrec <- datedeb <- datefin <- vector(mode="numeric", length = length(list_capt))
				if (length(list_capt) !=0)
					for (i in 1:length(list_capt)){
						k <- k+1
						xx <- filter(xxw, Capteur == list_capt[i])
						nrec[i] <- nrow(xx)
						datedeb [i] <- min(xx$Date)
						datefin [i] <- max(xx$Date)
						if (k == 1) a <- tibble(
							"Station"= sta,"Sensor"=list_capt[i],"Records"=nrec[i],"Date_init"=datedeb[i], "Date_end"=datefin[i])
						else {
							b <- tibble(
								"Station"= sta,"Sensor"=list_capt[i],"Records"=nrec[i],"Date_init"=datedeb[i], "Date_end"=datefin[i])
							a <- bind_rows(a, b)
						}
					}
				a$Date_init <- as_datetime(a$Date_init, tz= "CET")
				a$Date_end <- as_datetime(a$Date_end, tz= "CET")

				if(!(is.na(form.out))) {
					nfse <- tools::file_path_sans_ext(fsq)
					fileo <- paste0(nfse, "_inv-sen")
					if(form.out== "csv,") {
						fileo <- paste0(fileo,".csv")
						write.csv (a,file=fileo, row.names=FALSE)
					}
					if(form.out== "csv;"){
						fileo <- paste0(fileo,".csv")
						write.csv2 (a,file=fileo, row.names=FALSE)
					}
					if(form.out== "xlsx"){
						fileo <- paste0(fileo,".xlsx")
						WriteXLS::WriteXLS (a, ExcelFileName=fileo,SheetNames= "Sensors",
																col.names=TRUE, row.names=FALSE, na="#N/A")
					}
					message("\nThe files ",fileo," is written.\n")
				}
				return (a)
			}

			rep <- d_inventory_sensor(fsq = fsq, sta=st, form.out = form.out)
			rep$Date_init <- as.character(as_datetime(rep$Date_init, tz= "CET"))
			rep$Date_end <- as.character(as_datetime(rep$Date_end, tz= "CET"))
			rep$Records <- as.integer(rep$Records)

			output$utc <- renderText("Dates are given in UTC time zone.")

			output$stationtable <- renderTable ({rep})
		})

		observeEvent(input$close, stopApp())
	}

	# Run the application
	shinyApp(ui = ui, server = server)
}

