#' @title Inventory of the stations of an htsr data base
#'
#' @author P. Chevallier - Dec 2023
#'
#' @description The function display a web page in order to produce an inventory of the
#' stations in an htsr data base.
#'
#' @details
#'  Complete the requested information in the left panel, then press the submit button.
#'  When finished press "done".
#'
#'  If "Output format" is "none", the results are displayed on the screen, If it is "xlsx",
#'  or "csv" (, as separator) or "csv2" (; as separator), the corresponding file
#'  with a station list is written.
#'
#' @return
#' A table with the inventory of stations in the data base.

ds_inventory_station <- function (){

	requireNamespace("shiny", quietly = TRUE)
	requireNamespace("shinyFiles", quietly = TRUE)

	# Define UI
	ui <- fluidPage(
		theme = NULL,

		titlePanel("Data base station inventory"),

		sidebarLayout(
			sidebarPanel(width = 5,
									 shinyFilesButton("file", "Select database", "Please select a sqlite data base",
									 								 multiple = FALSE, viewtype = "detail", class="btn btn-primary"),
									 br(), br(),
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

			form.out <- input$filetyp
			if (form.out == "none") form.out <- NA
			if (!is.na(form.out)){
				if (form.out == "csv") form.out <- "csv,"
				if (form.out == "csv2") form.out <- "csv;"
				if (form.out == "xlsx") form.out <- "xlsx"
			}

			d_inventory_station <- function(fsq, form.out) {

				requireNamespace("RSQLite", quietly = TRUE)
				Type_Station<-Id_Station<-Nom<-Latitude<-Longitude<-Altitude<-NULL

				# lecture de la base de données
				conn <- dbConnect(SQLite(),fsq)
				ltable <- dbListTables(conn)
				if(!("ST" %in% ltable)) return(warning("\nNo table 'ST'.\n"))
				selection <- paste ("SELECT * FROM ST")
				xxt <-dbGetQuery(conn, selection)
				dbDisconnect(conn)
				a <- select (xxt, Type_Station, Id_Station, Nom, Latitude, Longitude, Altitude)
				a <- arrange(a, Id_Station)

				if(!(is.na(form.out))) {
					nfse <- tools::file_path_sans_ext(fsq)
					fileo <- paste0(nfse, "_inv-sta")
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
						WriteXLS::WriteXLS (a, ExcelFileName=fileo,SheetNames= "Stations",
																col.names=TRUE, row.names=FALSE, na="#N/A")
					}
					message("\nThe files ",fileo," is written.\n")
				}
				return (a)
			}

			rep <- d_inventory_station (fsq, form.out)
			output$stationtable <- renderTable ({rep})
		})

		observeEvent(input$close, stopApp())
	}

	# Run the application
	shinyApp(ui = ui, server = server)
}

