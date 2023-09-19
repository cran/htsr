#' @title Shiny app: export hts files from a sqlite data base
#'
#' @author P. Chevallier - Apr 2020 - Sep 2023
#'
#' @description Shiny application of the \code{\link{d_exp_hts}} function
#'
#' @details
#'  Complete the requested information in the left panel, then press the submit button in
#' order to extract the file. If you want to display the plot of the extracted file,
#' choose "line" or "bar" and press the plot button.
#'
#' @return a shiny session


# Define UI ------
ds_exp_hts <- function () {

	requireNamespace("shiny", quietly = TRUE)
	requireNamespace("shinyFiles", quietly = TRUE)
	requireNamespace("tibble", quietly = TRUE)
	requireNamespace("dplyr", quietly = TRUE)
	requireNamespace("lubridate", quietly = TRUE)
	requireNamespace("ggplot2", quietly = TRUE)
#	requireNamespace(htsr)


	ui <- fluidPage(
		theme = NULL,
		titlePanel("hts file import"),

		fluidRow(sidebarLayout(
			sidebarPanel(
				width = 5,
				shinyFilesButton("file", "File select", "Please select a sqlite data base",
												 multiple = FALSE, viewtype = "detail", class = "btn btn-primary"),
				textInput("Station_id", "Station ID"),
				textInput("Sensor_id", "Sensor ID"),
				checkboxInput("Set_time", "Reduce time interval", value = FALSE),
				dateRangeInput("dates", "Time interval range"),
				actionButton("submit", "Submit", class = "btn btn-warning"),
				radioButtons(
					"plot_opt",
					br("Plot the extracted file"),
					c("line" = "line", "bar" = "bar"),
					selected = "line"
				),
				actionButton("plot", "Plot", class = "btn btn-success"),
				hr(),
				actionButton("close", "Done", class="btn btn-danger")
			),
			mainPanel(
				width = 7,
				textOutput("fsq"),
				textOutput("extract_written"),
				plotOutput("file_plot")
			)
		))
	)


	server <- function(input, output, session) {

		volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())

		shinyFileChoose(input, "file", roots = volumes, session = session,
										filetypes="sqlite")

		observeEvent(input$file, {
			tab <- parseFilePaths(volumes, input$file)
			fsq <- as.character(tab[1,4])
			# output$tab <- renderTable({tab})
			output$fsq <- renderText({paste("Selected sqlite data base:",fsq)})
		})

		re <- eventReactive (input$submit, ({
			req(input$file)
			tab <- parseFilePaths(volumes, input$file)
			fsq <- as.character(tab[1,4])
			output$fsq <- renderText({paste("Selected sqlite data base:",fsq)})

			sta <- input$Station_id
			sen <- input$Sensor_id
			sta1 <- paste0("'",sta,"'")
			wd <- paste0(dirname(fsq),"/")

			fileo <- paste0(wd, input$Sensor_id,"_",
											input$Station_id,".hts")
			compute <- TRUE

			if (sta == ""){
				compute <- FALSE
				rep <- paste0("A station id is mandatory!")
			}
			if (compute && sen == ""){
				compute <- FALSE
				rep <- paste0("A sensor id is mandatory!")
			}
			if (compute) {
				conn <- dbConnect(SQLite(),fsq)
				x <- dbReadTable(conn, "ST")
				nom <- x$Id_Station
				if (!(sta %in% nom)){
					compute <- FALSE
					rep <- paste0("The station ", sta, " is not in the data base!")
				}
				dbDisconnect(conn)
			}
			if (compute) {
				conn <- dbConnect(SQLite(),fsq)
				selection <- paste ("SELECT * FROM SS WHERE Id_station =",sta1)
				x <- dbGetQuery(conn, selection)
				nom <- x$Capteur
				if (!(sen %in% nom)){
					compute <- FALSE
					rep <- paste0("The sentor ", sen, " not exists for the station ", sta, "!")
				}
				dbDisconnect(conn)
			}
			if (compute) {
				tstab <-htsr::d_exp_hts(fsq = fsq,
																sta,
																sen,
																rtime = input$Set_time,
																dstart=as.character((input$dates[1])),
																dend=as.character((input$dates[2])),
																rplot = FALSE)
				save (file=fileo, tstab)
				rep <- paste0("File written: ", fileo)
			}

			po <- eventReactive (input$plot, {
				htsr::z_set(file.names= fileo, plot.label = sen,title = sta)
				if (input$plot_opt=="bar") htsr::p_bar()
				else htsr::p_line()
			})

			output$file_plot <- renderPlot({po()})

			return(rep)
		}))

		output$extract_written <- renderText({re()})

		observeEvent(input$close, {stopApp()})

	}

	# Run the app
	shinyApp(ui = ui, server = server)
}
