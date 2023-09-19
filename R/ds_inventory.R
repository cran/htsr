#' @title Shiny app: inventory of htsr sqlite data base
#'
#' @author P. Chevallier - Sep 2020 - Sep 2023
#'
#' @description Shiny application of the \code{\link{d_inventory}} function
#'
#' @details
#'  Complete the requested information in the left panel, then press the submit button.
#'  If the station field is empty, the function will return the list of the stations
#'  in the data base. If the station field is filled, the function will return the
#'  list of the station sensors in the data base. When finished press "done".

#' @return a shiny session
#'

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
									 actionButton("station", "Submit", class="btn btn-warning"),
									 br(),br(),
									 actionButton("close", "Done", class="btn btn-danger")
			),

			mainPanel(width =7,
								textOutput("fsq"),
								br(),
								tableOutput("station")
			)
		)
	)

	# Define server
	server <- function(input, output, session) {
		options(shiny.maxRequestSize = 1000 * 1024 ^ 2)

		volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())

		shinyFileChoose(input, "file", roots = volumes, session = session,
										filetypes="sqlite")

		observeEvent(input$file, {
			tab <- parseFilePaths(volumes, input$file)
			fsq <- as.character(tab[1,4])
			# output$tab <- renderTable({tab})
			output$fsq <- renderText({paste("Selected sqlite data base:",fsq)})
		})

		re <- eventReactive(input$station, ({
			req(input$file)

			tab <- parseFilePaths(volumes, input$file)
			fsq <- as.character(tab[1,4])

			st <- input$Station_id
			if (st == "")
				st <- NA
			waiter <- waiter::Waiter$new()
			waiter$show()
			on.exit(waiter$hide())

			a <-
				htsr::d_inventory(
					fsq = fsq,
					sta_sen = st,
					form.out = NA
				)
			if (is.na(st)) {
				rep <- a[[1]]
			} else {
				rep <- a[[2]]
				if (ncol(rep) > 2) {
					rep$Date_start <- as.character(rep$Date_start)
					rep$Date_end <- as.character(rep$Date_end)
				}
			}
			return(rep)
		}))

		output$station <- renderTable({
			re()
		})
		observeEvent(input$close, stopApp())
	}

	# Run the application
	shinyApp(ui = ui, server = server)
}
