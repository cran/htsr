#' @title Shiny app: create, modify or remove a sensor from a data base
#'
#' @author P. Chevallier - Nov 2020 - Sep 2023
#'
#' @description Shiny application of the \code{\link{d_sensor}} function
#'
#' @return a shiny session
#

ds_sensor <- function(){

	requireNamespace("shiny", quietly = TRUE)
	requireNamespace("shinyFiles", quietly = TRUE)
	requireNamespace("RSQLite", quietly = TRUE)
	requireNamespace("DBI", quietly = TRUE)
#	requireNamespace(htsr)

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
			shinyFilesButton("file", "File select", "Please select a sqlite data base",
											 multiple = FALSE, viewtype = "detail", class ="btn btn-primary"),
			textOutput("fsq"),
			hr()
		),

		sidebarLayout(
			sidebarPanel(width= 7,
									 div("For accessing to more fields than those displayed, use the detailed function",
									 		"d_sensor ()"), br(),
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

			mainPanel(width= 5,
								splitLayout(
									textInput("nature", "Nature"),
									textOutput("nature1")),
								splitLayout(
									textInput("description", "Description"),
									textOutput("description1")),
								splitLayout(
									textInput("comment", "Comment"),
									textOutput("comment1")),
								textOutput("Message1"),
								textOutput("Message2")
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
				value_fld = c(nature1, description1, comment1)

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

