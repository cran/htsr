#' @title Shiny app: plot hts files
#'
#' @author P. Chevallier - May 2020 - Sep 2023
#'
#' @description Shiny application of the \code{\link{p_line}} and \code{\link{p_bar}}
#' functions, associated with \code{\link{z_set}}
#'
#' @details When launched, a shiny window is open. Follow the instructions and steps.
#'
#'
#'
ps_plothts <- function(){

	requireNamespace("shiny", quietly = TRUE)
	requireNamespace("shinyFiles", quietly = TRUE)
	requireNamespace("tibble", quietly = TRUE)
	requireNamespace("dplyr", quietly = TRUE)
	requireNamespace("lubridate", quietly = TRUE)
	requireNamespace("editData", quietly = TRUE)
	requireNamespace("ggplot2", quietly = TRUE)

	selectfilesUI <- function(id) {
		tagList(
			shinyFilesButton(NS(id,"file"), "File select", "Please select a file", multiple = TRUE,
											 viewtype = "detail", class="btn btn-primary"),
			tags$p(),
			verbatimTextOutput(NS(id,"filepaths"))
		)
	}

	selectfilesServer <- function(id) {
		moduleServer(id, function(input, output, session) {
			volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
			shinyFileChoose(input, "file", roots = volumes, session = session,
											filetypes="hts")

			## print to browser
			output$filepaths <- renderPrint({
				if (is.integer(input$file)) {
					cat("No files have been selected")
				} else {
					parseFilePaths(volumes, input$file)
					tab <- parseFilePaths(volumes, input$file)

					files <- tab$datapath
					nf <- length(files)
					plot.label <- vector(mode="character", length = nf)
					for (i in 1:nf) {plot.label[i] <- paste ("label ", i)}

					fil <- tibble(file.names=files, plot.label, line.type = 1, line.width = 0.2,
												point.shape = 20, point.size = 2)

					save(fil, file=system.file("extdata/fil.RData",package="htsr"))
					cat(nf," files selected")
				}
			})
		})
	}

ui <- fluidPage(

	titlePanel("Files ploting"),

	selectfilesUI("sf"),

	fluidRow(
		verbatimTextOutput("text1"),

		actionButton("setfil", "Enter file settings", class = "btn-warning"),

		editData::editableDTUI("table2"),
		br(),
		actionButton("savefil", "Save file settings", class = "btn-warning"),
		hr()
	),

	fluidRow(
		splitLayout(
			textInput("title", "Title", "Title"),
			textInput("yaxis", "y-Axis label", "y-Axis label"),
		)
	),
	fluidRow(
		column(4,
					 checkboxInput("fixy","Set y-Axis scale", FALSE),
					 conditionalPanel(
					 	condition = "input.fixy",
					 	numericInput("ymin", "Min Y value",NA),
					 	numericInput("ymax", "Max Y value",NA)
					 )
		),
		column(4,
					 checkboxInput("fixtime","Set time interval", FALSE),
					 conditionalPanel(
					 	condition = "input.fixtime",
					 	dateRangeInput("daterange", "Time interval",
					 								 start = as_date("2000-01-01"), end=as_date(now()))
					 )
		),
		column(4,
					 selectInput("pal", "Color palette", "ggplot2", choices = c(
					 	"R3", "R4", "ggplot2", "Okabe-Ito", "Accent", "Dark 2",
					 	"Paired", "Pastel 1", "Pastel 2", "Set 1", "Set 2", "Set 3",
					 	"Tableau 10", "Classic Tableau", "Polychrome 36", "Alphabet"))
		)
	),
	fluidRow(
		column(2,
					 checkboxInput("plot.point", "Plot points", FALSE)
		),
		column(2,
					 checkboxInput("normval","Normalized values", FALSE)
		),
		column(2,
					 checkboxInput("trend", "Linear trend", FALSE)
		),
		column(2,
					 checkboxInput("facet","Facet plot", FALSE)
		),
	),
	fluidRow(
		br(),
		actionButton("saveconf", "Save plot settings", class = "btn-warning"),
		hr(),
		splitLayout(
			actionButton("plot", "Plot", class="btn btn-success"),
			radioButtons("linbar", "Plot type", choices = c("line", "bar"), inline = TRUE),
			checkboxInput("saveplot", "Save plot")
		),
		conditionalPanel(
			condition = "input.plot",
			plotOutput("plotresult", height = "600px"),
			hr(),
		),
		conditionalPanel(
			condition = "input.saveplot",
			column(5,
						 textInput("plotname","name + ext (pdf,png,jpg)", "resulting_plot.png")
			),
			column(2,
						 numericInput("plotwidth", "Width (cm)", 8)
			),
			column(2,
						 numericInput("plotheight", "Height (cm)", 6)
			),
			column(3,
						 actionButton("confirmsave", "Confirm Save", class="btn btn-success")
			),
		),
	),
	actionButton("close", "Done", class="btn btn-danger")
)

server <- function(input, output, session) {

		options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
		options(warn=-1)
		conf<-fil<-palette<-NULL

		selectfilesServer("sf")

		observeEvent(input$setfil, {

			load(file=system.file("extdata/fil.RData",package="htsr"))

			myfil <- fil
			df=callModule(editData::editableDT,"table2",data=reactive(myfil))

			observeEvent(input$savefil,{
				req(input$setfil)
				fil <- df()
				save(fil, file=system.file("extdata/fil.RData",package="htsr"))
			})

		})

		observeEvent(input$saveconf, {
			req(input$setfil)
			conf <- reactive(c(input$title,  input$yaxis,
												 input$normval, input$fixtime,
												 input$daterange[1], input$daterange[2],
												 input$fixy, input$ymin, input$ymax,
												 input$trend, input$facet, input$plot.point))
			conf <- conf()
			save(conf, file=system.file("extdata/conf.RData",package="htsr"))
		})


		observeEvent(input$plot, {
			req(input$setfil, input$saveconf, input$savefil)

			load(file=system.file("extdata/fil.RData",package="htsr"))
			load(file=system.file("extdata/conf.RData",package="htsr"))
			palette <- input$pal
			tz <- "UTC"
			save(file=system.file("extdata/settings.RData",package="htsr"), fil, conf, palette, tz)
			output$plotresult <- renderPlot({
				load(file=system.file("extdata/settings.RData",package="htsr"))
				if(input$linbar == "line"){
					p_line()
				} else {
					p_bar()
				}
			})
		})

		observeEvent(input$confirmsave,{
			req(input$plot)
			load(file=system.file("extdata/fil.RData",package="htsr"))
			wd <- dirname(fil$file.names[1])

			ggsave(filename= paste0(wd,"/",input$plotname), width=as.numeric(input$plotwidth),
						 height=as.numeric(input$plotheight), dpi=300)
		})


		observeEvent(input$close, {stopApp()})
	}



	# Run the application
	shinyApp(ui = ui, server = server)
}
