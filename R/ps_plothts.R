#' @title Plot hts files
#'
#' @author P. Chevallier - Apr 2015 - Sep 2023
#'
#' @description This function allows to plot one or several time series files using a shiny web page
#'
#' @details When launched, a shiny window is open. Follow the instructions, divided in 5 steps.
#' \enumerate{
#' \item Select hts files (8 max) pressing "File select". They must be located in the same folder.
#' When done, press "Enter file settings"
#' \item For each file, if needed, use the "Edit" tab to configure label, line.type, line.with,
#' point.shape and point.size. (The values follows the ggplot2 package conventions).
#' When done, press "Save file settings"
#' \item Configure the general layout of the file, entering Title and y-Axis label and
#' choosing a color palette. Several options are available: set y-Axis scale, set time interval,
#' point plot(*), display normalized values, draw a trend line, or display the plot as horizontal facets.
#' When done, press "Save plot settings"
#' \item Pressing "Plot" displays the graph. You can chose a line or bar graph. When the graph is finalized,
#' check the box "save plot". Three formats are allowed: .png, .jpeg or pdf. The resolution is 300 dpi.
#' Then, press "Save plot settings". The plot is saved in the folder of the selected files.
#' \item When finished, press "Done".
#' }
#' Items 2 and 3 can be performed and repeated in any order. Once they have been validated once,
#' item 4 can be executed as often as desired.
#'
#' (*) When point plot is selected, the points overlay the line (point plot doesn't work with bar).
#' If you want only the points on the plot, configure "line.type" and "line.width" = 0.
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

	# function p_line
	p_line <- function(){

		# settings
		fil <- tstab <- Value <- conf <- Legend <- NULL

		if (!file.exists (system.file("extdata/settings.RData",package="htsr")))
			warning("A function creating settings.RData in the data dir must be run before p_line()")

		options(warn=-1)

		load(file=system.file("extdata/settings.RData",package="htsr"))

		nf <- nrow(fil)
		pal <- palette.colors(n=nf, palette = palette)


		# Loop for each track
		for (i in 1:nf) {
			message("\nReading the file ", fil$file.names[i], "\n")
			fff <- fil$file.names[i]
			load(file=fff)
			y <- select(tstab, Date, Value)

			if (conf[4])  {
				y <- filter(y, Date >= as_date(as.numeric(conf[5])))
				y <- filter(y, Date <= as_date(as.numeric(conf[6])))
			}
			if (nrow(y)==0)
				stop (paste("The time-series", fil$plot.label[i],"has no data.\n"))

			# Normalized values
			moy <- mean (y$Value, na.rm=TRUE)
			sigma <- sd (y$Value, na.rm=TRUE)
			if (conf[3]==TRUE) y$Value <- (y$Value -moy)/sigma

			# Building data.frame
			y <- mutate (y, Legend = as.factor(fil$plot.label[i]))
			if (i==1) x <- y else x <- bind_rows (x, y)
		}

		# Plotting
		p <- ggplot (x, aes(x=Date, y= Value, colour = Legend, linewidth = Legend,
												linetype = Legend , size = Legend, shape = Legend)) + geom_line(na.rm = TRUE)
		p <- p + scale_colour_manual(values=pal) + scale_linetype_manual(values = fil$line.type) +
			scale_linewidth_manual(values = fil$line.width)

		if (conf[10]) p = p + stat_smooth(method=lm, se=FALSE)

		if (conf[11]) p = p + facet_grid (Legend ~ ., scales = "free_y") +
			theme(strip.text = element_text(size=rel(2)),
						strip.background = element_rect(colour="black", size =0.5))

		if (conf[12]) p = p +  geom_point(na.rm=TRUE) +
			scale_shape_manual(values = fil$point.shape) +
			scale_size_manual(values = fil$point.size)

		# Ecriture des labels
		p <- p + theme(panel.background=element_rect(fill="white", colour="black", linewidth = 2),
									 panel.grid.major=element_line(colour="black"),
									 panel.grid.minor=element_line(colour=NA))

		p <- p +
			ylab(conf[2]) + xlab("") +
			ggtitle(conf[1])  +
			theme (axis.text.x = element_text(size=20),
						 axis.text.y = element_text(size=20),
						 axis.title.x = element_text(size=20),
						 axis.title.y = element_text(size=20),
						 plot.title=element_text(face="bold", size=20))

		# Redimensionner l'ordonnee
		if(conf[7]==TRUE) p <- p + ylim(as.numeric(conf[8]),as.numeric(conf[9]))

		# Ecriture des legendes
		if (conf[11]) p <- p+ theme(legend.position="none")
		else p <- p+ theme(legend.position="bottom") + theme(legend.text=element_text(size =16))
		p <- p+ theme(legend.title=element_text(size =16, face="bold"))

		options(warn=0)

		return(p)
	}

	# function p_bar
	p_bar <- function(){

		# settings
		fil <- tstab <- Value <- conf <- Legend <- NULL

		if (!file.exists (system.file("extdata/settings.RData",package="htsr")))
			warning("A function creating settings.RData in the data dir must be run before p_bar()")

		load(file=system.file("extdata/settings.RData",package="htsr"))
		options(warn=-1)

		nf <- nrow(fil)
		pal <- palette.colors(n=nf, palette = palette)

		# Loop for each track
		for (i in 1:nf) {
			message("\nReading the file ", fil$file.names[i], "\n")
			fff <- fil$file.names[i]
			load(file=fff)
			y <- select(tstab, Date, Value)

			if (conf[4])  {
				y <- filter(y, Date >= as_date(as.numeric(conf[5])))
				y <- filter(y, Date <= as_date(as.numeric(conf[6])))
			}
			if (nrow(y)==0)
				stop (paste("The time-series", fil$plot.label[i],"has no data.\n"))

			# Normalized values
			moy <- mean (y$Value, na.rm=TRUE)
			sigma <- sd (y$Value, na.rm=TRUE)
			if (conf[3]==TRUE) y$Value <- (y$Value -moy)/sigma

			# Building data.frame
			y <- mutate (y, Legend = as.factor(fil$plot.label[i]))
			if (i==1) x <- y else x <- bind_rows (x, y)
		}

		# Trace du graphe
		p <- ggplot (x, aes(x=Date, y= Value, fill=Legend)) +
			geom_bar(stat = "identity", position = "dodge", na.rm = TRUE)
		p <- p + scale_fill_manual(values=pal)

		if (conf[10]==TRUE)
			p = p + stat_smooth(method=lm, se=FALSE)

		if (conf[11]) p = p + facet_grid (Legend ~ ., scales = "free_y") +
			theme(strip.text = element_text(size=rel(2)),
						strip.background = element_rect(colour="black", size =0.5))

		# Ecriture des labels
		p <- p + theme(panel.background=element_rect(fill="white", colour="black"),
									 panel.grid.major=element_line(colour="black"),
									 panel.grid.minor=element_line(colour=NA))
		p <- p +
			ylab(conf[2]) + xlab("") +
			ggtitle(conf[1])  +
			theme (axis.text.x = element_text(size=20),
						 axis.text.y = element_text(size=20),
						 axis.title.x = element_text(size=20),
						 axis.title.y = element_text(size=20),
						 plot.title=element_text(face="bold", size=20))

		# Redimensionner l'ordonnee
		if(conf[7]==TRUE) p <- p + ylim(as.numeric(conf[8]),as.numeric(conf[9]))

		# Ecriture des legendes
		if (conf[11]) p <- p+ theme(legend.position="none")
		else p <- p+ theme(legend.position="bottom") + theme(legend.text=element_text(size =16))
		p <- p+ theme(legend.title=element_text(size =16, face="bold"))

		options(warn=0)
		return(p)
	}

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
