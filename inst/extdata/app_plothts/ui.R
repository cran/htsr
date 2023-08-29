library(shiny)
library(shinyFiles)
library(tibble)
library(dplyr)
library(lubridate)
library(editData)
library(htsr)

fluidPage(
	
	titlePanel("Files ploting"),
	
	fluidRow(
		# actionButton("submitFILE", "Begin", class="btn btn-primary"),
		verbatimTextOutput("text1"),
		
		shinyFilesButton("file", "File upload", "Please select hts files in the same folder", 
										 multiple = TRUE, viewtype = "detail", class="btn btn-primary"),
		br(),

		# verbatimTextOutput("ff"),
		
		
		# conditionalPanel(
		# 	condition = "input.submitFILE",
		# 	fileInput("filei", "Upload files", accept = ".hts",multiple = FALSE)
		# ),
		# actionButton("settings", " File settings", class="btn btn-info"),
		# br(), br(),
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
				textInput("plotname","Full Path+name+ext (pdf, png, jpg)", NA)
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
