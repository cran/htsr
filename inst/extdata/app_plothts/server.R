library(shiny)
library(shinyFiles)
library(tibble)
library(dplyr)
library(lubridate)
library(editData)
library(htsr)

shinyServer (function(input, output, session) {
	options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
	options(warn=-1)
	conf<-fil<-palette<-NULL

	volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
	shinyFileChoose(input, "file", roots = volumes, session = session, 
									filetypes="hts")

	output$text1 <- renderPrint({
		cat('Upload files located in the same folder (8 max) and press "File settings".\n')
		cat('Modify the file parameters in the table, then press "Save file settings".\n')
		cat('And/or modify the general plotting parameters, then press "Save plot settings".\n')
		cat('Press "Plo"t at any time after saving file and/or plotting parameters.\n')
		cat('Note that it is possible to reset file entries, by pressing "Begin".')
	})
	
	observeEvent(input$file, {
		tab <- parseFilePaths(volumes, input$file)
		ff <- tab$datapath
		wd <- dirname(ff[1])

		nf <- nrow(tab)
		plot.label <- vector(mode="character", length = nf)
		for (i in 1:nf) {plot.label[i] <- paste ("label ", i)}

		fil <- tibble(file.names=ff, plot.label, line.type = 1, line.width = 0.2,
									point.shape = 20, point.size = 2)

		myfil <- fil
		df=callModule(editData::editableDT,"table2",data=reactive(myfil))

		observeEvent(input$savefil,{
		  req(input$file)
			fil <- df()
			save(fil, file = "fil.RData")
		})
	})
	
	observeEvent(input$saveconf, {
	  req(input$file)
		conf <- reactive(c(input$title,  input$yaxis,
			 input$normval, input$fixtime,
			 input$daterange[1], input$daterange[2],
			 input$fixy, input$ymin, input$ymax,
			 input$trend, input$facet, input$plot.point))
		conf <- conf()
		save(conf, file = "conf.RData")
	})


	observeEvent(input$plot, {
	  req(input$file, input$saveconf, input$savefil)
		load(file ="fil.RData")
		load(file="conf.RData")
		palette <- input$pal
		tz <- "UTC"
		save(file="settings.RData", fil, conf, palette, tz)
		output$plotresult <- renderPlot({
		load(file = "settings.RData")
			if(input$linbar == "line"){
				p_line()
			} else {
				p_bar()
			}
		})
		observeEvent(input$confirmsave,{
		  req(input$plot)
			ggsave(filename=input$plotname, width=input$plotwidth, 
				height=input$plotheight, dpi=300)
		})
	})

	observeEvent(input$close, {stopApp()})
})
