#' @title Extraction of a time-series from htsr data base
#'
#' @author P. Chevallier - Oct 2017 - Sep 2023
#'
#' @description The function display a web page allowing to extract a time-series in the "hts" format.
#'
#' @details
#'  Complete the requested information in the left panel, then press the submit button in
#' order to extract the file. If you want to display the plot of the extracted file,
#' choose "line" or "bar" and press the plot button.
#'
#'  When the subfunction "d_exp_hts(fsq, sta,sen,rtime=FALSE,dstart=NA,dend=NA, rplot=FALSE)"
#'  is used solely it returns a tibble tstab with 4 columns Date, Value, Station, Sensor.
#'  In this last subfunction fsq is the sqlite data base; sta, the station id, sen, the sensor id; rtime, dstart and
#'  dend define a time interval; rplot, the resulted plot.
#'
#' @return The function returns a file (nomfic) with the following name: <sensor.id>_<station.id>.hts
#'


# Define UI ------
ds_exp_hts <- function () {

	requireNamespace("shiny", quietly = TRUE)
	requireNamespace("shinyFiles", quietly = TRUE)
	requireNamespace("tibble", quietly = TRUE)
	requireNamespace("dplyr", quietly = TRUE)
	requireNamespace("lubridate", quietly = TRUE)
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
				y <- dplyr::filter(y, Date >= as_date(as.numeric(conf[5])))
				y <- dplyr::filter(y, Date <= as_date(as.numeric(conf[6])))
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
				y <- dplyr::filter(y, Date >= as_date(as.numeric(conf[5])))
				y <- dplyr::filter(y, Date <= as_date(as.numeric(conf[6])))
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

	# function z-set
	z_set <- function( file.names= file.names, plot.label="label",
										 line.type = 1, line.width = 0.2,
										 point.shape = 15, point.size = 8,
										 title = "Title", yaxis = "y-Axis", normval = FALSE,
										 fixtime = FALSE, begin = today(), end = today(),
										 fixy = FALSE, ymin = as.numeric(NA), ymax = as.numeric(NA),
										 trend = FALSE, facet=FALSE, point.plot = FALSE,
										 tz = "UTC", palette="ggplot2")
	{

		# fil
		fil <- tibble(file.names = as.character(file.names),
									plot.label = as.character(plot.label),
									line.type = as.integer(line.type), line.width = as.numeric(line.width),
									point.shape = as.integer(point.shape),
									point.size = as.numeric(point.size))

		nf <- nrow(fil)

		if (nf > 8) {warning(" The upload files are limited to 8.")}
		for (i in 1:nf) {
			# fil$plot.label[i] <- paste("label", i)
			if(tools::file_ext(fil$file.names[i]) != "hts")
			{warning(" Not allowed file type")}
			if (fil$line.type[i] <0 || fil$line.type[i] > 6)
			{warning(" line.type must be in the interval 0-6.")}
			if (fil$point.shape[i] <0 || fil$point.shape[i] > 24)
			{warning(" point.shape must be in the interval 0-24.")}
		}

		#	conf
		conf <- c(as.character(title),  as.character(yaxis),
							as.logical(normval), as.logical(fixtime),
							as_date(begin), as_date(end),
							as.logical(fixy), as.numeric(ymin), as.numeric(ymax),
							as.logical(trend), as.logical(facet),
							as.logical(point.plot))

		if (begin > end) {warning(" The beginning is later than the end.")}
		if (fixy == TRUE && ymin > ymax) {warning(" The min is higher than the max.")}

		# tz & palette
		if(tz %in% OlsonNames() == FALSE) {warning(" Unknown timezone.")}
		if(palette %in% palette.pals() == FALSE) {warning(" Unknown palette.")}

		save (file=system.file("extdata/settings.RData",package="htsr"), tz, palette, conf, fil)

		return(cat ("Setting file written"))
	}
	#FIN


	ui <- fluidPage(
		theme = NULL,
		titlePanel("hts file import"),

		fluidRow(sidebarLayout(
			sidebarPanel(
				width = 5,
				shinyFilesButton("file", "Select database", "Please select a sqlite data base",
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

			d_exp_hts <- function(fsq, sta,sen,rtime=FALSE,dstart=NA,dend=NA, rplot=FALSE){

				# fonction u_statnom
				u_statnom <- function(fsq,sta){
					conn <- dbConnect(SQLite(),fsq)
					sta <- paste("'",sta,"'",sep="")
					selection <- paste ("SELECT * FROM ST WHERE Id_station =",sta)
					xt <- dbGetQuery(conn, selection)
					nom <- xt$Nom[1]
					dbDisconnect(conn)
					return(nom)
				}

				# fonction u_stacapt
				u_stacapt <- function(fsq,table,sta,sen){
					Valeur <- NULL
					conn <- dbConnect(SQLite(),fsq)
					table1 <- paste("'",table,"'",sep="")
					sta1 <- paste("'",sta,"'",sep="")
					sen1 <- paste("'",sen,"'",sep="")
					selection <- paste ("SELECT * FROM", table1, " WHERE Id_Station =",sta1,
															" AND Capteur =",sen1)
					x <- dbGetQuery(conn, selection)
					xt <- tibble::as_tibble(x)
					dbDisconnect(conn)
					yt <- dplyr::select(xt,Date,Valeur)
					yt <- dplyr::arrange(yt,Date)
					return(yt)
				}

				# corps de fonction
				# suppressWarnings()

				# initialisation
				Sys.setenv(TZ='UTC')
				conn <- dbConnect(SQLite(),fsq)
				sta1 <- paste0("'",sta,"'")
				sen1 <- paste0("'",sen,"'")
				sel <- paste ("SELECT * FROM SS WHERE Id_Station =",sta1,
											" AND Capteur =",sen1)
				t <- dbGetQuery(conn,sel)
				table <- as.character(t$Tabl)
				dbDisconnect(conn)
				#  if(table=="PR") op <-"S" else op <- "Mo"

				# appel u_stacapt
				z <- u_stacapt(fsq, table, sta, sen)
				colnames(z) <- c("Date", "Value")
				z$Date <- as_datetime(z$Date)

				# preparation pour rafinage
				dstart <- as_datetime(dstart)
				dend <- as_datetime(dend)
				date_start <- as_datetime(min(z$Date))
				date_end <- as_datetime(max(z$Date))

				if(rtime) {
					if(is.na(dstart)) dstart <-date_start
					if(is.na(dend)) dend <- date_end
					z <- dplyr::filter(z, Date > dstart)
					z <- dplyr::filter(z, Date <= dend)
				}

				nomfic <- paste (dirname(fsq),"/",sen,"_",sta,".hts",sep="")
				tstab <- mutate(z, Station = as.factor(sta), Sensor = as.factor(sen))
				save(tstab, file=nomfic)

				# plot graphe
				if(rplot){
					z_set(file.names = nomfic, plot.label = sen, title = sta)
					if (table=="PR") p <- p_bar() else p <- p_line()
					show(p)
				}

				# sortie
				write(file="",paste("File",nomfic,"extracted !"))
				return (tstab)
			}


			if (compute) {
				tstab <-d_exp_hts(fsq = fsq, sta, sen, rtime = input$Set_time,
					dstart=as.character((input$dates[1])), dend=as.character((input$dates[2])),
					rplot = FALSE)
				save (file=fileo, tstab)
				rep <- paste0("File written: ", fileo)
			}

			po <- eventReactive (input$plot, {
				z_set(file.names= fileo, plot.label = sen,title = sta)
				if (input$plot_opt=="bar") p_bar()
				else p_line()
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

