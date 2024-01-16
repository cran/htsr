#' @title hts time series with fixed timestep
#'
#' @author P. Chevallier - Oct 2017 - Sep 2023
#'
#' @description Computes time-series with a fixed timestep from infra-daily to monthly within a shiny web page.
#'
#' @details
#' First of all, one must select a "starting" hts file, instantaneous or already with
#' a fixed timestep.
#'
#' Then one must choose the computing time-step and mode, between the possible choices. Note that
#' the timezone considered is the timezone of the "starting" file.
#'
#' Possible time-steps are: 5, 10 or 30 minutes, 1, 2, 3, 6 or 12 hours, 1 day, 1 month.
#' It shall be noted that when computing the monthly time step, the daily time step is previously computed.
#'
#' Possible modes are: average, sum, max or min. For monthly time step, max and min offers two options:
#' daily max averages, respectively min, or absolute, respectively min.
#'
#' In the case of a daily timestep, a shift value (in hours) allows to shift the time interval.
#' For example if shift = 6, the date is computed from 6am until 6am the following day. The result is
#' dated in the middle of the interval, i.e. if shift = 6; the datetime is 18.
#'
#' In the case of a monthly timestep, associated additional time series can be optionally
#' computed:
#'
#' \itemize{
#'   \item A mean monthly climatology, taking into account or not the missing daily values with the
#'   option "remove NA". Climatology files are by convention awarded to year 2000.
#'   \item Excel files: with a calendar presentation (days in rows, months in columns,
#'    years in sheets): option caledit_j ; with the monthly means (or sums): option caledit_m.
#'   \item Missing values can be replaced by the mean of the existing values for
#'   other years: option gapfill.
#'   \item Extract year stat
#' }
#'
#' The output files are written in same folder as the starting hts file.
#'
#' @return hts files at the requested timestep with a suffix giving the timestep in minutes, i.e.
#' 1440 for the daily timestep. In the case or monthly timestep, the suffixes are: M for the
#' current case, C for the climatology, G for the gapfilled file.
#'
#' Optionally, two Excel files with values in "calendar form": one with
#' daily data and one with monthly data, the fist one with a ad_ prefix and the
#' second one with the am_ prefix.



hs_tstep <- function (){
	requireNamespace("shiny", quietly = TRUE)
	requireNamespace("shinyFiles", quietly = TRUE)
	requireNamespace("tibble", quietly = TRUE)
	requireNamespace("dplyr", quietly = TRUE)
	requireNamespace("lubridate", quietly = TRUE)
	requireNamespace("waiter", quietly = TRUE)

	tstep <- c("monthly", "daily", "12h", "6h", "3h", "2h", "hourly", "30mn",
						 "10mn", "5mn")
	lmode <- c("average", "max", "min", "sum")
	lmodem <- c("average", "max-av", "max-max", "min-av", "min-min", "sum")

	# function h_month
	# --------------------------------------------------------------------------

	h_month <- function (file, op="M", ba=NA, rmna = FALSE, climedit = FALSE,
											 caledit_j=FALSE, caledit_m=FALSE, gapfill = FALSE,
											 hts_year = FALSE){

		# initialisation
		Apr <- Aug <- Dec <- Feb <- Jan <- Jul <- Jun <- Mar <- May <- Nov <- NULL
		Oct <- Sensor <- Sep <- Station <- Value <- av <- jour <- mn <- mois <- NULL
		mx <- gf <- NULL
		Sys.setenv(TZ="UTC")
		if(!(op %in% c("","M","m","S","s","Mx","Mn")))
			return(warning("\nNot allowed for op.\n"))
		if(op == "m") op <- "M"
		if(op == "s") op <- "S"
		dn <- dirname(file)
		bn <- basename(file)
		nfse <- tools::file_path_sans_ext(file)
		fse <- tools::file_path_sans_ext(bn)

		if(gapfill == FALSE && hts_year == TRUE)
			return(warning("\nFor computing hts_year files, gapfill must be TRUE.\n"))

		tss_mens <- NA
		tss_clim <- NA
		tss_gapf <- NA
		indic <- FALSE
		if(!is.na(ba)) indic <- TRUE

		# lecture et mise en forme
		load(file)
		message("Building the general table.\n")
		y <- dplyr::select(tstab, Date, Value)
		stat <- tstab$Station[1]
		capt <- tstab$Sensor[1]
		y <- dplyr::arrange(y, by = Date)
		nb <- nrow(y)
		annee.deb <- lubridate::year(y$Date[1])
		annee.fin <- lubridate::year(y$Date[nb])
		annee.nb <- annee.fin-annee.deb+1
		mois.deb <- lubridate::month(y$Date[1])
		mois.fin <- lubridate::month(y$Date[nb])
		y <- dplyr::mutate(y, annee=lubridate::year(Date), mois =lubridate::month(Date),
											 jour = annee <-lubridate::day(Date))
		d1 <- as.numeric(as.POSIXct(y$Date[2]))
		d2 <- as.numeric(as.POSIXct(y$Date[1]))
		if(d1 - d2 != 86400) return(warning("\nThe file seems not have a daily time step!\n"))

		ny <- annee.fin -annee.deb +1

		# tableaux annuels journaliers
		message("Building the daily table. \n")
		nommois <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

		# boucle sur les annees
		for (i in 1:ny){
			ni <- annee.deb +i -1
			xm <- vector("numeric",12)

			# boucle sur les mois
			for (j in seq_along(xm)){
				xj <- vector("numeric", 31)

				# boucle sur les jours
				for (k in seq_along(xj)){
					xjk <- dplyr::filter(y, annee == ni, mois == j, jour==k)
					if(nrow(xjk) == 0L) xj[[k]] <- NA
					else {
						xjk <- as.numeric(xjk$Value)
						if (indic) xjk <- xjk * 86.4 /ba
						if(!is.na(xjk) || ! is.null(xjk)) xj[[k]] <- xjk #ERROR plus d'éléments fournis que d'éléments à remplacer
						else xj[[k]] <- NA
					}
				} # fin boucle sur les jours


				if(j == 1) {
					xa <- data.frame(xj)
					colnames(xa)[1] <- nommois[1]
				} else {
					xa <- cbind(xa, xj)
					colnames(xa)[j] <- nommois[j]
				}

				if(op %in% c("Mx", "Mn")) {
					if (op == "Mx") xm[j] <- max(xj) else xm[j] <- min(xj)

				} else {

					if(j %in% c(1, 3, 5, 7, 8, 10, 12)){
						if(op=="S" || indic) xm[j] <- sum(xj) else xm[j] <- mean(xj)
					}
					if(j %in% c(4, 6, 9, 11)){
						if(op=="S" || indic) xm[j] <- sum(xj[1:30]) else xm[j] <- mean(xj[1:30])
					}
					if(j == 2 && ni%%4 ==0){
						if(op=="S" || indic) xm[j] <- sum(xj[1:29]) else xm[j] <- mean(xj[1:29])
					}
					if(j == 2 && ni%%4 !=0){
						if(op=="S" || indic) xm[j] <- sum(xj[1:28]) else xm[j] <- mean(xj[1:28])
					}
				}
			} # fin boucle sur les mois

			xa <- rbind(xa,xm)
			xa <- tibble::as_tibble(xa)
			xa <- dplyr::mutate(xa,year=ni)
			xa <- dplyr::select(xa,year,Jan, Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
			if(i == 1) xy <- xa else xy <- rbind(xy, xa)

		} # fin boucle sur les annees

		# tableau de valeurs mensuelles
		xm <- xy[seq(from = 32, to = nrow(xy), by = 32),]
		nbmois <- 12 * nrow(xm)
		zvaleur <- NA ; length(zvaleur) <- nbmois
		zdate <- NA ; length(zdate) <- nbmois
		k <-1
		for (i in 1:annee.nb) {
			zannee <-as.character(annee.deb+i-1)
			for (j in 1:12){
				zvaleur[k] <- as.numeric(xm[i,j + 1])
				zmois <- as.character(j)
				zd <- paste0(zannee,"-",zmois,"-16 00:00:00")
				zdate[k] <- as.character(as.POSIXct(zd), origin="1970-01-01 00:00:00")
				k <- k+1
			}
		}
		tstab <- tibble::tibble(Date = zdate, Value = zvaleur, Station = stat, Sensor =capt)
		tstab$Date <- as.POSIXct(tstab$Date, origin = "1970-01-01", tz = "UTC")
		tstab0 <- tstab
		filem <- paste0(nfse,"_M.hts")
		save(tstab,file=filem)
		message ("Montly hts file written: ",filem,"\n")
		st <- c(lubridate::year(tstab$Date[1]),lubridate::month(tstab$Date[1]))
		tss_mens <- ts(data = tstab$Value, start = st, frequency = 12)

		# ecriture du fichier Excel mensuel
		if (caledit_m==TRUE) {
			fileo <- paste0(dn,"/cm_",fse,".xlsx")
			WriteXLS::WriteXLS(xm,ExcelFileName=fileo,SheetNames="months", row.names = TRUE)
			message ("Monthly Excel file written: ",fileo,"\n")
		}

		# ecriture des fichiers Excel journalier
		if(caledit_j==TRUE){
			fileo <- paste0(dn,"/cd_",fse,".xlsx")
			nn <- vector("character", ny)
			xx <- vector("list", ny)
			for(i in 1:ny){
				ni <- annee.deb +i -1
				nn[[i]] <- as.character(ni)
				xxi <- dplyr::filter(xy, year == ni)
				xxi <- dplyr::select(xxi,Jan, Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
				xxi <- as.data.frame(xxi)
				if(op=="S") rownames(xxi)[[32]] <- "sum" else rownames(xxi)[[32]] <- "mean"
				xx[[i]] <- xxi
			}
			WriteXLS::WriteXLS(xx, ExcelFileName=fileo, SheetNames = nn, row.names=TRUE)
			message ("Daily Excel file written: ",fileo,"\n")
		}

		# climatologies mensuelles
		mens <- NA ; length(mens) <- 12
		for (j in 1:12) {
			xmm <- xm[,j+1] ; colnames(xmm) <- "cc"
			mens[j] <- mean(xmm$cc, na.rm = TRUE)
		}
		tss_clim <- ts(mens, start=1, end=12)

		# ecriture du fichier ts de climatologie
		if (climedit == TRUE){
			zd <- c("2000-01-16", "2000-02-16", "2000-03-16", "2000-04-16", "2000-05-16", "2000-06-16",
							"2000-07-16", "2000-08-16", "2000-09-16", "2000-10-16", "2000-11-16", "2000-12-16")
			zd <- as.POSIXct(zd, origin = "1970-01-01", tz = "UTC")
			tstab <- tibble::tibble(Date =zd, Value = mens, Station = stat, Sensor = capt)
			fileo <- paste0(nfse,"_C.hts")
			save(tstab, file = fileo)
			message ("Climatology hts file written: ",fileo,"\n")
		}

		# ecriture du fichier ts mensuel gapfilled
		if (gapfill == TRUE) {
			zdd <- tstab0
			zdd <- dplyr::mutate(zdd, gf = ifelse(is.na(Value),
																						mens[as.integer(lubridate::month(Date))], Value))
			tstab <- dplyr::select(zdd, Date, gf, Station, Sensor)
			tstab$Date <- as.POSIXct(tstab$Date, origin = "1970-01-01", tz = "UTC")
			colnames(tstab) <- c("Date","Value","Station","Sensor")
			fileo <- paste0(nfse,"_G.hts")
			save(tstab,file=fileo)
			message ("Gapfilled monthly hts file written: ",fileo,"\n")
			tss_gapf <- ts(data = tstab$Value, start = st, frequency = 12)

			# écriture des fichiers ts annuels
			if (hts_year == TRUE) {
				z <- dplyr::mutate(tstab, annee = lubridate::year(Date))
				z1 <- dplyr::group_by(z, annee)
				z2 <- dplyr::summarise(z1, av = mean(Value), mn = min (Value), mx = max (Value))
				annee1 <- lubridate::ymd(paste0(as.character(z2$annee), "-07-01"))
				z2 <- dplyr::mutate(z2, Date = as.POSIXct(annee1,
																									origin = "1970-01-01", tz = "UTC"), Station = stat, Sensor = capt)
				fileo <- paste0(nfse,"_Yav.hts")
				tstab <- dplyr::select(z2, Date, av, Station, Sensor)
				colnames(tstab) <- c("Date","Value","Station","Sensor")
				save(tstab,file=fileo)
				message ("Yearly hts file (monthly means) written: ",fileo,"\n")
				fileo <- paste0(nfse,"_Ymn.hts")
				tstab <- dplyr::select(z2, Date, mn, Station, Sensor)
				colnames(tstab) <- c("Date","Value","Station","Sensor")
				save(tstab,file=fileo)
				message ("Yearly hts file (monthly min) written: ",fileo,"\n")
				fileo <- paste0(nfse,"_Ymx.hts")
				tstab <- dplyr::select(z2, Date, mx, Station, Sensor)
				colnames(tstab) <- c("Date","Value","Station","Sensor")
				save(tstab,file=fileo)
				message ("Yearly hts file (montly max) written: ",fileo,"\n")

			}

		}
		# retour
		if (gapfill == TRUE) return(list(filem, tss_mens, tss_clim, tss_gapf))
		else return(list(filem, tss_mens, tss_clim))
	}
	# ----------------------------------------------------
	# end function h_month

	# function h_time_step
	# ---------------------------------------------------
	h_timestep <- function(file,tst,op="M", shift=0){

		#controle
		Sys.setenv(TZ='UTC')
		cas <- c("M","Mn","Mx","S")
		if(!(op %in% cas[1:4])) stop("Wrong value of op!")
		if (1440%%tst!=0 || tst <10 || tst > 1440)
			stop(warning(tst, "is not a divisor of 1440 mn or <10!"))
		shift <- trunc(shift)
		if (shift < 0 || shift > 23)
			stop(warning("the shift value must be in the interval [0-23]"))

		#initialisation
		load(file)
		y <- tstab
		sta <- y$Station[1]
		capt <- y$Sensor[1]
		nfse <- tools::file_path_sans_ext(file)
		fileo <- paste0(nfse,"_",tst,".hts")
		y <- arrange(y, Date)

		#infrajour
		date.deb <- as.numeric(y$Date[1])
		date.end <- as.numeric(y$Date[nrow(y)])
		if (tst == 1440) td <- as.double(date.deb %/% 86400) * 86400 +
			(shift * 3600) else td <- as.double(date.deb %/% 86400) * 86400
		if (tst == 1440) te <- as.double((date.end %/% 86400) + 1) * 86400 +
			(shift * 3600) else te <- as.double((date.end %/% 86400) + 1) * 86400

		dte <- te/86400
		dtd <- td/86400
		dtst <- (tst*60)/86400

		dtte <- (dte-dtd)
		ni <- dtte / dtst
		message("nb of iterations ",ni)

		yd <- as.double(y$Date)/86400-dtd
		yv <- as.numeric(y$Value)

		if (op == "S") iop <- 1
		if (op == "M") iop <- 0
		if (op == "Mn") iop <- -2
		if (op == "Mx") iop <- 2

		#Boucle cpp pour calcul valeur
		xv <- u_timestep (dtte, yd, yv, dtst, iop)

		#Calcul date
		xd <- vector(mode="integer", length = ni)
		for (i in 1:ni) xd[i] <- (td) + (i-1)* tst * 60
		if (tst == 1440) xd = xd-43200

		#Ecriture
		x <- tibble(Date=as_datetime(xd), Value=xv)
		tstab <- mutate(x, Station = as.factor(sta), Sensor = as.factor(capt))
		save(tstab,file=fileo)
		message("Init ", as.character(as.POSIXct(td,origin="1970-1-1")),
						" End ", as.character(as.POSIXct(te,origin="1970-1-1")))
		message("Timestep ", tst, " minutes")
		if (op=="S") message("Sum values")
		if (op=="M") message("Mean values")
		if (op=="Mn") message("Min values")
		if (op=="Mx") message("Max values")

		# retour
		message("File written with ", nrow(tstab), "rows.
  Can be renamed for a future use.\n")
		return(fileo)
	}
	# -------------------------------------------
	# end funtion h_timestep


	# Define UI
	ui <- fluidPage(

		waiter::use_waiter(),
		titlePanel("Calculation of fixed time-step files"),

		fluidRow(
			shinyFilesButton("file", "File upload", "Please select hts files in the same folder",
											 multiple = FALSE, viewtype = "detail", class="btn btn-primary"),
			textOutput("ff"),
			br()
		),

		fluidRow(
			column(width = 4,
				selectInput("ts", "Time-step", tstep, selected = "daily"),
				conditionalPanel(
					 	condition = "input.ts == 'daily'",
					 	numericInput("shift", "Time shift (hours)",0,0,23,1)
				)
			),
			column(width = 4,
				conditionalPanel(
					condition = "input.ts == 'monthly'",
					selectInput("mode", "Mode", lmodem, selected = "average")
				),
				conditionalPanel(
					condition = "input.ts != 'monthly'",
					selectInput("mode", "Mode", lmode, selected = "average")
				),
			),
			conditionalPanel(
				condition = "input.ts == 'monthly'",
					column(width = 4,
						checkboxInput("caledit_j", "calendar daily Excel file"),
						checkboxInput("caledit_m", "calendar monthly Excel file"),
						checkboxInput("climedit", "climatogy file"),
						checkboxInput("rmna", "remove NA"),
						checkboxInput("gapfill", "gapfilling"),
						checkboxInput("hts_year", "extract year stat")
				)
			)
		),

		fluidRow(
			verbatimTextOutput("MESS2"),
			actionButton("submit", "Submit", class = "btn btn-warning"),
			br(),
			textOutput("mon"),
			textOutput("MESS"),
			textOutput("MESS1"),
			hr(),
			actionButton("close", "Done", class = "btn btn-danger")
		)
	)

	# Define server
	server <- function(input, output, session) {

		options(shiny.maxRequestSize=1000*1024^2)

		volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
		shinyFileChoose(input, "file", roots = volumes, session = session,
										filetypes="hts")

		observeEvent(input$file, {
			tabfile <- parseFilePaths(volumes, input$file)
			ff <- tabfile$datapath[1]

			output$ff <- renderText ({paste("selected file:", ff)})
			output$MESS2 <- renderText({
				"The calculation time depends on the number of records and the time step.
      It can last. Wait for the file writing message to appear. Be patient!"})
		})

		observeEvent(input$submit, {
			tabfile <- parseFilePaths(volumes, input$file)
			ff <- tabfile$datapath[1]

			tst <- 1440
			mn <- FALSE
			if(input$ts == "monthly") mn <- TRUE
			if(input$ts == "hourly") tst <- 60
			if(input$ts == "daily") tst <- 1440
			if(input$ts == "5mn") tst <- 5
			if(input$ts == "10mn") tst <- 10
			if(input$ts == "30mn") tst <- 30
			if(input$ts == "2h") tst <- 120
			if(input$ts == "3h") tst <- 180
			if(input$ts == "6h") tst <- 360
			if(input$ts == "12h") tst <- 720
			if(input$mode == "average") op <- "M"
			if(input$mode == "sum") op <- "S"
			if(input$mode == "min") op <- "Mn"
			if(input$mode == "max") op <- "Mx"
			shift <- as.numeric(input$shift)

			# Mensuel
			if (mn) {
				waiter <- waiter::Waiter$new()
				waiter$show()
				on.exit(waiter$hide())
				tst <- 1440
				if(input$mode == "average") {op <- "M" ; op1 <- "M"}
				if(input$mode == "sum") {op <- "S" ; op1 <- "S"}
				if(input$mode == "max-av") {op <- "Mx" ; op1 <- "M"}
				if(input$mode == "max-max") {op <- "Mx" ; op1 <- "Mx"}
				if(input$mode == "min-av") {op <- "Mn" ; op1 <- "M"}
				if(input$mode == "min-min") {op <- "Mn" ; op1 <- "Mn"}
				f <- h_timestep(file=ff, tst=1440, op = op, shift = 0)
				f1 <- h_month(file = f, op = op1, ba = NA, rmna = input$rmna, climedit = input$climedit,
											caledit_j=input$caledit_j, caledit_m=input$caledit_m,
											gapfill = input$gapfill, hts_year = input$hts_year)
				output$MESS1 <- renderText({paste("File written:", f1[1],
																					" with eventual accompanying files")})
			} else {
				# Journalier et infra-journalier
				waiter <- waiter::Waiter$new()
				waiter$show()
				on.exit(waiter$hide())
				f <- h_timestep(file=ff, tst=tst, op = op, shift = shift)
				output$MESS <- renderText({paste("File written:", f)})
			}
		})

		observeEvent(input$close, stopApp())
	}

	# Run the application
	shinyApp(ui = ui, server = server)
}
