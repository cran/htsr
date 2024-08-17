#' @title Plot wind roses
#'
#' @author P. Chevallier - Dec 2019 - Sep 2023
#'
#' @description Plot wind roses
#'
#' @details
#' For a detailed description of all parameters see \code{\link[openair]{windRose}}
#'
#' @param fsq Full name of the htsr data base
#' @param sta Station id
#' @param swd Id of wind direction sensor
#' @param swv Id of wind speed sensor
#' @param ws.int Size of speed intervals
#' @param angle Value in percent of the range unit
#' @param grid.line Value in percent of the grid line frequency
#' @param type Type of plot: "default", "year" or "month"
#' @param breaks Number of speed intervals
#' @param offset Size in percent of the central hole
#' @param paddle Shape of the basic elements: if FALSE, polar,
#' if TRUE, rectangular
#' @param key.position Position of the legend
#'
#' @seealso
#' \code{\link[openair]{windRose}}
#'
#' @return
#' A wind rose plot

#function p_wind
p_wind <- function(fsq, sta, swd, swv, ws.int=0.5, angle=45, grid.line=10,
  type="default", breaks=5, offset=5, paddle=FALSE,
  key.position = "right"){

	requireNamespace("openair", quietly = TRUE)

	d_wind <- function(fsq, sta, swd, swv){


		requireNamespace("openair", quietly = TRUE)


		# function d_exp_hts

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
				# z_set(file.names = nomfic, plot.label = sen, title = sta)
				# if (table=="PR") p <- p_bar() else p <- p_line()
				# show(p)
			}

			# sortie
			write(file="",paste("File",nomfic,"extracted !"))
			return (tstab)
		}



		#extraction
		tstab <- d_exp_hts (fsq, sta = sta, sen = swd)
		fwd <- (paste0(swd,"_",sta,".hts"))
		save(file=fwd,tstab)
		rm(tstab)
		tstab <- d_exp_hts (fsq, sta = sta, sen = swv)
		fwv <- (paste0(swv,"_",sta,".hts"))
		save(file=fwv,tstab)
		rm(tstab)

		#tibble de travail
		fcom <- h_common(c(fwd, fwv))
		load(fcom[1])
		data_wind <- select(tstab, date = Date)
		data_wind <- openair::cutData(data_wind, type = "month")
		data_wind <- openair::cutData(data_wind, type = "year")
		data_wind <- mutate(data_wind, wind_dir=tstab$Value)
		load(fcom[2])
		data_wind <- mutate(data_wind, wind_spd=tstab$Value)

		#suppression des fichiers de calcul
		file.remove(fwd)
		file.remove(fwv)
		file.remove(paste0("co_",fwd))
		file.remove(paste0("co_",fwv))

		# save (data_wind, file="data_wind.RData")
		#
		# return (message("data_wind table created in the data_wind.RData file"))
		return (data_wind)
	}

	data_wind <- d_wind(fsq, sta, swd, swv)

  coln <- colnames(data_wind)
  ws <- coln[5]
  wd <- coln[4]

  # wind rose using openair
  openair::windRose(mydata=data_wind, ws = ws, wd = wd, ws2 = NA, wd2 = NA,
    ws.int = ws.int, angle = angle, type = type, bias.corr = TRUE,
    cols = "default", grid.line = grid.line, width = 1, seg = NULL, auto.text = TRUE,
    breaks = breaks, offset = offset, normalise = FALSE, max.freq = NULL,
    paddle = paddle, key.header = NULL, key.footer = "(m/s)",
    key.position = key.position, key = TRUE, dig.lab = 5, statistic =
                      "prop.count", pollutant = NULL, annotate = FALSE, angle.scale =
                      315, border = NA)
  return()
}

