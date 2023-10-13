#' @title Compute the potential evapotranspiration with several methods
#'
#' @author P. Chevallier - April 2020-Nov2022
#'
#' @description ETP calculation
#'
#' @details f_temp and f_relh are mandatory in all cases.
#' @details For the Turc method, f_radg is needed.
#' @details For the Penman-Monteith method, f_atmp, f_wvel, h and z are needed. If
#' f_radn is not avalaible, lat, f_tmin and ftmax are also needed.
#' @details The Turc method only works with a monthly frequence.
#'
#' @source  Hingray, B., Picouet, C., Musy A.,
#' Hydrologie, une science pour l'ingénieur, Presses Plolytechniques et Universitaires
#' Romandes, 2008,
#' @source  Allen, R.G., L.S. Pereira, D. Raes, and M. Smith. 1998.
#' Crop Evapotranspiration. Guidelines for Computing Crop Water Requirements.
#' FAO Irrigation and Drainage Paper 56. 300p
#' @source  Er-Raki, S., A. Chehbouni,
#' S. Khabba, V. Simonneaux, L. Jarlan, A. Ouldbba, J. C. Rodriguez,
#' and R. Allen. 2010. “Assessment of Reference Evapotranspiration Methods in
#' Semi-Arid Regions: Can Weather Forecast Data Be Used as Alternate of Ground
#' Meteorological Parameters?” Journal of Arid Environments 74 (12): 1587–96.
#' https://doi.org/10.1016/j.jaridenv.2010.07.002.

#'
#' @param method Method "Turc", "Penman-Monteith", "Priestley-Taylor", "Makkink", "Heargraves-Samani"
#' @param freq Frequency "day", "month"
#' @param f_temp File of air temperature in degC, mandatory
#' @param f_tmin File of air min temperature in degC
#' @param f_tmax File of air max temperature in degC
#' @param f_relh File of relative humidity in percent, mandatory
#' @param f_radg File of global radiation in W/m2
#' @param f_radn File of net radiation in W/m2
#' @param f_atmp File of atmospheric pressure in hPa
#' @param f_wvel File of wind velocities in m/s
#' @param lat Latitude in deg
#' @param alt Altitude in m
#' @param albedo Albedo
#' @param z Anemometer high in m
#'
#' @return An hts files resulting of the operation with a name composed as:
#' @return <J or M><EtpTu>_<Station_id>.hts for the Turc method,
#' @return <J or M><EtpPM>_<Station_id>.hts for the Penman-Monteith method,
#' @return <J or M><EtpPT>_<Station_id>.hts for the Priestley-Taylor method
#' @return <J or M><EtpMa>_<Station_id>.hts for the Makkink method
#' @return <J or M><EtpHS>_<Station_id>.hts for the Heargraves-Samani method

w_etp <- function (
  method = c("Turc", "Penman-Monteith", "Priestley-Taylor", "Makkink", "Heargraves-Samani"),
  freq = c("day", "month"), f_temp, f_relh = NA, f_radg = NA, f_radn = NA, f_atmp = NA,
  f_wvel = NA, f_tmin = NA, f_tmax = NA, lat = NA, alt = NA, albedo = NA, z = NA) {

	# function h_month

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
	# end function h_month



  #initialisation generale
  atmp <- delta <- ea <- es <- fracjour <- ra <- radn <- relh <- rs <- NULL
  temp <- tmax <- tmin <- wvel <- NULL
  if(freq == "day" && method %in% c("Turc")) {
    cat("The", method, "method is not convenient for a daily computation.\n")
    cat("The frequence is changed as monthly!")
    freq = "month"
  }
  if(!file.exists(f_temp) || is.na(f_temp)) stop("\nThe file f_temp is mandatory.\n")
  dn <- dirname(f_temp)

  # construction matrice de base

  #temp
  load(f_temp)
  Station_id <- as.character(tstab$Station[1])
  x <- tibble(Date=tstab$Date, temp=tstab$Value)

  # Calcul de la fraction de jour dans l'annee
  u_fracjour <- function(date){
    an <-as.character(lubridate::year(date))
    valentin <- paste0(an,"-12-31")
    dval <- as.numeric(strftime(as.Date(valentin), format = "%j"))
    jour <- lubridate::date(date)
    doy <- as.numeric(strftime(jour, format = "%j"))
    fracjour <- doy/dval
    return(fracjour)
  }
  x <- mutate(x, fracjour =u_fracjour(Date))

  #relh
  if (method %in% c("Turc", "Penman-Monteith", "Priestley-Taylor", "Makkink")){
    load(f_relh) ; y <- tibble(Date=tstab$Date, relh=tstab$Value)
    x <- left_join(x,y)
  }

  #radg
  if (method %in% c("Turc")){
    load(f_radg) ; y <- tibble(Date=tstab$Date, radg=tstab$Value)
    x <- left_join(x,y)
  }

  #ra
  if (method %in% c("Penman-Monteith", "Priestley-Taylor", "Makkink",
    "Heargraves-Samani")){

    #function u_ra
    u_ra <- function(fracjour, lat) {
      lat <- pi / 180 * lat
      dr <- 1 + (0.033 * cos(2 * pi * fracjour))
      pd <- 0.409 * sin((2 * pi * fracjour) -1.39)
      oms <- acos(-tan(lat)*tan(pd))
      ra <- 24 * 60 * 0.0820 / pi * dr * ((oms * sin(lat) * sin (pd)) + (cos(lat) * cos(pd) * sin (oms)))
      return (ra)
    }
    x <- mutate (x, ra = u_ra(x$fracjour, lat))
  }

  #atmp,gamma, delta, es, ea
  if (method %in% c("Penman-Monteith", "Priestley-Taylor", "Makkink")){
    load(f_atmp) ; y <- tibble(Date=tstab$Date, atmp=tstab$Value/10)
    x <- left_join(x,y)
    x <- mutate (x,
      delta = 4098*(0.6108* exp((17.27*temp)/(temp+237.3)))/((temp+237.3)^2),
      gamma = 0.665*atmp*10^-3,
      es = 0.6108*exp((17.27*temp)/(temp+237.3)),
      ea = 0.6108*exp((17.27*temp)/(temp+237.3)) * relh / 100)
  }

  #wvel
  if (method %in% c("Penman-Monteith")){
   load(f_wvel) ; y <- tibble(Date=tstab$Date, wvel=tstab$Value)
    x <- left_join(x,y)
    x$wvel <- x$wvel * 4.87 / log (67.8 * z - 5.42)
  }

  #tmin, tmax
  if (method == "Heargraves-Samani" ||
    (method %in% c("Penman-Monteith", "Priestley-Taylor") && is.na(f_radn))){
    if(freq == "month") {
      res <- h_month(f_tmin, op="M", rmna = TRUE)
      f_tmin <- as.character(res[[1]])
      res <- h_month(f_tmax, op="M", rmna = TRUE)
      f_tmax <- as.character(res[[1]])
    }
    load(f_tmin) ; y <- tibble(Date=tstab$Date, tmin=tstab$Value)
    x <- left_join(x,y)
    load(f_tmax) ; y <- tibble(Date=tstab$Date, tmax=tstab$Value)
    x <- left_join(x,y)
  }


  #rs
  if (method %in% c("Penman-Monteith", "Makkink")){
    x <- mutate (x, rs = (0.75 + (2 * 10^-5 * alt)) * ra)
  }

  #radn
  if (method %in% c("Penman-Monteith", "Priestley-Taylor")){

    # si f_radn existe
    if(!is.na(f_radn)){
      load(f_radn) ; y <- tibble(Date=tstab$Date, radn=tstab$Value)
      x <- left_join(x,y)
    } else {

    # sinon function u_rn
    u_radn <- function(ra, alt, albedo, tmin, tmax, ea) {
      rs <- (0.75 + (2 * 10^-5 * alt)) * ra
      rns <- (1 - albedo) * rs
      rnl <- 2.4515 * 10^-9 * ((tmax+273)^4 + (tmin+273)^4) * (0.34 - 0.14 * ea)
      radn <- rns - rnl
      return (radn)
    }
    x <- mutate (x, radn = u_radn(ra, alt, albedo, tmin, tmax, ea))
    }
  }

  # calcul Turc
  if(method == "Turc"){
    Value <- NA; length(Value) <- nrow(x)
    t1 <- x$temp
    for (i in 1:nrow(x)){
      if(!is.na(t1[i]) && !is.na(x$radg[i]) &&!is.na(x$relh[i])){
        if(t1[i]<0) Value [i]<- 0
        else {
          if(!is.na(t1[i])) {
            if(x$relh[i] > 50)
              Value [i]<- 0.13*((x$radg[i]*2.065)+50)*t1[i]/(t1[i]+15)
            else
              Value[i] <- 0.13*((x$radg[i]*2.065)+50)*t1[i]/(t1[i]+15)*(1+(50-x$relh[i])/70)
          }
        }
      }
    }
    etp <- transmute(x, Date, Value)
  }

  # calcul Penman-Monteith
  if(method == "Penman-Monteith"){
    etp <- transmute(x, Date, Value = ((0.408 * delta * radn) +
        (gamma * (900 / (temp + 273)) * wvel * (es- ea))) /
        (delta + (gamma * (1 + (0.34 * wvel)))))
  }

  # calcul Priestley-Taylor
  if(method == "Priestley-Taylor"){
    etp <- transmute(x, Date,
      Value = 0.408 * 1.26 * delta * radn / (delta + gamma))
  }

  # calcul Makkink
  if(method == "Makkink"){
    etp <- transmute(x, Date,
      Value = (0.408 * 0.61 * delta * rs / (delta + gamma)) - 0.12)
  }

    # calcul Heargraves-Samani
  if(method == "Heargraves-Samani"){
    etp <- transmute(x, Date,
      Value = 0.408 * 0.0023 * (temp + 17.8)* (tmax-tmin)^0.5 * ra)
  }

  # resultat
  if(freq=="day") pref <- "J" else pref <- "M"
  if(method == "Turc") sen <- paste0(pref,"EtpTu")
  if(method == "Penman-Monteith") sen <- paste0(pref,"EtpPM")
  if(method == "Priestley-Taylor") sen <- paste0(pref,"EtpPT")
  if(method == "Makkink") sen <- paste0(pref,"EtpMa")
  if(method == "Heargraves-Samani") sen <- paste0(pref,"EtpHS")

  tstab <- mutate(etp, Station = Station_id, Sensor = sen)
  fileo <- paste0(dn,"/",sen,"_",Station_id,".hts")
  save(file = fileo, tstab)
  return(cat("File written",fileo))
}


