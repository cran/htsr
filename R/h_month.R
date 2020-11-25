#' @title Monthly operations, based on a daily time-series
#'
#' @author P. Chevallier - Oct 2017- Apr 2020
#'
#' @details Based on a daily time-series, the function returns a monthly
#' time-series, and computes a mean monthly climatology. It allows to consider or
#' not the missing daily values: option rmna.
#'
#' The function can also produce Excel files: with a calendar presentation (days
#' in rows, months in columns, years in sheets): option caledit_j ; with the
#' monthly means (or sums): option caledit_m. In addition, the missing values can
#' be replaced by the mean of the existing values for other years : option gapfill.
#'
#' @details Climatology files are by convention attibuted to year 2000.
#'
#' @details Generally, the values of the monthly climatologies are mean values (op="M"),
#' except if they are vomumes (e.g.: precipitation, evaporation,
#' etc.). In these cases, the parameter op="S" must be precised.
#'
#' If rmna = TRUE , the NA values are not taken into consideration for computing
#' the sum or the mean.
#'
#' In the case of discharge values, it is possible to compute monthly volumes
#' expressed in mm. Thatfore, the basin area ba must be given in km2.
#'
#' By default, the reference name of the time-series is <sensor.id>_<station.id>.
#' It is possible to change it giving a value to the parameter ref.
#'
#' @param file Full file name of the daily time-series.
#' @param op Operation: mean ("M") or sum ("S")
#' @param ba Basin area in km2 or NA (default)
#' @param rmna Remove NA values TRUE / FALSE (default)
#' @param climedit Write a climatology file TRUE / FALSE (default)
#' @param caledit_j Write an Excel file with daily calendar TRUE / FALSE (default)
#' @param caledit_m Write an Excel file with monthly calendar TRUE / FALSE (default)
#' @param gapfill Replace the missing months by the "climatology" value TRUE / FALSE
#' (default)
#' @param hts_year Extract the mean, max & min yearly values in hts files
#' TRUE / FALSE (default)
#'
#' @return
#' A list of timeSeries class objects including:
#' [1] raw monthly data;
#' [2] 12 climatology means (January to December);
#' [3] gapfilled monthly data, if the option gapfill is TRUE.
#'
#' @return
#' Three hts time series files: a monthly data file with the suffix _M, a climatology data
#' file with the suffix _C and, optionnally, a gapfilled monthly data file with
#' the suffix _G.
#'
#' Optionnaly, two Excel files with calendar presented values:, one with
#' daily data and one with monthly data, the fist one with a ad_ prefix and the
#' second one with the am_ prefix.
#'
#' @examples \dontrun{
#'
#' res <- h_month("foo.ts",op="S", ba=135, caledit_m = TRUE)
#' }
#'

# fonction ts_mois

h_month <- function (file, op="M", ba=NA, rmna = FALSE, climedit = FALSE,
                    caledit_j=FALSE, caledit_m=FALSE, gapfill = FALSE,
                    hts_year = FALSE){

  # initialisation
  Apr <- Aug <- Dec <- Feb <- Jan <- Jul <- Jun <- Mar <- May <- Nov <- NULL
  Oct <- Sensor <- Sep <- Station <- Value <- av <- jour <- mn <- mois <- NULL
  mx <- gf <- NULL
  Sys.setenv(TZ="UTC")
  if(!(op %in% c("","M","m","S","s")))
    return(warning("\nNot allowed for op.\n"))
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
          if (indic==TRUE) xjk <- xjk * 86.4 /ba
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
      if(j %in% c(1, 3, 5, 7, 8, 10, 12)){
        if(op=="S" || indic==TRUE) xm[j] <- sum(xj) else xm[j] <- mean(xj)
      }
      if(j %in% c(4, 6, 9, 11)){
        if(op=="S" || indic==TRUE) xm[j] <- sum(xj[1:30]) else xm[j] <- mean(xj[1:30])
      }
      if(j == 2 && ni%%4 ==0){
        if(op=="S" || indic==TRUE) xm[j] <- sum(xj[1:29]) else xm[j] <- mean(xj[1:29])
      }
      if(j == 2 && ni%%4 !=0){
        if(op=="S" || indic==TRUE) xm[j] <- sum(xj[1:28]) else xm[j] <- mean(xj[1:28])
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

#FIN
