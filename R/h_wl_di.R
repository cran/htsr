#' @title Computation of the discharges from water-levels
#'
#' @author P. Chevallier - Dec 2020
#' @description Computes a discharge time-series from water levels data and calibration curves
#'
#' @param fsq htsr data base
#' @param sta Station Id.
#' @param seni Input sensor Id (water levels)
#' @param seno Output sensor Id (discharges)
#' @param dstart Start date (NA by default)
#' @param dend End date (NA by default)
#' @param dbo Includes the result in the data base (TRUE by default)
#'
#' @details
#' Calibration curves must exist in the data base.
#'
#' If 'dbo' is TRUE, a discharge table "DI" and the sensor 'seno'
#' must exist in the data base. The new discharge time-series overwrites the already existing data ; however, it is
#' asked to confirm the operation. In any case the data base is previously backed up.
#'
#' @seealso The functions \code{\link{d_exp_hts}} and \code{\link{d_imp_hts}}are used for export the water levels,
#' respectively import the discharges within the data base. The function \code{\link{d_exp_discalib}} is used for
#' loading the calibration curves.
#'
#' @return Writes an hts file with the resulting discharges and optionally includes it in the data base.
#'


h_wl_di <- function(fsq, sta, seni, seno, dstart=NA, dend=NA, dbo = TRUE){

  # fonction hauteur-debit
  hq <- function (h, Hc, Qc) {
    q <- NA
    for (i in 1:(length(Hc) - 1)) {
      if (!is.na(h) && h >= Hc[i] && h < Hc[i+1])
        q <- Qc[i] + ((Qc[i+1]-Qc[i]) * (h-Hc[i]) / (Hc[i+1]-Hc[i]))
    }
    return(q)
  }


  # nommage fichier sortie
  fileo <- paste0(dirname(fsq),"/",seno,"_",sta,".hts")

  # extraction du fichier des hauteurs de la base de donnees
  if(is.na(dstart)) rtime <- FALSE else rtime <- TRUE
  xh <- d_exp_hts(db.sqlite =fsq, sta =sta, sen = seni, rtime = rtime,
    dstart, dend, rplot = FALSE)

  # extraction des courbes de tarage de la base de donnees
  CD <- NULL
  calib <- d_exp_discalib(db.sqlite = fsq, sta=sta, calib = TRUE, dism = FALSE)[[1]]
  calib <- mutate (calib, CD = as.factor(Date))
  calib$Date <- as_datetime(calib$Date)
  nc <- length(levels(calib$CD))   # nbre d'etalonnages
  dcdeb <- as_datetime(levels(calib$CD)) # date debut de chaque etalonnage

  # boucle sur les etalonnages
  tstab <- x <-  NULL

  # cas etalonnage unique
  if(nc==1) {
    Hc <- calib$H
    Qc <- calib$Q
    xhf1 <- filter(xh, Date < dcdeb[1])
    xhf1$Value <- NA
    xhf2 <- dplyr::filter (xh, Date >= dcdeb[1])
    a <- vector(mode = "double", length = nrow(xhf2))
    for(j in 1: nrow(xhf2)) a[j] <- hq(xhf2$Value[j], Hc, Qc)
    xhf2 <- transmute(xhf2, Date, Value = a, Station = as.factor(sta),
      Sensor = as.factor (seno))
    tstab <- bind_rows(xhf1, xhf2)
  } else {

    # cas plusieurs etalonnages
    for(i in 1:nc) {
      calibi <- dplyr::filter(calib, CD == levels(calib$CD)[i])
      Hc <- calibi$H
      Qc <- calibi$Q
      if (i == 1) {
        x <- filter(xh, Date < dcdeb[1])
        x$Value <- NA
      }
      xhf <- dplyr::filter(xh, (Date >= dcdeb[i]))
      if (i != nc) xhf <- dplyr::filter(xhf, (Date < dcdeb[i+1]))
      a <- vector(mode = "double", length = nrow(xhf))
      for(j in 1: nrow(xhf)) a[j] <- hq(xhf$Value[j], Hc, Qc)
      x <- transmute(xhf, Date, Value = a, Station = as.factor(sta),
        Sensor = as.factor (seno))
      tstab <- bind_rows(tstab, x)
    }
  }

  # écriture fichier et enregistrement dans la base de données
  save(tstab, file = fileo)
  message("File written: ", fileo)
  if (dbo) d_imp_hts(db.sqlite=fsq, file_hts = fileo, table = "DI", bku = TRUE)
}
