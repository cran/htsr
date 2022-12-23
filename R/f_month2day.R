#' @title Interpolation of daily records from a monthly time series
#'
#' @author P. Chevallier - dec 2022
#'
#' @description Interpolation of daily records from a monthly time-series
#'
#' @details
#' The function build and interpolated daily time-series from a monthly one. The 
#' daily values are linearly computed between two consecutive monthly values.
#'
#' @param file monthly time series to process
#'
#' @return a daily time series
#'


f_month2day <- function(file) {
  
  #initialisation
  Sys.setenv(TZ="UTC")
  nfse <- tools::file_path_sans_ext(file)
  
  #lecture
  load(file)
  xm <- tstab
  start <- min(xm$Date)
  end <- max(xm$Date)
  nbd <- as.numeric(end-start)
  jour <- valjour <- vector(mode="numeric", length=nbd)
  jour[1] <- as.POSIXct(start,origin = "1970-01-01")+43200
  for(j in 2:nbd) jour[j] <- jour[j-1]+86400
  nbm <- nrow(xm)
  mois <- xm$Date
  valmois <- xm$Value

  #calcul
  k <- 1
  for(i in 1:(nbm-1)){
    nbj <- as.numeric(mois[i+1]-mois[i])
    vident <- (valmois[i+1]-valmois[i])/nbj
    valj <- valmois[i]+ (vident/2)
    valjour[k] <- valj
    for(j in (k+1):(k+nbj-1)) {
      valjour[j] <- valj+vident
      valj <- valjour[j]
    }
    k <-k+nbj
  }
  
  #tableau
  tstab <- tibble(Date=as_datetime(jour),Value=valjour,Station=as.factor(xm$Station)[1],
               Sensor=as.factor(xm$Sensor)[1])
  
  #ecriture sortie
  fileo <- paste0(nfse,"_D.hts")
  save(tstab, file=fileo)
  return(message("File written: ", fileo))

}

#Fin
