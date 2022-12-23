#' @title Daily average over a sequence of several years 
#'
#' @author P. Chevallier - Nov 2022
#'
#' @details The function means the values of each calendar day over a period larger 
#' than 4 years (i.e. it includes at least one Feb 29 day). The result is transfered
#' to the last possible hydrological year of the interval.
#'  
#'  In the special case of precipitation, where the distribution is discontinuous 
#'  over time, the original values of the last hydrological year are replaced by values 
#'  corrected proportionately.
#'
#' @param file File name to proceed
#' @param start Starting date (default = NA)
#' @param end Ending date (default = NA)
#' @param mhy Starting month of the hydrological year (default = 1)
#' @param precip Precipitation time series (default = FALSE)
#' @param dig Number of significant digits for Value (default = 1)
#'
#' @examples \dontrun{
#'
#' f <- h_avday(f,   start=NA, end=NA, mhy=10, precip=TRUE, dig=1)
#' }
#'

h_avday <- function (file, start=NA, end=NA, mhy=1, precip=FALSE, dig=1) {

  #initialisation
  Sys.setenv(TZ='UTC')
  Station <- Sensor <- NULL
  dn <- dirname (file)
  bn <- basename (file)
  bnse <- tools::file_path_sans_ext(bn)

  # controles
  moisjour <- NA
  load(file)
  x <- tstab
  if(is.na(start)) start <- x$Date[1]
  if(is.na(end)) end <- x$Date[nrow(x)]

  # cadrage du fichier
  ftemp <- h_restrict (file, start=start, end=end)
  load(ftemp)
  x <- tstab
  moisjour <- vector(length=nrow(x))
  file.remove(ftemp)
  # x1 <- mutate (x, moisjour = as.factor(paste0(month(x$Date),"-",day(x$Date))))
  x1 <- mutate (x, moisjour = as.factor(month(x$Date)*32 + day(x$Date)))
  x2 <- group_by(x1,moisjour)
  x3 <- summarize(x2,mean(Value))
  x3 <- ungroup(x3)

  Value <-x3$`mean(Value)`
  
  # tableau final
  if(year(end)%%4 !=0) bis <-FALSE else bis <- TRUE
  if(bis == FALSE) {
    Date <- vector(mode = "numeric", length=365)
    Value <- c(Value[1:59],Value[61:366])
    nbd <- c(1,32,60,91,121,152,182,213,244,274,305,335)
  } else {  
    Date <- vector(mode = "character", length=366)
    nbd <- c(1,32,61,92,122,153,183,214,245,275,306,336)
  }
  if (mhy==1) {
    deb <- as.POSIXct(paste0(year(end),"-01-01 12:00 UTC"))
  } else {
    deb <- as.POSIXct(paste0(year(end)-1,"-", mhy, "-01 12:00 UTC"))
    Value <- c(Value[nbd[mhy]:length(Date)],Value[1:nbd[mhy]-1])
  }
  Date[1] <- as.numeric(deb)
  for(i in 2:length(Date)) {
    Date[i] <- deb + 86400*(i-1)  
  }
  Date=as.POSIXct(Date, origin="1970-01-01", tz="UTC")
  Value <- round(Value, digits=dig)
  x4 <- tibble(Date, Value, Station = x$Station[1], Sensor = "avd")
  if (precip == FALSE) {
    meanav <- mean(tstab$Value)
    cat ("\nAnnual Mean of the averaged time-series", round(meanav, dig))
  }
  
  # cas des precipitations
  if (precip == TRUE) {
    totav <- sum(x4$Value)
    cat ("\nAnnual total of the averaged time-series", round(totav,1))
    ftemp <- h_restrict (file, start=x4$Date[1], end=x4$Date[nrow(x4)])
    load(ftemp)
    y <- tstab
    y$Value <- round(y$Value, dig)
    file.remove(ftemp)
    ratio <- totav/sum(y$Value)
    x4$Value <- y$Value*ratio
    x4$Value <- round(x4$Value, dig)
  }

  #ecriture fichier
  tstab <- x4
  fileo <- paste0(dn,"/avd_",bn)
  save(tstab, file=fileo)
  message("\nFile written: ", fileo,"\n")
  return (fileo)

}
# FIN
