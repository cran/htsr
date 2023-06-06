#' @title Convert specific humidity to relative humidity
#'
#' @details Converting specific humidity into relative humidity.
#' from Bolton 1980 The computation of Equivalent Potential Temperature 
#'  
#' @param f_spechum file of specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass
#' @param f_temp file of temperature degrees C
#' @param f_atm file of atmospheric pressure in mb
#' 
#' @return a file of relative humidity, ratio of actual water mixing ratio to saturation mixing ratio
#' 
#' @author P. Chevallier - Nov 2022
#' 
#' @source David LeBauer - 2014
#' @source from Bolton 1980 The computation of Equivalent Potential Temperature
#' @source \url{https://earthscience.stackexchange.com/questions/2360/how-do-i-convert-specific-humidity-to-relative-humidity}

w_spechum2relhum <- function(f_spechum, f_temp, f_atm){

  Sys.setenv(TZ='UTC')
  Sensor <- Station <- Value <- NULL
  dn <- dirname(f_spechum)
  
  load(f_temp)
  x <- tstab
  es <- qair <- press <- e <- rh <- numeric(length=nrow(tstab))
  es <-  6.112 * exp((17.67 * tstab$Value)/(tstab$Value + 243.5))
  
  load(f_spechum)
  qair <- tstab$Value
  
  load(f_atm)
  press <- tstab$Value
  
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
  
  z <- tibble(Date=x$Date, Value = rh*100, Station = x$Station[1], Sensor = paste0("relhum"))
  fileo <- paste0(dn,"/",z$Station[1],"_",z$Sensor[1],".hts")
  tstab <- z
  save(tstab, file=fileo)
  
  return(fileo)
}