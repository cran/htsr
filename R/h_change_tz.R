#' @title Change the time zone of a time series
#'
#' @author P. Chevallier - June 2023
#'
#' @details 
#' The output file is named with a tz prefix.
#'
#' @param file File name to proceed
#' @param tz1 original time zone (default = "UTC")
#' @param tz2 new time zone (default = "Europe/Paris")
#'

h_changetz <- function(file, tz1="UTC", tz2="Europe/Paris"){
  load(file)
  dn <- dirname (file)
  bn <- basename (file)
  
  d0 <- tstab$Date
  d1 <- force_tz(d0, tzone=tz1)
  d2 <- force_tz(d1, tzone=tz2)
  tstab$Date <- d2

  fileo <- paste0(dn,"/tz_",bn)
  save(tstab, file=fileo)
  message("\nFile written: ", fileo,"\n")
}