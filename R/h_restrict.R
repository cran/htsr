#' @title Restrict a series between 2 dates
#'
#' @author P. Chevallier - Nov 2017-Jan 2019
#'
#' @details The output file is named with a rs_ prefix.
#'
#' @param file File name to proceed
#' @param start Start date/time (included) of POSIXct class (default = NA)
#' @param end End date/time (excluded) of POSIXct class (default = NA)
#'
#'
#'

h_restrict <- function(file, start=NA, end=NA) {
  Sys.setenv(TZ='UTC')
  if(is.na(start) & is.na(end))
    return(warning("\nAt least one date must not be NA.\n"))
  load(file)
  dn <- dirname (file)
  bn <- basename (file)

  start <- as_datetime(start)
  if(is.na(start)) start <- tstab$Date[1]
  end <- as_datetime(end)
  if(is.na(end)) end <- tstab$Date[nrow(tstab)]
  tstab <- dplyr::filter(tstab, Date >= start & Date <= end)
  fileo <- paste0(dn,"/rs_",bn)
  save(tstab, file=fileo)
  message("\nFile written: ", fileo,"\n")
  return (fileo)

}
# FIN
