#' @title Cumul of time-series
#'
#' @author P. Chevallier - Oct 2017-Jan 2019
#'
#' @description The function returns a time-series of cumulated values. If the value
#' is negative, the absolute value is taken.
#' It is possible to limit the computation time interval. NA values are ignored.
#'
#' @details The output file is named with a cu_ prefix.
#'
#' @param file File name to proceed
#' @param start Start date, default = NA
#' @param end End date, default NA

#'
#' @examples \dontrun{
#'
#' f <- h_cumul(f, start="2012-1-1", end = "2013-1-1")
#' }
#'

h_cumul <- function (file, start=NA, end =NA) {

  Sensor <- Station <- Value <- cumvaleur <- NULL

  Sys.setenv(TZ='UTC')
  dn <- dirname (file)
  bn <- basename (file)

  # execution
  fileo <- paste0(dn,"/cu_",bn)
  load(file)
  tstab <- dplyr::filter(tstab, !is.na(tstab$Value))
  tstab$Value <- abs(tstab$Value)
  if(is.na(start)) {
    tstab <- dplyr::mutate(tstab, cumvaleur = cumsum(Value))
  } else {
    tstab <- dplyr::filter(tstab, Date >= start & Date <= end)
    tstab <- dplyr::mutate(tstab, cumvaleur = cumsum(Value))
  }
  tstab <- dplyr::select(tstab, Date, cumvaleur, Station, Sensor)
  colnames(tstab)[2] <- "Value"


# sauve le resultat
  save(tstab, file=fileo)

# retour
  message("\nFile written: ", fileo,"\n")
  return (fileo)

}
# end
