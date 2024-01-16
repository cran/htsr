#' @title Adjust a time series to a statistical model
#'
#' @author P. Chevallier - January 2024
#'
#' @details The function adjust a time series with a statistical model. For instance
#' it works only with a linear model.
#'
#' "year" corresponds to an average year of 365.25 days and month to an average month of 30,4575 days.
#'
#' @param file File to proceed
#' @param time_unit to be chosen in: "100y", "year", "month", "day"
#'
#'



h_adjust <- function (file , time_unit = "year") {

  #initialisation
  if ( !(time_unit) %in% c("100y", "year", "month", "day")) stop(cat("\ntime_unit not allowed!"))
	tstab <- NULL
	load(file)
	if (is.null(tstab)) stop(cat("\nfile  is not an hts time series file!"))
	fact <- 86400
	if (time_unit == "100y") fact <- fact*365.25*100
	if (time_unit == "year") fact <- fact*365.25
	if (time_unit == "month") fact <- fact*30.475

	#calcul
	tstab$Date <- as.numeric(tstab$Date) / fact
	rep <- lm(formula = Value ~ Date, data = tstab)

	return(rep)
}
# end
