#' @title Edit settings
#'
#' @author P. Chevallier - nov 2018 - aug 2023
#'
#' @description Utility for editing the settings of a plotting operation.
#'
#' @details tz is a time zone included in the Olson list.
#'
#' @details palette is a list of colors chosen in the palette.pals list.
#'
#' @param file.names character, compulsory list of file names to be plotted (no default)
#' @param plot.label character, list of labels for each plot (default = "label" [i])
#' @param line.type integer in the interval [0-6] (default = 1)
#' @param line.width numeric, width (in point unit) of the line (default = 0.2)
#' @param point.shape integer in the interval [0-25] (default = 20)
#' @param point.size numeric, size of the plotted points (in point unit) (default = 8)
#' @param title character, plot title (default="Title")
#' @param yaxis character, label of the y-Axis (default="y-Axis)
#' @param fixy logical, set limits on the y-Axis (default=FALSE)
#' @param ymin numeric, min limit on the Y-Axis (default = NA)
#' @param ymax numeric, max limit on the Y-Axis (default = NA)
#' @param fixtime logical, set a time interval (default = FALSE)
#' @param begin "YYYY-MM-DD" or date function, beginning time (default = today())
#' @param end "YYYY-MM-DD" or date function, ending time (default = today())
#' @param normval logical, normalizes the plotted value (default = FALSE)
#' @param trend logical, overlays a linear trend line (default = FALSE)
#' @param facet logical, shows a facet plot (default = FALSE)
#' @param point.plot logical, shows points on the plot (default = FALSE)
#' @param tz character, time zone (default="UTC")
#' @param palette character, palette (default="ggplot2")
#'
#'
#' @returns A file "settings.RData" in the working directory

# library(htsr)
# library(tidyverse)


z_set <- function( file.names= file.names, plot.label="label",
	line.type = 1, line.width = 0.2,
	point.shape = 15, point.size = 8,
	title = "Title", yaxis = "y-Axis", normval = FALSE,
	fixtime = FALSE, begin = today(), end = today(),
	fixy = FALSE, ymin = as.numeric(NA), ymax = as.numeric(NA),
	trend = FALSE, facet=FALSE, point.plot = FALSE,

	tz = "UTC", palette="ggplot2")
	{

# fil
	fil <- tibble(file.names = as.character(file.names),
		plot.label = as.character(plot.label),
		line.type = as.integer(line.type), line.width = as.numeric(line.width),
		point.shape = as.integer(point.shape),
		point.size = as.numeric(point.size))

	nf <- nrow(fil)

	if (nf > 8) {warning(" The upload files are limited to 8.")}
	for (i in 1:nf) {
		# fil$plot.label[i] <- paste("label", i)
		if(tools::file_ext(fil$file.names[i]) != "hts")
			{warning(" Not allowed file type")}
		if (fil$line.type[i] <0 || fil$line.type[i] > 6)
			{warning(" line.type must be in the interval 0-6.")}
	  if (fil$point.shape[i] <0 || fil$point.shape[i] > 24)
  		{warning(" point.shape must be in the interval 0-24.")}
  }

#	conf
	conf <- c(as.character(title),  as.character(yaxis),
		as.logical(normval), as.logical(fixtime),
		as_date(begin), as_date(end),
		as.logical(fixy), as.numeric(ymin), as.numeric(ymax),
		as.logical(trend), as.logical(facet),
		as.logical(point.plot))

	if (begin > end) {warning(" The beginning is later than the end.")}
	if (fixy == TRUE && ymin > ymax) {warning(" The min is higher than the max.")}

# tz & palette
  if(tz %in% OlsonNames() == FALSE) {warning(" Unknown timezone.")}
	if(palette %in% palette.pals() == FALSE) {warning(" Unknown palette.")}

	save (file=system.file("extdata/settings.RData",package="htsr"), tz, palette, conf, fil)

  return(cat ("Setting file written"))
}
#FIN
