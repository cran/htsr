#' @title Basic statistics of a time-series
#'
#' @author P. Chevallier - Oct 2017-Jan 2019
#'
#' @description Compute the main statistic parameters of a time-series
#'
#' @param file File to process

#' @return nb_val, mean, standard deviation, min, quantile .25, median, quantile .75, max
#' and display a boxplot with these values.
#'
#' @examples \dontrun{
#'
#' simplestat <- h_stat_basic("foo.hts")
#' }
#'



h_stat_basic <- function(file){

  #initialisation
  tstab <- Value <- NULL
  nfe <- tools::file_ext(file)
  if (nfe!="hts")
    return(warning("\nThe file is not a hts time-series.\n"))

#Statistiques simples
  load(file)
  serie <- tstab$Value
  nbval <- length (serie)
  moy <- mean(serie, na.rm=TRUE)
  qut <- quantile(serie, na.rm=TRUE)
  std <- sd(serie, na.rm=TRUE)

  message("\nsize               ",nbval)
  message("\nmean               ",moy)
  message("\nstandard deviation ",std)
  message("\nminimum            ",qut[1])
  message("\nquantile 25%       ",qut[2])
  message("\nmedian             ",qut[3])
  message("\nquantile 75%       ",qut[4])
  message("\nmaximum            ",qut[5])

  p <- ggplot(tstab, aes(x=1, y=Value)) + geom_boxplot(notch=TRUE) +
    stat_summary(fun="mean", geom="point", shape=23, size=3, fill = "white")
  show(p)

  return (c(nbval, moy, std, qut[1], qut[2], qut[3], qut[4], qut[5]))
}
# FIN
