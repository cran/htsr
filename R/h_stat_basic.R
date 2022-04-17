#' @title Basic statistics of a time-series
#'
#' @author P. Chevallier - Oct 2017 - Feb 2022
#'
#' @description Compute the main statistic parameters of a time-series
#'
#' @param files vector of file names to process

#' @return a tibble with the basic stats of the files.
#'
#' @examples \dontrun{
#' simplestat <- h_stat_basic(c("foo1.hts", "foo2.hts")
#' }
#'

h_stat_basic <- function(files){

  #initialisation
  tstab <- Value <- NULL
  for (i in 1:length(files)){
    nfe <- tools::file_ext(files[i])
    if (nfe!="hts")
      return(warning("\nThe file is not a hts time-series.\n"))
  }

#Statistiques simples
  nam <- vector(mode = "numeric", length = length(files))
  nbval <- moy <- std <- vector(mode = "numeric", length = length(files))
  qut <- array (dim = c(length(files), 7))
  for (i in 1:length(files)){
    load(files[i])
    nam[i] <- basename(files[i])
    serie <- tstab$Value
    nbval[i] <- length (serie)
    moy[i] <- mean(serie, na.rm=TRUE)
    qutt <- quantile(serie, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm=TRUE)
    qut[i,] <- qutt
    std[i] <- sd(serie, na.rm=TRUE)
  }
  x <- tibble (nam, nbval, moy, std, qut[,1], qut[,2], qut[,3], qut[,4], qut[,5], qut[,6], qut[,7])
  colnames(x) <- c("f_name", "size", "mean", "std", "min", "0.10", "0.25","0.50", "0.75", "0.90", "max")
  
  return (x)
}
# FIN
