#' @title Weighted sum of time-series
#'
#' @author P. Chevallier - Oct 2017-Nov 2020
#'
#'
#' @description The function only works on the common period of the files without NA values. It
#' operates weighted sums on one or several time-series. It is also possible to add a constant.
#'
#' @details For averaging n time-series one can use n weights wit a value of 1/n and constant = 0.
#'
#'
#' @return The function returns
#' + n hts files with the extracted common period
#' + 1 hts file named as the first file of the list
#' with the prefix w_. The sensor id is automatically set to "weighted".
#'
#' @param files List of file names to proceed
#' @param weights List of weights (must have the same length as files)
#' @param constant Constant to add (default = 0)
#'
#' @examples \dontrun{
#'
#' # choose time-series f1, f2, f3
#' f1 <- "foo1.hts" ; f2 <- "foo2.hts" ; f3 <- "foo3.hts"
#' # the new f time-series contains records f[i] = f1[i] - (0.5 * f2[i]) + (0.5 * f3[i]) + 5
#' f <- h_weightedsum(c(f1,f2,f3), c(1,-0.5,0.5)), 5)
#' # the new f time-series contains records f[i] = (1.12 * f1[i]) + 3
#' f <- h_weightedsum(f1, 1.12, 3)
#' }
#'

h_weightedsum <- function (files, weights, constant = 0) {

  #initialisation
  n <- length(files)
  if (length(weights) != n)
    return(warning("\nFiles and weights must have the same length."))

  dn <- dirname (files[1])
  bn <- basename (files[1])
  if (n > 1) files <- h_common(files)

  #calcul
  z <- NULL

  for (i in 1:n){
    load(files[i])
    z <- dplyr::arrange(tstab,Date)
    if(i==1) z$Value <- tstab$Value * weights[i]
    else z$Value <- z$Value + tstab$Value * weights[i]
  }
  z$Value <- z$Value + constant
  z$Sensor <- as.factor("weighted")

# Ecriture
  tstab <- z
  fileo <- paste0(dn,"/ws_",bn)
  save(tstab, file=fileo)
  message("\nFile written: ", fileo,"\n")

  return (fileo)

}
# FIN
