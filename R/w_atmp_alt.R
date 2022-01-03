#' @title Compute atmospheric pressure, function of altitude
#'
#' @author P. Chevallier - Nov 2021
#'
#' @details The function computes an atmospheric pressure time-series at a given altitude,
#' based on a known atmospheric pressure time-series at a known altitude. It also needs
#' an air temperature ts at the known altitude.
#'
#' The result is given for the common periods of the atmospheric pressure and the
#' air temperature ts, based on the result of the h_common function.
#'
#'
#' @param f_atmp File name of the known atmospheric pressure ts (mb)
#' @param f_temp File name of the air temperature at the known altitude (°C)
#' @param alt0 Altitude of the known atmospheric pressure ts - default = 0 (m)
#' @param alt Altitude of the computed air- temperature ts (m)
#'
#' @return An hts file with the prefix co_ and the suffix _<alt>



w_atmp_alt <- function (f_atmp, f_temp, alt0 = 0, alt) {

  Sys.setenv(TZ='UTC')
  Sensor <- Station <- Value <- NULL

  #initialisation
  files <- h_common(c(f_atmp, f_temp))
  load(files[1])
  x <- tstab
  load(files[2])
  y <- tstab
  nfse <- tools::file_path_sans_ext(files[1])
  fileo <- paste0(nfse,"_",as.character(alt),".hts")

  #calcul
  coeff <- numeric(length=nrow(y))
  coeff <- exp(-(7*9.81)/(2*1006*(y$Value + 273))*(alt-alt0))
  x$Value <- x$Value * coeff
  tstab <- x

#ecriture fichier
  save(tstab, file=fileo)
  message("\nFile written: ", fileo,"\n")

  return (fileo)

}
# end
