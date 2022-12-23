#' @title Compute atmospheric pressure, function of altitude
#'
#' @author P. Chevallier - Nov 2021 / Nov 2022
#'
#' @details The function computes an atmospheric pressure time-series at a given altitude,
#' based on a known atmospheric pressure time-series at the sea level. It also needs
#' the air temperature time-series at the sea level for the same times.
#'
#' In order to verify that both time-series correspond, it is strongly recommended to 
#' run previously the function \code{\link{h_common}}.
#'
#'
#' @param f_atmp File name of the known atmospheric pressure ts (mb)
#' @param f_temp File name of the air temperature at the known altitude (°C)
#' @param alt Altitude of the computed air- temperature ts (m)
#'
#' @return An hts file with the suffixe _<alt>



w_atmp_alt <- function (f_atmp, f_temp, alt) {

  Sys.setenv(TZ='UTC')
  Sensor <- Station <- Value <- NULL

  #initialisation
#  files <- h_common(c(f_atmp, f_temp))
  load(f_atmp)
  x <- tstab
  load(f_temp)
  y <- tstab
  nfse <- tools::file_path_sans_ext(f_atmp)
  fileo <- paste0(nfse,"_",as.character(alt),".hts")

  #calcul
  val <- numeric(length=nrow(x))
  val <- x$Value * exp(-(7*9.81)/(2*1006*(y$Value + 273))*(alt))
  z <- tibble(Date=x$Date, Value = val, Station = x$Station[1], Sensor = paste0("atmp",alt))
  tstab <- z
  

#ecriture fichier
  save(tstab, file=fileo)
  message("\nFile written: ", fileo,"\n")

  return (fileo)

}
# end
