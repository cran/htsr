#' @title Compute temperature, function of altitude
#'
#' @author P. Chevallier - Nov 2021
#'
#' @details The function computes an air temperature time-series at a given altitude,
#' based on a known air temperature time-series at a known altitude.
#'
#'
#' @param file File name of the known air temperature ts (°C)
#' @param alt0 Altitude of the known air temperature ts - default = 0 (m)
#' @param alt Altitude of the computed air- temperature ts (m)
#' @param grad Temperature gradient vs elevation - default = -0.0065 (°C/m)
#'
#' @return An hts file with the suffix _<alt>



w_temp_alt <- function (file , alt0 = 0, alt, grad = -0.0065) {

  Sys.setenv(TZ='UTC')
  Sensor <- Station <- Value <- NULL

  #initialisation
  load(file)
  nfse <- tools::file_path_sans_ext(file)
  fileo <- paste0(nfse,"_",as.character(alt),".hts")

  tstab$Value <- tstab$Value + (alt - alt0) * grad

#ecriture fichier

  save(tstab, file=fileo)
  message("\nFile written: ", fileo,"\n")

  return (fileo)

}
# end
