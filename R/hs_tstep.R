#' @title Shiny app: convert f file with fixed time-step
#'
#' @author P. Chevallier - Dec 2020 - Oct 2021
#'
#' @description Shiny application of the  functions \code{\link{h_timestep}} and \code{\link{h_month}}
#'
#'
#' @param file File to process

#' @return a shiny session
#'
#'
hs_tstep <- function(file){
  fil <- file
  save(fil, file=system.file("extdata/fichier_fhts.RData",package="htsr"))
  runApp(system.file("extdata/app_tstep", package="htsr"))
}
