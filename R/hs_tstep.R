#' @title Shiny app: convert f file with fixed time-step
#'
#' @author P. Chevallier - Dec 2020
#'
#' @description Shiny application of the  functions \code{\link{h_timestep}} and \code{\link{h_month}}
#'
#'
#' @param fhts File name of the Sqlite data base

#' @return a shiny session
#'
#'
hs_tstep <- function(fhts){
  fhts <- fhts
  save(fhts, file=system.file("extdata/fichier_fhts.RData",package="htsr"))
  runApp(system.file("extdata/app_tstep", package="htsr"))
}
