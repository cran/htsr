#' @title Shiny app: convert f file with fixed time-step
#'
#' @author P. Chevallier - Dec 2020 - Aug 2023
#'
#' @description Shiny application of the  functions \code{\link{h_timestep}} and \code{\link{h_month}}
#' 
#' @details
#' The output files are written in the specified working directory. 
#' 
#'
#' @return a shiny session
#'
#'
hs_tstep <- function(){
  #runApp(system.file("extdata/app_tstep", package="htsr"),launch.browser=TRUE)
  runApp(system.file("extdata/app_tstep", package="htsr"))
}
