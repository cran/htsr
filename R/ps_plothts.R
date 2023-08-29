#' @title Shiny app: plot hts files
#'
#' @author P. Chevallier - May 2020 - Aug 2023
#'
#' @description Shiny application of the \code{\link{p_line}} and \code{\link{p_bar}}
#' functions, associated with \code{\link{z_set}}
#'
#' @details When launched, a shiny window is open. Follow the instructions and steps.
#'
#'
#'
ps_plothts <- function(){
  runApp(system.file("extdata/app_plothts", package="htsr"))
}
