#' @title Shiny app: inventory of htsr sqlite data base
#'
#' @author P. Chevallier - Sep 2020 - Aug 2023
#'
#' @description Shiny application of the \code{\link{d_inventory}} function
#'
#' @details
#'  Complete the requested information in the left panel, then press the submit button.
#'  If the station field is empty, the function will return the list of the stations
#'  in the data base. If the station field is filled, the function will return the
#'  list of the station sensors in the data base. When finished press "done".
#'
#' @details If the data do not exist, the app crashes and error messages
#' are displayed in the console window.
#'

#' @return a shiny session
#'
#'
ds_inventory <- function(){
  #runApp(system.file("extdata/app_invent", package="htsr"),launch.browser=TRUE)
  runApp(system.file("extdata/app_invent", package="htsr"))
}
