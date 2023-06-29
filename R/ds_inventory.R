#' @title Shiny app: inventory of htsr sqlite data base
#'
#' @author P. Chevallier - Sep-Nov 2020
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
#' @param fsq File name of the Sqlite data base

#' @return a shiny session
#'
#'
ds_inventory <- function(fsq){
  fsq <- fsq
  save(fsq, file=system.file("extdata/fichier_fsq.RData",package="htsr"))
  #unApp(system.file("extdata/app_invent", package="htsr"),launch.browser=TRUE)
  runApp(system.file("extdata/app_invent", package="htsr"))
}
