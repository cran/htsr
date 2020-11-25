#' @title Shiny app: export hts files from a sqlite data base
#'
#' @author P. Chevallier - Apr-Nov 2020
#'
#' @description Shiny application of the \code{\link{d_exp_hts}} function
#'
#' @details When launched, this function first ask to choose the sqlite data base.
#'  Complete the requested information in the left panel, then press the submit button in
#' order to extract the file. If you want to display the plot of the extacted file,
#' choose "line" or "bar" and press the plot button. When finished press "done".
#'
#' @details If the data do not exist, the app crashes and error messages
#' are displayed in the console window.
#'
#' @param fsq File name of the Sqlite data base
#'
#' @return a shiny session
#'
d_exphts <- function(fsq) {
  fsq <- fsq
  save(fsq, file=system.file("extdata/fichier_fsq.RData",package="htsr"))
  runApp(system.file("extdata/app_exphts", package="htsr"))
}
