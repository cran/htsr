#' @title Shiny app: export hts files from a sqlite data base
#'
#' @author P. Chevallier - Apr 2020 - Oct 2021
#'
#' @description Shiny application of the \code{\link{d_exp_hts}} function
#'
#' @details
#'  Complete the requested information in the left panel, then press the submit button in
#' order to extract the file. If you want to display the plot of the extracted file,
#' choose "line" or "bar" and press the plot button. When finished press "done".
#'
#' @details If the data do not exist, the app crashes and error messages
#' are displayed in the console window.
#'
#' @param fsq File name of the Sqlite data base
#'
#' @return a shiny session
#'
ds_exp_hts <- function(fsq) {
  fsq <- fsq
  save(fsq, file=system.file("extdata/fichier_fsq.RData",package="htsr"))
  runApp(system.file("extdata/app_exphts", package="htsr"))
}
