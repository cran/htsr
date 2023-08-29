#' @title Shiny app: create, modify or remove a station from a data base
#'
#' @author P. Chevallier - Apr 2020 - Aug 2023
#'
#' @description Shiny application of the \code{\link{d_station}} function
#'
#' @param fsq File name of the Sqlite data base
#'
#' @return a shiny session
#'
ds_station <- function(fsq) {
  # fsq <- fsq
  # save(fsq, file=system.file("extdata/fichier_fsq.RData",package="htsr"))
  # runApp(system.file("extdata/app_station", package="htsr"),launch.browser=TRUE)
  runApp(system.file("extdata/app_station", package="htsr"))
}
