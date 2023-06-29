#' @title Add, Modify or Remove discharge measurements (Shiny app)
#'
#' @author P. Chevallier - Dec 2020
#' @description Add, Modify or Remove discharge measurements for a station/sensor
#'
#' @param fsq htsr data base
#' @param sta Station Id.
#' @param sen Sensor Id.
#'
#' @return an actualized data base
#'


ds_dismeas <- function(fsq, sta, sen){
  save(fsq, sta, sen, file=system.file("extdata/fichier_discalib.RData",package="htsr"))
  #runApp(system.file("extdata/app_dismeas", package="htsr"),launch.browser=TRUE)
  runApp(system.file("extdata/app_dismeas", package="htsr"))
}

