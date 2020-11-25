#' @title Shiny app: plot hts files
#'
#' @author P. Chevallier - May 2020
#'
#' @description Shiny application of the \code{\link{p_line_app}} and \code{\link{p_bar_app}}
#' functions
#'
#' @details When launched, this function first ask to give the number of files to plot
#' and to select them. After that a shiny window is open.
#'  The setting tab must be first completed and saved. Then go to the plot tab, complete
#'  the plotting settings and press Plot.
#'  If you want to modify the initial settings, return to the setting tab.
#'  After setting changes
#'  press Save one more time, go to the plot tab and press Plot!
#'
#' @details When finished, press Done to exit from the shiny windows
#'
#'
#'
p_plothts <- function(){
  stdin()
  tzo <- conf <- mapalette <- NULL
  load(file = system.file("extdata/settings.RData",package="htsr"))
  nbst <- as.numeric(readline("Number of files to plot : "))
  filei <- as.character(NA)[1:nbst]
  ser <- as.character(NA)[1:nbst]
  for(i in 1:nbst) {
    filei[i] <- file.choose()
    ser[i] <- paste0("ser_id",i)
  }
  myfil <- tibble::tibble(filename = filei, series_id = ser, color = "black",
                          lineshape = 1, linewidth = 0.2, plotpoint = FALSE, pointshape = 20,
                          pointsize = 8)
  fil <- myfil
  save(nbst, fil, conf, mapalette, tzo, file = system.file("extdata/settings.RData",package="htsr"))
  runApp(system.file("extdata/app_plothts", package="htsr"))
}
