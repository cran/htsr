#' @title Edit settings
#'
#' @author P. Chevallier - nov 2018 - nov 2020
#'
#' @description Utility for editing the settings of the htsr package.
#'
#' @details The function allows to edit user settings for time zone and color palette.
#'
#' @details The settings are stored in an external data file of the htsr package,
#' named "settings.RData".
#'
#' @details tz is the time zone coded following the Olson standard list.
#'
#' @details my palette is a list of 12 colors from the R color name list.
#'
#' @param tz Logical, setting time zone (default=FALSE)
#' @param mapal Logical, setting my palette (default=FALSE)
#'


z_set <- function(tz = FALSE, mapal=FALSE){
  if(tz==FALSE && mapal==FALSE)
    return (warning ("\nAt least one variable tz or mapal must be TRUE"))
  nbst <- conf <- fil <- NULL
  load(file=system.file("extdata/settings.RData",package="htsr"))

  # time zone tzo
  if(tz == TRUE) {
    message ("\nCurrent time-zone: ", tzo)
    tzo <- edit(tzo)
    repeat{
      if(tzo %in% OlsonNames()) break
      else warning(" Unknown timezone")
    }
    save(nbst, tzo, mapalette, conf, fil, file=system.file("extdata/settings.RData",package="htsr"))
  }

  # personnal color palette mapalette
  if(mapal == TRUE) {
    message("\nCurrent colors:")
    mapalette <- edit(mapalette)
    repeat{
      correct <- TRUE
      for (i in 1:12)
        if(mapalette[i] %in% (colors()) == FALSE) {correct <- FALSE ; k <- i ; next}
      if (correct == FALSE) {
        warning(" Unknown color:", mapalette[k])
        mapalette <- edit(mapalette)
      } else break
    }
    save(nbst, tzo, mapalette, conf, fil, file=system.file("extdata/settings.RData",package="htsr"))
  }

  return()
}
#FIN
