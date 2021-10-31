#' @title Bind 2 time-series on consecutive periods
#'
#' @author P. Chevallier - Mar-Nov 2020
#'
#' @description The fonction binds the data of 2 hts time-series
#' for consecutive date/time records (precision of the second) of the same station.
#'
#' @details In the list, the files must be ordered from the oldest to the newest.
#' If gap is TRUE, a gap is introduced between both series.
#'
#' @param files List of char, File names to process.
#' @param sensor New sensor name of the resulting hts file (default ="NewS")
#' @param gap Introduce or not a gap between both series (default = TRUE)
#'
#' @return hts file resulting of the operation; its names are composed as:
#' <sensor>_<station>.hts, with the prefix na, if a gap has been introduced.
#'
#' @examples \dontrun{
#'
#' f <- h_bind(files = c("foo1.hts","foo2.hts"), sensor = "NewOne")
#' }
#'

h_rbind <- function (files, sensor = "NewS", gap = TRUE) {

  # initialisation et tests
  dn <- dirname(files[1])
  for (i in 1:2){
    nfe <- tools::file_ext(files[i])
    if (nfe!="hts")
      return(warning("\nThe file",files[i], "is not a hts time-series.\n"))
  }
  sta = NA ; length(sta) <- 2
  for(i in 1:2){
    load(files[i])
    sta[i]=as.character(tstab$Station[1])
    if (sta[i]!=sta[1])
      return(warning("\nThe stations are not the same.\n"))
  }

  # action
  z = NA
  load(files[1])
  z <- tstab
  finit=max(tstab$Date)
  if (gap == TRUE)
    z <- add_row(z, Date = finit +1 , Station = tstab$Station[1], Sensor = tstab$Station[1])
  load(files[2])
  commence=min(tstab$Date)
  if (commence < finit)
    return(warning("\nThe files are not consecutive.\n"))
  if (gap == TRUE)
    tstab <- add_row(tstab, Date = commence - 1 , Station = tstab$Station[1], Sensor = tstab$Station[1])

  z <- dplyr::bind_rows(z, tstab)
  z <- arrange(z, Date)
  z$Sensor <- as.factor(sensor)
  tstab <- z
  fileo <- paste0(dn,"/",sensor,"_", sta[1],".hts")
  save(tstab, file = fileo)


  # retour
  message("\nFile ",fileo," written\n")
  return(fileo)
}
