#' @title Create a wind table
#'
#' @author P. Chevallier - Dec 2019
#'
#' @description Create a tibble with wind direction and speed
#'
#' @param fsq Full name of the tshm data base
#' @param sta Station id
#' @param swd Id of wind direction sensor
#' @param swv Id of wind speed sensor
#'
#' @seealso \code{\link{p_wind}} plot wind roses
#'
#' @examples \dontrun{
#'
#' h_wind (fsq, sta="VB", swd="WD", swv="WV")
#' }
#'
#' @return
#' A tibble named "data_wind" with 5 columns date, month, year, wind_dir, wind_spd

# function h_wind

d_wind <- function(fsq, sta=NA, swd=NA, swv=NA){

  sta <- as.character(sta)
  swd <- as.character(swd)
  swv <- as.character(swv)

  #extraction
  tstab <- d_exp_hts (fsq, sta = sta, sen = swd)
  fwd <- (paste0(swd,"_",sta,".hts"))
  save(file=fwd,tstab)
  rm(tstab)
  tstab <- d_exp_hts (fsq, sta = sta, sen = swv)
  fwv <- (paste0(swv,"_",sta,".hts"))
  save(file=fwv,tstab)
  rm(tstab)

  #tibble de travail
  fcom <- h_common(c(fwd, fwv))
  load(fcom[1])
  data_wind <- select(tstab, date = Date)
  data_wind <- cutData(data_wind, type = "month")
  data_wind <- cutData(data_wind, type = "year")
  data_wind <- mutate(data_wind, wind_dir=tstab$Value)
  load(fcom[2])
  data_wind <- mutate(data_wind, wind_spd=tstab$Value)

  #suppression des fichiers de calcul
  file.remove(fwd)
  file.remove(fwv)
  file.remove(paste0("co_",fwd))
  file.remove(paste0("co_",fwv))


  return (data_wind)
}
