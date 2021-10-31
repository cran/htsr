#' @title Change Station id or Sensor id in a hts file
#'
#' @author P. Chevallier - Nov 2017-Jan 2019
#'
#' @description The function changes the station and/or the sensor id of a hts file.
#' The new file is renamed with the new ids and a prefix n_: nw_<sensor.id>_<station.id>.hts, BUT the
#' eventual prefixes or suffixes of the original name are not conserved.
#' The original file is not removed.
#'
#' @param file file to proceed
#' @param sta new station id (default: NA)
#' @param sen new sensor id (default: NA)
#' @param overwrite TRUE / FALSE (default) if the output file exists
#'
#'

f_change_id <- function (file, sta=NA, sen=NA, overwrite = FALSE) {

  # Control
  if(is.na(sta) & is.na(sen))
    return(warning("\nAt least one new id must be given.\n"))

  # Output file name
  dn <- dirname(file)
  fileo <- paste0(dn,"/nw_",sen,"_",sta,".hts")
  if(overwrite == FALSE && file.exists(fileo))
    return(warning("\nThe output file already exists.\n"))

  # Proceed
  load(file)
  if (!is.na(sta)) tstab$Station <- as.factor(sta)
  if (!is.na(sen)) tstab$Sensor <- as.factor(sen)

  # Final
  save(tstab, file=fileo)
  return (message("\nFile written ", fileo))
}

