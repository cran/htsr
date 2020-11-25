#' @title Replace a value by another
#'
#' @author P. Chevallier - Oct 2017- Nov 2020
#'
#' @details The output file is named with a re_ prefix.
#'
#' @param file File name to proceed
#' @param old.val Value to be replaced
#' @param new.val New value
#'
#' @examples \dontrun{
#'
#' f <- ts_remplace_ts(f,  NA, 0)
#' }
#'

h_replace <- function (file, old.val, new.val) {

  #initialisation
  NValue <- Station <- Sensor <- NULL
  dn <- dirname (file)
  bn <- basename (file)

  # lecture du fichier

  oldnew <- function (x , old.val, new.val) {
    if (x == old.val) x <- new.val
    return (x)
  }
  load(file)
  x <- mutate(tstab, NValue = NA)
  for (i in 1:nrow(x)) x$NValue[i] <- oldnew (x$Value[i] , old.val, new.val)

  tstab <- select(x, Date, Value = NValue, Station, Sensor)

  #ecriture fichier
  fileo <- paste0(dn,"/re_",bn)
  save(tstab, file=fileo)
  message("\nFile written: ", fileo,"\n")
  return (fileo)

}
# FIN
