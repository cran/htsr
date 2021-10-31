#' @title Rolling average of a daily time-series
#'
#' @author P. Chevallier - Apr 2020
#'
#' @description The function compute a rollong average of daily time-series values.
#' NA values are removed.
#'
#' @details The output file is named with a ro_ prefix. The computation can considers
#' the values before and after the current time step (position = "central") or the values
#' before the current time step. If the position is "central", the position must be an
#' odd integer.
#'
#' @param file File name to proceed
#' @param ti  Time interval of computation in days (default = 7)
#' @param position Position "central" or "right"


h_rollav <- function (file, ti = 7, position = "central") {

 #controle
  if (position != "central"&& position !="right") stop ("position must be central or right")
  if (((ti %% 2) == 0) && position == "central") stop ("ti must be odd when position is central")

  Sys.setenv(TZ='UTC')
  dn <- dirname (file)
  bn <- basename (file)
  fileo <- paste0(dn,"/ro_",bn)


  # execution
  load(file) ; wt <- tstab
  rollav <- as.numeric(NA) ; length(rollav) <- nrow(wt)
  for (i in 1:nrow(wt)){
    if (position == "central") {
        xDate_b <- wt$Date[i] - ti/2*86400
        xDate_a <- wt$Date[i] + ti/2*86400
      } else {
        xDate_b <- wt$Date[i] - ti + 1
        xDate_a <- wt$Date[i]
      }
    wt1 <- dplyr::filter(wt, Date >= xDate_b & Date <= xDate_a)
    rollav[i] <- mean(wt1$Value, na.rm = TRUE)
  }
 tstab$Value <- rollav

# sauve le resultat
  save(tstab, file=fileo)

# retour
  message("\nFile written: ", fileo,"\n")
  return (fileo)

}
# end
