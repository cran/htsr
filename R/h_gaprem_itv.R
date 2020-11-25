#' @title Remove gaps in a time-series with a time interval threshold
#'
#' @author P. Chevallier - Nov 2019
#'
#' @details Remove the missing values when the time interval between the previous
#' and the next record is less than a fixed threshold
#'
#' @param file File name to proceed
#' @param itv0 Time threshold in seconds, default = 43201 (i.e 12 hours)
#'
#' @return a time-series file with the prefix gr_

h_gaprem_itv <- function (file, itv0=43201){

  # fonction u_gaprem_mul
  u_gaprem_mul <- function (file) {
    Sensor <- Station <- Value <- NULL
    load(file)
    mul <- as.numeric(NA) ; length(mul) <-nrow(tstab)
    if (is.na(tstab$Value[1])) mul[1] <- 1 else mul[1] <- 0
    for (i in 1:nrow(tstab)) {
      if (is.na(tstab$Value[i])) mul[i] <- mul[i-1]+1 else mul[i] <- 0
    }
    x <- dplyr::mutate(tstab, mul)
    x1 <- dplyr::filter (x, mul == 0 | mul == 1 )
    tstab <- dplyr::select(x1, Date, Value, Station, Sensor)
    save(tstab, file="temp.hts")
    return ()
  }

  # preparation
  Sensor <- Station <- Value <- NULL
  dn <- dirname(file)
  bn <- basename(file)
  fileo <- paste0(dn,"/gr_",bn)
  u_gaprem_mul(file)
  load("temp.hts")

  # traitement
  itv <- as.numeric(NA) ; length(itv) <-nrow(tstab)
  itv[1] <- itv0
  for (i in 2:length(itv)) {
    if (!is.na(tstab$Value[i])) itv[i] <- itv0
    else {
      if (i == length(itv)) itv[i] <- itv0
      else itv[i] <- as.numeric(as.POSIXct(tstab$Date[i+1], origin = "1970-01-01")) -
        as.numeric(as.POSIXct(tstab$Date[i-1], origin = "1970-01-01"))
    }
  }
  crit <- NA ; length(crit) <-nrow(tstab)
  for (i in 1:nrow(tstab)) crit[i] <- (is.na(tstab$Value[i])) & (itv[i] < itv0)
  x <- dplyr::mutate(tstab, itv,  crit)
  x1 <- dplyr::filter (x, crit == FALSE)
  tstab <- dplyr::select(x1, Date, Value, Station, Sensor)

  # ecriture
  file.remove ("temp.hts")
  save(tstab, file=fileo)
  message("File written: ",fileo,"\n")
  return(fileo)
}


