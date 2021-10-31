#' @title Replace errors with gaps in a time-series based on neighboring values
#'
#' @author P. Chevallier - Nov 2019
#'
#' @details  Replace errors with gaps in a time-series based on neighboring values
#'
#' @param file, File name to proceed
#' @param nv Number of below and above neighboring values to take into account,
#' default = 1
#' @param itv0 Threshold of minimum time gap (see function h_gaprem_itv)
#' @param df Deviation value factor for testing if a value is correct or not
#'
#'
#' @return a time-series file with the prefix eg_

h_gaperr <- function (file, nv = 1, itv0 = 43201, df){

  # preparation
  dn <- dirname(file)
  bn <- basename(file)
  fileo <- paste0(dn,"/eg_",bn)
  file1 <- h_gaprem_itv(file = file, itv0 = itv0)
  load (file1)

  if (nv < 1) return(warning("\nThe value of nv must be at least 1!"))
  if (nrow(tstab) <= 2*nv+1)
    return(warning
      ("\nThe length of the time-series is too short for the chosen nv!"))

  # traitement
  for (i in 1:nrow(tstab)) {
    if (is.na(tstab$Value[i])) next
    if (i <=nv) {
      for (j in 1:nv){
        if (j ==1) abx <- tstab$Value[i+j]
        else abx <- c(abx,tstab$Value[i+j])
      }
      ab <- mean(abx)
      be <- NA
      besup <- NA ; absup <- ab + df
      beinf <- NA ; abinf <- ab - df
    }
    if (i > nrow(tstab)-nv) {
      for (j in 1:nv){
        if (j ==1) bex <- tstab$Value[i-j]
        else bex <- c(bex,tstab$Value[i-j])
      }
      be <- mean(bex)
      ab <- NA
      besup <- be + df ; absup <- NA
      beinf <- be - df ; abinf <- NA
    }
    if (i > nv || i <= nrow(tstab)-nv){
      for (j in 1:nv){
        if (j ==1) {abx <- tstab$Value[i+j] ; bex <- tstab$Value[i-j]}
        else {abx <- c(abx,tstab$Value[i+j]) ; bex <- c(bex,tstab$Value[i-j])}
        be <- mean(bex)
        ab <- mean(abx)
      }
    besup <- be + df ; absup <- ab + df
    beinf <- be - df ; abinf <- ab - df
  }
    if(is.na(be) && is.na(ab)) {
      tstab$Value[i] <- NA
      next
    }
    if(is.na(be) && !is.na(ab)){
      if(tstab$Value[i] > absup || tstab$Value[i] < abinf) {
        tstab$Value[i] <- NA
        next
      }
    }
    if(!is.na(be) && is.na(ab)){
      if(tstab$Value[i] > besup || tstab$Value[i] < beinf){
        tstab$Value[i] <- NA
        next
      }
    }
    if(!is.na(be) && !is.na(ab)){
      if((tstab$Value[i] > absup) || (tstab$Value[i] < abinf)
        || (tstab$Value[i] > besup) || (tstab$Value[i] < beinf))
      {tstab$Value[i] <- NA}
    }
  }

  # ecriture
  save(tstab, file=fileo)
  file1 <- h_gaprem_itv(file = fileo, itv0 = itv0)
  load (file1)
  save(tstab, file=fileo)
  message("File written: ",fileo,"\n")
  return(fileo)
}

