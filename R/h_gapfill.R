#' @title Simple gapfilling in a time-series
#'
#' @author P. Chevallier - Nov 2017 - Feb 2019
#'
#' @details Repalce the missing values with the mean of the last and next values
#' when the time interval is less than a number of fixed time steps.
#'
#' @param file File name to proceed
#' @param npdt Number of time-steps
#'
#' @return a time-series file with the prefix gf_

h_gapfill <- function (file, npdt){

# preparation
  ze <- NULL
  load(file)
  dn <- dirname(file)
  bn <- basename(file)
  nfse <- tools::file_path_sans_ext(file)
  f1 <- paste0(nfse,".gap")
  fileo <- paste0(dn,"/gf_",bn)
  y <- tstab
  durpdt <- (as.numeric(y$Date[2])-as.numeric(y$Date[1]))
  if (nrow(y) > 10) {
    testinter <- (as.numeric(y$Date[11])-as.numeric(y$Date[1]))
    if ((testinter / 10) != durpdt )
      return(warning("The time series doesn't have a fixed time step.\n"))
  }
  f_properties(file,gaps=TRUE)
  load(f1)
  z <- ze

# traitement lacunes
  for (i in 2:nrow(z)-1){
    if(is.na(z$valeur[i])){
      t0 <- z$date[i-1]
      t1 <- z$date[i+1]
      nb <- ((as.numeric(t1)-as.numeric(t0)) / durpdt ) - 1
      if ((as.numeric(t1)-as.numeric(t0)) <= durpdt * npdt) {
        ym <- (y$Value[y$Date==t0] + y$Value[y$Date==t1]) / 2
        for (j in 1:nb){
          y$Value[y$Date==(t0 + (j * durpdt))] <- ym
        }
      }
    }
  }

  # sauve le resultat
  tstab <- y
  save(tstab, file=fileo)
  message("File written.\n")
  return(fileo)
}
