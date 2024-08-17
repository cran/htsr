#' @title Annual time series
#'
#' @author P. Chevallier - Nov 2022
#'
#' @details The function computes an annual time-series using the annual mean or the
#' annual sum of daily values. It allows the use of hydrological years. The date corresponds
#' to the middle of the year, i.e. the 182th day.
#'
#' @param file File name to proceed
#' @param mhy Starting month of the hydrological year (default = 1)
#' @param op Sum (S) or Mean (M) (default = "M")
#' @param dig Number of significant digits for Value (default = 1)
#'
#' @return The function returns a time-series of annual values.
#'


h_year <- function (file,  mhy=1, op="M", dig=1) {

  #initialisation
  Sys.setenv(TZ='UTC')
  Station <- Sensor <- NULL
  dn <- dirname (file)
  bn <- basename (file)
  bnse <- tools::file_path_sans_ext(bn)

  # controle
  annee <- Value <- NA
  load(file)
  x <- tstab
  if (op %in% c("M","S") == FALSE )
    return(warning('\nop must be "M" or "S"\n'))

  #calcul
  annee <- Value <- vector(length=nrow(x))
  x1 <- mutate (x, annee=year(x$Date), mois=month(x$Date))
  for (i in 1:nrow(x1)){
    if ((x1$mois[i] - mhy)<0) x1$annee[i] <- x1$annee[i]-1
  }

  anneemin <- min(x1$annee)
  anneemax <- max(x1$annee)
  if (mhy != 1){
    x2 <- dplyr::filter(x1, annee != anneemin)
    x2 <- dplyr::filter(x2, annee != anneemax)
  }
  else {x2 <- x1}

  x2 <- group_by(x2, annee)

  if(op=="M") x3 <- summarize(x2,mean(Value)) else
    x3 <- summarize(x2,sum(Value))
  colnames(x3) <- c("annee", "Value")
  x2 <- ungroup(x2)

  nbd <- c(1,32,60,91,121,152,182,213,244,274,305,335)
  Date0 <- as.POSIXct(paste0(x3$annee,"-01-01 00:00 UTC"), origin="1970-01-01 UTC" )
  Date <- Date0 + 86400 * (180+nbd[mhy])

  x4 <- tibble(Date, Value=x3$Value, Station=x$Station[1], Sensor=x$Sensor[1])
  x4$Value <- round(x4$Value,dig)

  #ecriture fichier
  tstab <- x4
  fileo <- paste0(dn,"/",bnse,"_Y",mhy,".hts")
  save(tstab, file=fileo)
  message("\nFile written: ", fileo,"\n")
  return (fileo)

}
# FIN
