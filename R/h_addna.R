#' @title Add NA values within a time series
#'
#' @author P. Chevallier - November 2022
#'
#' @details The function adds records with NA in a time series at given dates. 
#' If the date already exists, the value is replaced by NA
#'
#' The output file is named with a nap_ prefix.
#'
#' @param file File name to proceed
#' @param add List of dates with NA values to be added
#'
#'
#' @examples \dontrun{
#'
#' f <- h_addna (f, add = c("2021-01-01 12:00:00 UTC", "2031-01-01 12:00:00 UTC"))
#' }
#'



h_addna <- function (file , add) {

  Sys.setenv(TZ='UTC')
  add <- as.POSIXct(add, tz="UTC")

  #initialisation
  load(file)
  dn <- dirname (file)
  bn <- basename (file)
  sta <- tstab$Station[1]
  sen <- tstab$Sensor[1]
  
  # boucle sur les dates
  for (j in 1: length(add)){
    indic <-0
    
    #cas ou la date existe
    for (i in 1:length(tstab)){
      if (tstab$Date[i]==add[j]) {
        tastab$Value[i] <- NA
        indic <-1
      }
    }
   
   #cas ou la date n'existe pas
    if (indic==0){
      x <- tibble(Date=add[j],Value=as.numeric(NA),Station=sta, Sensor=sen)
      tstab <- rbind(tstab,x)
    }
  }

#ecriture fichier
  tstab <- arrange(tstab, Date)
  fileo <- paste0(dn,"/nap_",bn)
  save(tstab, file=fileo)
  message("\nFile written: ", fileo,"\n")

  return (fileo)

}
# end
