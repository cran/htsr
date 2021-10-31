#' @title Replace values with NA conditionally or in a time interval
#'
#' @author P. Chevallier - Oct 2017-Jan 2019
#'
#' @details The function replace values with NA conditionally or introduce a gap for a given interval.
#'
#' For the conditional option, the start parameter must be NA. A conditional test is applied on
#' the values (= ; > ; >= ; < ; <=) with a fixed threshold returning NA if the test is verified.
#'
#'For the gap option, the threshold parameter must be NA. All the values of the records within the
#' interval start end are replaces by NA.
#'
#' CAUTION ! At least one of both parameters threshold or start must not be NA.
#' NA.
#'
#' The output file is named with a na_ prefix.
#'
#' @param file File name to proceed
#' @param threshold Threshold value (default = NA)
#' @param test Test "=" (default);"<";"<=";">";">="
#' @param start Start date/time (included) of POSIXct class (default = NA)
#' @param end End date/time (excluded) of POSIXct class (default = NA)
#'
#'
#' @examples \dontrun{
#'
#' f <- h_nodata(f, threshold=10., test= "<=", start=NA)
#' }
#'



h_nodata <- function (file , threshold=NA, test="=", start=NA, end=NA) {

  Sys.setenv(TZ='UTC')
  Sensor <- Station <- Value <- NULL

  #verifications
  if(is.na(threshold)==TRUE & is.na(start)== TRUE)
    return(warning("\nGive a threshold value or a time interval.\n"))
  if(is.na(threshold)==FALSE & is.na(start)== FALSE)
    return(warning("\nImpossible to proceed simultaneously a threshold and a time interval.\n"))
  if(is.na(start)==FALSE & is.na(end==TRUE))
    return(warning("\nGive an end date for the time interval.\n"))
  if(!(test %in% c("=", "<", "<=", ">", ">=")))
    return(warning("\nNot allowed entry for test.\n"))

  #initialisation
  load(file)
  dn <- dirname (file)
  bn <- basename (file)

  start <- as.POSIXct(start)
  end <- as.POSIXct (end)

  #remplacement
  if (is.na(threshold)==FALSE) {
    if(test=="=" )
      tstab <- transmute(
        tstab,Date,Value=ifelse(Value==threshold,NA,Value), Station, Sensor)
    if(test=="<" )
      tstab <- transmute(
        tstab,Date,Value=ifelse(Value<threshold,NA,Value), Station, Sensor)
    if(test=="<=" )
      tstab <- transmute(
        tstab,Date,Value=ifelse(Value<=threshold,NA,Value), Station, Sensor)
    if(test==">" )
      tstab <- transmute(
        tstab,Date,Value=ifelse(Value>threshold,NA,Value), Station, Sensor)
    if(test==">=" )
      tstab <- transmute(
        tstab,Date,Value=ifelse(Value>=threshold,NA,Value), Station, Sensor)
  } else {
    tstab <- transmute(
      tstab,Date,Value=ifelse(Date>=start & Date <= end,NA,Value), Station,
      Sensor)
  }

#ecriture fichier
  fileo <- paste0(dn,"/na_",bn)
  save(tstab, file=fileo)
  message("\nFile written: ", fileo,"\n")

  return (fileo)

}
# end
