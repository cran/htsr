#' @title Build a multivariable table file in csv format
#'
#' @author P. Chevallier - Jan-Feb 2022
#'
#' @param files list of hts files
#' @param daily default = TRUE
#' @param fileo name of the output file (without extension)
#' 
#' @details The function build a cvs file with values extracted from several hts files at the same date. 
#' So, it's better to run `h_common` before to apply `f_csv_multivar`
#'  
#' If daily is TRUE, only the date is taking into account, not the time.
#' 
#' @return A csv table, where the first field is a date and the next fields values 
#'
#'


# fonction

f_csv_multivar <- function(files, daily = TRUE, fileo = "fileo")
{

# tableau
  x <- tstab <- Value <- NA
  for (i in 1:length(files)){
    load (files[i])
    colab <- paste0(as.character(tstab$Station[1]),"_", as.character(tstab$Sensor[1]))
    y <- select (tstab, Date, Value)
    if (daily) y <- transmute (y, Date = lubridate::as_date(Date), Value) else 
      y <- transmute (y, Date = as.character(lubridate::ymd_hms(as_datetime(Date))), Value)
    colnames(y) <- c("Date", colab)
    if (i==1) x <- y else x <- left_join(x, y, by="Date")
  }

# ecriture
  fileo <- paste0(fileo,".csv")
  write.csv(x, file=fileo, row.names=FALSE)
  message("File ", fileo, " written")
  return(fileo)
}

#FIN
