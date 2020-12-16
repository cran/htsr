#' @title Extraction of a time-series from htsr data base
#'
#' @author P. Chevallier - oct 2017 - dec 2019
#'
#' @description The function extracts a time-series in the "hts" format. It products a "tibble"
#' table with four columns : Date, Value, Station, Sensor. It is the default format of the package.
#' The function \code{\link{f_convert}} converts it in Excel or csv format.
#'
#' @param db.sqlite Full name of the data base
#' @param sta Station id.
#' @param sen Sensor id.
#' @param rtime Reduce time interval TRUE / FALSE (default)
#' @param dstart Start date YYYY-MM-DD (default: start date of the ts)
#' @param dend End date YYYY-MM-DD (default: end date of the ts)
#' @param rplot Plot the extracted file TRUE / FALSE (default)
#'
#' @seealso
#' \code{\link{ds_exp_hts}} manual settings of the parameters
#'

#'
#' @details
#' For a step by step operation the function \code{\link{ds_exp_hts}} is more convenient.
#'
#' @return The function returns:
#' \itemize{
#' \item a tibble tstab with 4 columns Date, Value, Station, Sensor
#' \item a file (nomfic) with the following name:
#' <sensor.id>_<station.id>.hts}
#'
#'
#' @examples \dontrun{
#'
#' f <- d_exp_hts("foo.sqlite","M","station","sensor")
#'
#'
#' }
#'



# fonction exp_hts

d_exp_hts <- function(db.sqlite, sta,sen,rtime=FALSE,dstart,dend, rplot=FALSE){

  # fonction u_statnom
  u_statnom <- function(db.sqlite,sta){
    conn <- dbConnect(SQLite(),db.sqlite)
    sta <- paste("'",sta,"'",sep="")
    selection <- paste ("SELECT * FROM ST WHERE Id_station =",sta)
    selection[1]
    xt <- dbGetQuery(conn, selection)
    nom <- xt$Nom[1]
    dbDisconnect(conn)
    return(nom)
  }

  # fonction u_stacapt
  u_stacapt <- function(db.sqlite,table,sta,sen){
    Valeur <- NULL
    conn <- dbConnect(SQLite(),db.sqlite)
    table1 <- paste("'",table,"'",sep="")
    sta1 <- paste("'",sta,"'",sep="")
    sen1 <- paste("'",sen,"'",sep="")
    selection <- paste ("SELECT * FROM", table1, " WHERE Id_Station =",sta1,
                        " AND Capteur =",sen1)
    x <- dbGetQuery(conn, selection)
    #  x$Date <- as.POSIXct(x$Date, format= "%Y/%m/%d %H:%M:%S")
    xt <- tibble::as_tibble(x)
    dbDisconnect(conn)
    yt <- dplyr::select(xt,Date,Valeur)
    yt <- dplyr::arrange(yt,Date)
    return(yt)
  }

# corps de fonction
  # suppressWarnings()

# initialisation
  Sys.setenv(TZ='UTC')
  conn <- dbConnect(SQLite(),db.sqlite)
  sta1 <- paste0("'",sta,"'")
  sen1 <- paste0("'",sen,"'")
  sel <- paste ("SELECT * FROM SS WHERE Id_Station =",sta1,
    " AND Capteur =",sen1)
  t <- dbGetQuery(conn,sel)
  table <- as.character(t$Tabl)
  dbDisconnect(conn)
#  if(table=="PR") op <-"S" else op <- "Mo"

# appel u_stacapt
  taa <- u_stacapt(db.sqlite, table, sta, sen)

# preparation pour rafinage
  taa$Date <- as.POSIXct(taa$Date, tz = "UTC", origin="1970-01-01")
  z <- zoo (taa$Valeur, taa$Date)
  date_start <- min(taa$Date)
  date_end <- max(taa$Date)
  if(rtime==TRUE) {
    if(dstart=="") dstart <-date_start
    if(dend=="") dend <- date_end
    z <- window (z, start = dstart, end = dend)
  }

# rafinage
  Date <- index(z)
  Value <- coredata (z)
  tab <- data.frame (Date, Value)
  tab <- tab[order(as.Date (Date)),]
  colnames(tab) <-c("Date",paste(sen,"_",sta,sep=""))
  nomfic <- paste (dirname(db.sqlite),"/",sen,"_",sta,".hts",sep="")
  tstab <- as_tibble(cbind(tab,sta,sen))
  colnames(tstab) <-c("Date","Value","Station","Sensor")
  save(tstab, file=nomfic)

# plot graphe
  if(rplot==TRUE){
    if (table=="Pluies") p_bar(nbst=1, filei=nomfic,
      serlab=sta, start=dstart, end=dend,
      title=sta, type="Value",rnorm = FALSE, rtime=FALSE, rfixy=FALSE,
      pal = "black", fct = FALSE)
  else p_line(nbst=1, filei=nomfic,serlab=sta,
      title=sta, type="value",rnorm = FALSE, rtime=FALSE, rfixy=FALSE,
      start=dstart, end=dend,
      pal="black",linet = 1, rppt = FALSE, linew=0.1, smooth = FALSE, fct = FALSE)
  }

# sortie
  write(file="",paste("File",nomfic,"extracted !"))
  return (tstab)
}


#FIN
