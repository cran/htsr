#' @title Extraction of a time-series from htsr data base
#'
#' @author P. Chevallier - oct 2017 - aug 2023
#'
#' @description The function extracts a time-series in the "hts" format. It products a "tibble"
#' table with four columns : Date, Value, Station, Sensor. It is the default format of the package.
#' The function \code{\link{f_convert}} converts it in Excel or csv format.
#'
#' @param fsq character, Full name of the data base
#' @param sta character, Station id.
#' @param sen character, Sensor id.
#' @param rtime logical, Reduce time interval (default = FALSE)
#' @param dstart character, Start date "YYYY-MM-DD" (default=NA)
#' @param dend character, End date "YYYY-MM-DD" (default: NA)
#' @param rplot logical Plot the extracted file (default = FALSE)
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
#' f <- d_exp_hts("foo.sqlite","station","sensor")
#'
#'
#' }
#'

# options(warn=-1)

# fonction exp_hts

d_exp_hts <- function(fsq, sta,sen,rtime=FALSE,dstart=NA,dend=NA, rplot=FALSE){

  # fonction u_statnom
  u_statnom <- function(fsq,sta){
    conn <- dbConnect(SQLite(),fsq)
    sta <- paste("'",sta,"'",sep="")
    selection <- paste ("SELECT * FROM ST WHERE Id_station =",sta)
    xt <- dbGetQuery(conn, selection)
    nom <- xt$Nom[1]
    dbDisconnect(conn)
    return(nom)
  }

  # fonction u_stacapt
  u_stacapt <- function(fsq,table,sta,sen){
    Valeur <- NULL
    conn <- dbConnect(SQLite(),fsq)
    table1 <- paste("'",table,"'",sep="")
    sta1 <- paste("'",sta,"'",sep="")
    sen1 <- paste("'",sen,"'",sep="")
    selection <- paste ("SELECT * FROM", table1, " WHERE Id_Station =",sta1,
                        " AND Capteur =",sen1)
    x <- dbGetQuery(conn, selection)
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
  conn <- dbConnect(SQLite(),fsq)
  sta1 <- paste0("'",sta,"'")
  sen1 <- paste0("'",sen,"'")
  sel <- paste ("SELECT * FROM SS WHERE Id_Station =",sta1,
    " AND Capteur =",sen1)
  t <- dbGetQuery(conn,sel)
  table <- as.character(t$Tabl)
  dbDisconnect(conn)
#  if(table=="PR") op <-"S" else op <- "Mo"

# appel u_stacapt
  z <- u_stacapt(fsq, table, sta, sen)
  colnames(z) <- c("Date", "Value")
  z$Date <- as_datetime(z$Date)

# preparation pour rafinage
  dstart <- as_datetime(dstart)
  dend <- as_datetime(dend)
  date_start <- as_datetime(min(z$Date))
  date_end <- as_datetime(max(z$Date))
  
  if(rtime) {
    if(is.na(dstart)) dstart <-date_start
    if(is.na(dend)) dend <- date_end
    # z <- window (z, start = dstart, end = dend)
    z <- filter(z, Date > dstart)
    z <- filter(z, Date <= dend)
  }

  nomfic <- paste (dirname(fsq),"/",sen,"_",sta,".hts",sep="")
  tstab <- mutate(z, Station = as.factor(sta), Sensor = as.factor(sen))
  save(tstab, file=nomfic)

# plot graphe
  if(rplot){
  	htsr::z_set(file.names = nomfic, plot.label = sen, title = sta)
    if (table=="PR") p <- htsr::p_bar() else p <- htsr::p_line()
    show(p)
  }

# sortie
  write(file="",paste("File",nomfic,"extracted !"))
  return (tstab)
}


#FIN
