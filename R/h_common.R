#' @title Extract 2 (or more) time-series on their common period
#'
#' @author P. Chevallier - Oct 2017-Dec 2019
#'
#' @description The fonction extract the data of 2 (or more) hts time-series
#' for the common date/time records (precision of the second).
#'
#' @param files List of file names to process.
#'
#' @return hts files resulting of the operation; their names are composed as:
#' co_<original filename>
#'
#' @examples \dontrun{
#'
#' f <- h_common(files = c("foo1.hts","foo2.hts"))
#' }
#'

h_common <- function (files) {

  # fonction u_merge
  u_merge <- function(files) {

    #Initialisation
    nbser <- length(files)
    t <- NA
    for (i in 1:nbser) {
      load(files[i])
      if(i==1) t <-tstab
      else t <- rbind (t,tstab)
    }
    return(t)
  }

  #initialisation
  Sensor <- Station <- Value <- capsta <- NULL
  n <- length(files)
  dn <- dirname(files[1])
  for (i in 1:n){
    nfe <- tools::file_ext(files[i])
    if (nfe!="hts")
      return(warning("\nThe file is not a hts time-series.\n"))
  }
  cast = NA ; length(cast) <- n

  for(i in 1:n){
    load(files[i])
    cast[i]=(paste0(tstab$Sensor[1],"_",tstab$Station[1]))
  }
  z <- u_merge(files)
  z <- dplyr::mutate(z,capsta=as.factor(paste0(Sensor,"_",Station)))

# Suppression des lignes a valeurs manquantes
  z <- dplyr::filter(z, !is.na(Value)) %>% dplyr::arrange(Date)

# Elimination de lignes non apairees
  z <- dplyr::mutate(z,index = FALSE)
  d <- z$Date
  pb <- txtProgressBar(1,nrow(z),style=3)
  for (i in 1:nrow(z)){
    setTxtProgressBar(pb,i)
    zw <- dplyr::filter(z,Date==d[i])
    if (nrow(zw)==n) z$index[i] <- TRUE
  }
  z <- dplyr::filter(z,index==TRUE)

# separation des fichiers
  fileo <- NA ; length (fileo) <- n
  for (i in 1:n){
    tstab <- dplyr::filter(z, capsta == cast[i])
    tstab <- dplyr::select(tstab, Date, Value, Station, Sensor)
    fileo[i] <- paste0(dn,"/co_", basename(files[i]))
    save(tstab, file = fileo[i])
  }

# retour
  message("\n",n, " files written with ", nrow(tstab), " lines each.\n")
  return(fileo)
}

