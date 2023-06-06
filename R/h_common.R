#' @title Extract 2 (or more) time-series on their common period
#'
#' @author P. Chevallier - Oct 2017-June 2023
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
#' f <- h_common(files = c("foo1.hts","foo2.hts"))
#' }

h_common <- function (files) {

  # fonction u_merge
  u_merge <- function(files) {
    nbser <- length(files)
    t <- NA
    for (i in 1:nbser) {
      load(files[i])
      if(i==1) t <-tstab
      else t <- rbind (t,tstab)
    }
    return(t)
  } # end u_merge

  #initialisation
  start_time <- Sys.time()
  Sensor <- Station <- Value <- capsta <- NULL
  n <- length(files)
  dn <- dirname(files[1])
  for (i in 1:n){
    nfe <- tools::file_ext(files[i])
    if (nfe!="hts")
      return(warning("\nThe file is not a hts time-series.\n"))
  }
  cast <- vector (mode="character", length= n)
  for(i in 1:n){
    load(files[i])
    cast[i]=(paste0(tstab$Sensor[1],"_",tstab$Station[1]))
  }
  z <- u_merge(files)
  z <- dplyr::mutate(z,capsta=as.factor(paste0(Sensor,"_",Station)))

# Suppression des lignes a valeurs manquantes
  z <- dplyr::filter(z, !is.na(Value)) 
  z <- dplyr::arrange(z, Date)
  nz <- nrow(z)
  x <- vector(mode = "integer", length = nz)

# appel fonction cpp u_index.cpp et renvoi d'un vecteur d'entiers
  z <- dplyr:: mutate(z, index = u_index(nz, as.integer (z$Date)))
  z <- dplyr::filter(z,index==n)

# separation des fichiers
  fileo <- NA ; length (fileo) <- n
  for (i in 1:n){
    tstab <- dplyr::filter(z, capsta == cast[i])
    tstab <- dplyr::select(tstab, Date, Value, Station, Sensor)
    fileo[i] <- paste0(dn,"/co_", basename(files[i]))
    save(tstab, file = fileo[i])
  }
  duration <- round(Sys.time() - start_time, 2)
  
# retour
  message("\n",n, " files written with ", nrow(tstab), " lines each. in ", duration, " sec\n")
  return(fileo)
}

