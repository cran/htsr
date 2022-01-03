#' @title Properties of a hts series
#'
#' @author P. Chevallier - Jan 2019 - Oct 2021
#'
#' @description The function provides the properties of a time-series, its duration and
#' the inventory of its gaps
#'
#' @details
#' If gaps = TRUE, a file is produced, with the same name of file and the extension .gap.
#' It contents a table with the gaps of the series and allows to build a plot with the
#' function \code{\link{p_gaps}}.
#'
#' @param file : file to be analyzed
#' @param gaps : produce a file with a table of the gaps: TRUE / FALSE (default)
#'
#' @seealso \code{\link{p_gaps}}.

#' @return Basic infos on a hts time-series
#'
#'

f_properties <- function(file, gaps = FALSE){

#initialisation
  tstab <- NULL
  nfse <- tools::file_path_sans_ext(file)
  nfe <- tools::file_ext(file)
  load(file)
  z <- tstab
  # sta <- levels(tstab$Station)[1]
  # capt <- levels(tstab$Sensor)[1]
  sta <- tstab$Station[1]
  capt <- tstab$Sensor[1]
  stacapt <- paste0(sta,"_",capt)
  dplyr::arrange(z, Date)
  date_start <- as.character(z$Date[1])
  date_end <- as.character(z$Date[nrow(z)])

# basique
  message("Time-series properties\n")
  message("----------------------\n")
  message("File: ", file, "\n")
  message("Station: ", sta, "\n")
  message("Capteur: ", capt, "\n")
  message("File init date: ", date_start, "\n")
  message("File end  date: ", date_end, "\n")
  message("Number of records: ", nrow(z), "\n")

# recherche des periodes sans lacunes
  k <- 1
  date_deb <- NA ; date_fin <- NA ; zd0 <- NA
  for (i in 1:nrow(z)){
    zv <- z$Value[i]
    zd1 <- z$Date[i]
    if (i > 1) zd0 <- z$Date[i-1]

# cas zv == NA
    if (is.na(zv)){
      if(!is.na(date_deb)){
        date_fin <- zd0
# ecriture ligne k / date_deb / date_fin
        if (k == 1) zd <- data.frame(k, date_deb, date_fin)
        else {
          zdd <- data.frame(k, date_deb, date_fin)
          zd <- rbind(zd, zdd)
        }
        k <- k+1
        date_deb <- NA
      }
    }

# cas zv != NA
    else {
      if(is.na(date_deb)) {
        date_deb <-zd1
        date_fin <- NA
      }
    }
  }

# fin de fichier si date_fin == na
  if(is.na(date_fin)) {
    date_fin <- z$Date[nrow(z)]
    if (k == 1) zd <- data.frame(k, date_deb, date_fin)
    else {
      zdd <- data.frame(k, date_deb, date_fin)
      zd <- rbind(zd, zdd)
    }
  }

# calcul des lacunes
  date_d <- zd$date_deb[1]
  date_f <- zd$date_fin[nrow(zd)]
  duree_tot <- as.numeric(date_f)- as.numeric(date_d)
  zd <- as_tibble(zd)
  zd <- dplyr::mutate(zd, duree = as.numeric(date_fin)-as.numeric(date_deb))
  duree_val <- sum(zd$duree)
  pcgap <- (1 - duree_val / duree_tot) *100
  message("\nReal init Date: ", as.character(date_d), "\n")
  message("Real end  Date: ", as.character(date_f), "\n")
  message("Total Duration: ", duree_tot/86400, "days\n")
  message("Duration without gaps: ", duree_val/86400, "days\n")
  message("Number of gaps: ", k-1, "\n")
  message("Percentage of gaps: ", round(pcgap,1), "%\n")
  if (pcgap == 0) indic <-0 else indic <- 1

# gaps table
  if (indic==1)  {
    message("\n Inventory of gaps (duration in days)\n")
    init <- zd$date_fin[1:nrow(zd)-1]
    end <- zd$date_deb[2:nrow(zd)]
    duration <- as.numeric(end) - as.numeric(init)
    duration <- round(duration/86400,2)
    gt <- data.frame(init,end, duration)
    show(gt)
  }

# inventory of gaps
  if (gaps == TRUE){
    if (indic==0) {
      date <- c(date_d, date_f)
      valeur <- c(1,1)
      ze <- tibble(date, valeur, stacapt)
    } else {
      for (i in 1:nrow(zd)){
        if (i == 1) {
          ze <- tibble(
            date = c(zd$date_deb[i],zd$date_fin[i],zd$date_fin[i]+1),
            valeur= c(1,1,NA), stacapt)
        }
        else {
          if (i != nrow(zd)) {
            zee <- tibble(
              date = c(zd$date_deb[i],zd$date_fin[i],zd$date_fin[i]+1),
              valeur=c(1,1,NA), stacapt)
            ze <- rbind(ze,zee)
          } else {
            zee <- tibble(
              date = c(zd$date_deb[i],zd$date_fin[i]),
              valeur=c(1,1), stacapt)
            ze <- rbind(ze,zee)
          }
        }
      }
    }

# create inventory file
    fileo <- paste0(nfse,".gap")
    save(file = fileo, ze)
    message("\nFile ", fileo," written")
    return(ze)
  }
  message("\n")
  return()
}
