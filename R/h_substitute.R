#' @title Subtitute the missing values in a series by existing values of another series
#'
#' @author P. Chevallier - Feb 2017 - Mar 2020
#'
#' @description The series to proceed (first in file list) contents missing values or gaps
#' to be replaced by those of the second series (second in file list).
#'
#' The function only works on the common dates of both series.
#'
#' @details The output file is named with a sb_ prefix.
#'
#' @param files List of two file names
#'
#' @examples \dontrun{
#'
#' f <- h_substitute(c(f1, f2))
#' }
#'



h_substitute <- function (files) {

  #initialisation
  Value <- valeur <- NULL
  if (length(files) != 2)
    return(warning("\nThe function only works with a 2 files list.\n"))
  dn <- dirname (files[1])
  bn <- basename (files[1])



# montage des deux fichiers
  load(files[1])
  sta <- as.factor(tstab$Station[1])
  sen <- as.factor(tstab$Sensor[1])
  z1 <- dplyr::arrange(tstab,Date)
  load(files[2])
  z2 <- dplyr::arrange(tstab,Date)
  gap <- f_properties (files[1], gaps = TRUE)
  sz1 <- min(z1$Date) ; sz2 <- min(z2$Date)
  ez1 <- max(z1$Date) ; ez2 <- max(z2$Date)
  z1 <- select(z1, Date, Value)
  z2 <- select(z2, Date, Value)
  gap <- select(gap, date, valeur)
  sg <- min(gap$date) ; eg <- max(gap$date)


# boucle si données dans f2 avant données dans f1
  indic <- 0
  # if (sz2 < sz1) {
  #   k2 <- 1 ; indic <- 1
  #   z <- tibble(Date=z2$Date[1], as.numeric(Value=z2$Value[1]))
  #   repeat {
  #     k2 <- k2+1
  #     if (as_datetime(z2$Date[k2]) >= as_datetime(sz1)) break
  #     lz <- tibble(Date=z2$Date[k2], Value=z2$Value[k2])
  #     z <- bind_rows(z, lz)
  #   }
  # }

# boucle si données dans f2 et pas dans f1
  if (indic == 0) z <- tibble(Date=z1$Date[1], Value=z1$Value[1])
  k1 <- 1 ; k2 <- 1 ; kg <- 2
  repeat {
    if (kg > nrow(gap)) break
    k1 <- k1 + 1
    if (k1 > nrow(z1)) break
    dkg <- gap$date[kg]
    if (as_datetime(z1$Date[k1]) <= as_datetime(dkg)) {
      lz <- tibble(Date=z1$Date[k1], Value=z1$Value[k1])
      z <- bind_rows(z, lz)
      edk <- z1$Date[k1]
    }
    else {
      kg <- kg + 2
      if (kg > nrow(gap)) break
      dkg <- gap$date[kg]
      repeat {
        k2 <- k2+1
        if(k2 > nrow(z2)) break
        if (as_datetime(z2$Date[k2]) < as_datetime(dkg)) {
          if (as_datetime(z2$Date[k2]) <= as_datetime(edk)) next
          lz <- tibble(Date=z2$Date[k2], Value=z2$Value[k2])
          z <- bind_rows(z, lz)
        }
        else {
          kg <- kg + 1
          if (kg > nrow(gap)) break
          k2 <- k2 -1
          break
        }
      }
    }
  }

  # ecriture fichier final
  tstab <- mutate(z, Station = sta, Sensor = sen)
  fileo <- paste0(dn,"/sb_",bn)
  save(tstab, file=fileo)
  message("\nFile written: ", fileo,"\n")
#  file.remove(files)
  return (fileo)
# FIN
}





