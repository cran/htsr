#' @title Convert hts files in HDSM input format
#'
#' @author P. Chevallier - October 2017 - December 2021
#'
#' @description
#' Converter from hts files to HDSM input time series
#'
#' @param files List of files
#' @param freqs List of time frequencies
#' @param units List of unit of the time-series
#' @param unit_factors List of unit of the time-series
#' @param fileo  name of the output file (whithout extension)
#'
#' @details
#' The output file is stored in the directory of the first file in the files list.
#'
#' freqs is a list with the time frequency of each selected time series. It can be "d" (daily),
#' "8d" (8 days) or "m" (monthly).
#'
#' units is a list with the unit of each selected time series and unit_factors allow to modify the original
#' units of the hts files. If NA (default), factors are equal to 1.
#'
#' @seealso \code{\link{f_convert_hdsm2hts}}
#'
#' @return
#' A text file to be used in the HDSM input section (calval).

f_convert_hts2hdsm <- function(files, freqs, units, unit_factors, fileo = "calibration_data")
{

# initialisation
  dn <- dirname(files[1])
  fileo <- paste0(dn, "/",fileo,".dat")
  Value <- tstab <- NULL

# messages
  if(length(freqs) != length(files)) return(message("freqs must have the same length as files!"))
  if(length(units) != length(files)) return(message("units must have the same length as files!"))
  if (length (unit_factors) != length(files))
      return(message("unit_factors must have the same length as files!"))
  for (i in 1:length(freqs)) if (!(freqs[i] %in% c("d", "m", "8d")))
    return(message("freqs must be d, m or 8d"))

# day
  message ("\nDay processing")
  if("d" %in% freqs){
    dayfiles <- dayunits <- vector(mode="character")
    factorunits <- vector(mode="numeric")
    for (i in 1:length(files)) if (freqs[i] == "d") {
      dayfiles <- c(dayfiles, files[i])
      dayunits <- c(dayunits, units[i])
      factorunits <- c(factorunits, unit_factors[i])
    }
    dmin <- dmax <- vector(mode="numeric", length = length(dayfiles))

    #time basis
    for (i in 1:length(dayfiles)) {
      nfe <- tools::file_ext(dayfiles[i])
      if (nfe != "hts") return(cat(dayfiles[i], "isn't an hts file n!"))
      load(dayfiles[i])
      dmin[i] <- min(as.numeric((tstab$Date)))
      dmax[i] <- max(as.numeric((tstab$Date)))
    }
    ddeb <- min(dmin)
    dfin <- max(dmax)
    ydeb <- year(as_datetime(ddeb))
    yfin <- year(as_datetime(dfin))
    jourdeb <- paste0(ydeb,"-01-01 12:00:00 UTC")
    jourfin <- paste0(yfin,"-12-31 12:00:00 UTC")
    jour <- as_datetime(jourdeb)
    x <- tibble(Date=jour)
    repeat {
      if (jour == as_datetime(jourfin)) break
      jour <- jour + 86400
      x <- add_row(x, Date=jour)
    }

    #table
    for (i in 1:length(dayfiles)) {
      load(dayfiles[i])
      x1 <- tstab
      x1 <- select (x1, Date, Value)
      colnames(x1) <- c("Date", paste0(tstab$Station[1],"_",tstab$Sensor[1]))
      x <- left_join(x, x1)
    }

    # writing file
    ligne <- "#  daily data -----------"
    write(file = fileo, ligne)
    ligne <- paste0("       ",nrow(x),"                    ",length(dayfiles))
    write(file = fileo, ligne, append = TRUE)
    ligne <- "           "
    for (i in 1:length(dayfiles)) ligne <- paste0(ligne,"'",colnames(x)[i+1],"'    ")
    write(file = fileo, ligne, append = TRUE)
    ligne <- "#          "
    for (i in 1:length(dayfiles)) ligne <- paste0(ligne,dayunits[i],"     ")
    write(file = fileo, ligne, append = TRUE)
    for (i in 1:nrow(x)) {
      xdate <- x$Date[i]
      dd <- as.character(day(xdate))
      if(str_length(dd)==1) dd <- str_c("0",dd)
      mm <- as.character(month(xdate))
      if(str_length(mm)==1) mm <- str_c("0",mm)
      ddmmyyyy <- str_c(dd,"/",mm,"/",as.character(year(xdate)))
      ligne <- ddmmyyyy
      for (j in 1:length(dayfiles)){
        val <- as.numeric(x[i,j+1]) * factorunits[j]
        if (is.na(val)) val <- -999
        ligne <- paste0(ligne, "             ", format(val,nsmall=3,width=8))
      }
      write(file = fileo, ligne, append = TRUE)
    }
  } else {
    ligne <- "#  daily data -----------"
    write(file = fileo, ligne)
    ligne <- paste0("       ",0,"                    ",0)
    write(file = fileo, ligne, append = TRUE)
    ligne <- "           "
#    for (i in 1:length(dayfiles)) ligne <- paste0(ligne,"'",colnames(x)[i+1],"'    ")
    write(file = fileo, ligne, append = TRUE)
    ligne <- "#          "
#    for (i in 1:length(dayfiles)) ligne <- paste0(ligne,dayunits[i],"     ")
    write(file = fileo, ligne, append = TRUE)

  }

  # monthly
  message ("\nMonth processing")
  if("m" %in% freqs){
    monthfiles <- monthunits <- vector(mode="character")
    factorunits <- vector(mode="numeric")
    for (i in 1:length(files)) if (freqs[i] == "m") {
      monthfiles <- c(monthfiles, files[i])
      monthunits <- c(monthunits, units[i])
      factorunits <- c(factorunits, unit_factors[i])
    }
    dmin <- dmax <- vector(mode="numeric", length = length(monthfiles))

    #time basis
    for (i in 1:length(monthfiles)) {
      nfe <- tools::file_ext(monthfiles[i])
      if (nfe != "hts") return(cat(monthfiles[i], "isn't an hts file n!"))
      load(monthfiles[i])
      dmin[i] <- min(as.numeric((tstab$Date)))
      dmax[i] <- max(as.numeric((tstab$Date)))
    }
    ddeb <- min(dmin)
    dfin <- max(dmax)
    ydeb <- year(as_datetime(ddeb))
    yfin <- year(as_datetime(dfin))
    jourdeb <- paste0(ydeb,"-01-16 00:00:00 UTC")
    jourfin <- paste0(yfin,"-12-16 00:00:00 UTC")
    jour <- as_datetime(jourdeb)
    x <- tibble(Date=jour)
    repeat {
      if (jour == as_datetime(jourfin)) break
      mois <- month(jour) + 1
      annee <- year(jour)
      if (mois == 13) {
        mois <- 1
        annee  <- annee+1
      }
      jour <- as_datetime(paste0(annee,"-",mois, "-16 00:00:00 UTC"))
      x <- add_row(x, Date=jour)
    }

    #table
    for (i in 1:length(monthfiles)) {
      load(monthfiles[i])
      x1 <- tstab
      x1 <- select (x1, Date, Value)
      colnames(x1) <- c("Date", paste0(tstab$Station[1],"_",tstab$Sensor[1]))
      x <- left_join(x, x1)
    }

      # writing file
    ligne <- "#  monthly data -----------"
    write(file = fileo, ligne, append = TRUE)
    ligne <- paste0("       ",nrow(x),"                    ",length(monthfiles))
    write(file = fileo, ligne, append = TRUE)
    ligne <- "           "
    for (i in 1:length(monthfiles)) ligne <- paste0(ligne,"'",colnames(x)[i+1],"'    ")
    write(file = fileo, ligne, append = TRUE)
    ligne <- "#          "
    for (i in 1:length(monthfiles)) ligne <- paste0(ligne,monthunits[i],"     ")
    write(file = fileo, ligne, append = TRUE)
    for (i in 1:nrow(x)) {
      xdate <- x$Date[i]
      # dd <- as.character(day(xdate))
      # if(str_length(dd)==1) dd <- str_c("0",dd)
      dd <- "15"
      mm <- as.character(month(xdate))
      if(str_length(mm)==1) mm <- str_c("0",mm)
      ddmmyyyy <- str_c(dd,"/",mm,"/",as.character(year(xdate)))
      ligne <- ddmmyyyy
      for (j in 1:length(monthfiles)){
        val <- as.numeric(x[i,j+1]) * factorunits[j]
        if (is.na(val)) val <- -999
        ligne <- paste0(ligne, "             ", format(val,nsmall=3,width=8))
      }
      write(file = fileo, ligne, append = TRUE)
    }
  } else {
    ligne <- "#  monthly data -----------"
    write(file = fileo, ligne, append = TRUE)
    ligne <- paste0("       ",0,"                    ",0)
    write(file = fileo, ligne, append = TRUE)
    ligne <- "           "
    # for (i in 1:length(monthfiles)) ligne <- paste0(ligne,"'",colnames(x)[i+1],"'    ")
    write(file = fileo, ligne, append = TRUE)
    ligne <- "#          "
    # for (i in 1:length(monthfiles)) ligne <- paste0(ligne,monthunits[i],"     ")
    write(file = fileo, ligne, append = TRUE)
  }


  # 8days
  message ("\n8days processing")
  if ("8d" %in% freqs){
    eightdfiles <- eightdunits <- vector(mode="character")
    factorunits <- vector(mode="numeric")
    for (i in 1:length(files)) if (freqs[i] == "8d") {
      eightdfiles <- c(eightdfiles, files[i])
      eightdunits <- c(eightdunits, units[i])
      factorunits <- c(factorunits, unit_factors[i])
    }
    dmin <- dmax <- vector(mode="numeric", length = length(eightdfiles))

    #time basis
    for (i in 1:length(eightdfiles)) {
      nfe <- tools::file_ext(eightdfiles[i])
      if (nfe != "hts") return(cat(eightdfiles[i], "isn't an hts file n!"))
      load(eightdfiles[i])
      dmin[i] <- min(as.numeric((tstab$Date)))
      dmax[i] <- max(as.numeric((tstab$Date)))
    }
    ddeb <- min(dmin)
    dfin <- max(dmax)
    ydeb <- year(as_datetime(ddeb))
    yfin <- year(as_datetime(dfin))
    jourdeb <- paste0(ydeb,"-01-01 12:00:00 UTC")
    jourfin <- paste0(yfin,"-12-31 12:00:00 UTC")
    jour <- as_datetime(jourdeb)
    x <- tibble(Date=jour)
    annee0 <- year(jour)
    repeat {
      jour <- jour + 8* 86400
      annee1 <- year(jour)
      if (annee1 > yfin) break
      if (annee1 != annee0)
        jour <- as_datetime(paste0(annee1,"-01-01 12:00:00 UTC"))
      x <- add_row(x, Date=jour)
      annee0 <- year(jour)
    }

    #table
    for (i in 1:length(eightdfiles)) {
      load(eightdfiles[i])
      x1 <- tstab
      x1 <- select (x1, Date, Value)
      colnames(x1) <- c("Date", paste0(tstab$Station[1],"_",tstab$Sensor[1]))
      x <- left_join(x, x1)
    }

    # writing file
    ligne <- "#  8days data -----------"
    write(file = fileo, ligne, append = TRUE)
    ligne <- paste0("       ",nrow(x),"                    ",length(eightdfiles))
    write(file = fileo, ligne, append = TRUE)
    ligne <- "           "
    for (i in 1:length(eightdfiles)) ligne <- paste0(ligne,"'",colnames(x)[i+1],"'    ")
    write(file = fileo, ligne, append = TRUE)
    ligne <- "#          "
    for (i in 1:length(eightdfiles)) ligne <- paste0(ligne,eightdunits[i],"     ")
    write(file = fileo, ligne, append = TRUE)
    for (i in 1:nrow(x)) {
      xdate <- x$Date[i]
      dd <- as.character(day(xdate))
      if(str_length(dd)==1) dd <- str_c("0",dd)
      mm <- as.character(month(xdate))
      if(str_length(mm)==1) mm <- str_c("0",mm)
      ddmmyyyy <- str_c(dd,"/",mm,"/",as.character(year(xdate)))
      ligne <- ddmmyyyy
      for (j in 1:length(eightdfiles)){
        val <- as.numeric(x[i,j+1])* factorunits[j]
        if (is.na(val)) val <- -999
        ligne <- paste0(ligne, "             ", format(val,nsmall=3,width=8))
      }
      write(file = fileo, ligne, append = TRUE)
    }
  } else {
    ligne <- "#  8days data -----------"
    write(file = fileo, ligne, append = TRUE)
    ligne <- paste0("       ",0,"                    ",0)
    write(file = fileo, ligne, append = TRUE)
    ligne <- "           "
    # for (i in 1:length(eightdfiles)) ligne <- paste0(ligne,"'",colnames(x)[i+1],"'    ")
    write(file = fileo, ligne, append = TRUE)
    ligne <- "#          "
    # for (i in 1:length(eightdfiles)) ligne <- paste0(ligne,eightdunits[i],"     ")
    write(file = fileo, ligne, append = TRUE)
  }
  return (message("\nFile", fileo, " written "))

}
#FIN
