#' @title Seasonal selection
#'
#' @author P. Chevallier - Oct 2017 - Mar 2020
#'
#' @description The function provides seasonal time-series.
#'
#' @details
#' 2 to 4 seasons can be selected. For each season, the prefix sx_ where x is the season
#' is added to the file name.
#'
#' @param file Full file name to proceed
#' @param monthstart List of 2 to 4 integers (between 1 and 12) giving the starting
#' month of each season.
#'
#' @return
#' list of file names for each seasonal time-series.
#'
#' @examples \dontrun{
#'
#' files <- h_season("foo.hts", monthstart=c(3,6,9,12))
#' }
#'

# fonction ts_saison

h_season <- function(file, monthstart){

  # initialisation
  tzo <- NULL
  load(file = system.file("extdata/settings.RData",package="htsr"))
  Sensor <- Station <- Value <- saison <- NULL
  nbs <- length(monthstart)
  if (nbs<2 | nbs>4)
    return (warning("\nThe length of monthstart must be between 2 and 4."))
  for (i in 1:nbs){
    if (!(monthstart[i] %in% 1:12))
      return (warning("\nMonthstart must be integers between 1 and 12"))
  }
  dn <- dirname(file)
  bn <- basename(file)

  # lecture du fichier
  load(file)
  y <- dplyr::mutate(tstab, Annee=lubridate::year(Date),
                     Mois=lubridate::month(Date),saison=as.integer(NA))
  an_min <- min(y$Annee)
  an_max <- max(y$Annee)

  # calcul (2 saisons)
  if (nbs==2){
    for (i in 1:nrow(y))
      ifelse(y$Mois[i] >= monthstart[1] & y$Mois[i] < monthstart[2], y$saison[i] <- 1,
      y$saison[i] <- 2)
  }

  # calcul (3 saisons)
  if (nbs==3){
    for (i in 1:nrow(y))
      ifelse(y$Mois[i] >= monthstart[1] & y$Mois[i] < monthstart[2], y$saison[i] <- 1,
      ifelse(y$Mois[i] >= monthstart[2] & y$Mois[i] < monthstart[3], y$saison[i] <- 2,
      y$saison[i] <- 2))
  }

  # calcul (4 saisons)
  if (nbs==4){
    for (i in 1:nrow(y))
      ifelse(y$Mois[i] >= monthstart[1] & y$Mois[i] < monthstart[2], y$saison[i] <- 1,
      ifelse(y$Mois[i] >= monthstart[2] & y$Mois[i] < monthstart[3], y$saison[i] <- 2,
      ifelse(y$Mois[i] >= monthstart[3] & y$Mois[i] < monthstart[4], y$saison[i] <- 3,
      y$saison[i] <- 4)))
  }

  # insertion lacune en debut et fin de saison
  for (i in an_min:an_max){
    for (j in 1:nbs){
      aam <- aap <- i
      mmm <- monthstart[j] - 1
      mmp <- monthstart[j] + 1
      if (monthstart[j] == 1) {mmm <- 12 ; aam <- aam -1}
      if (monthstart[j] == 12) {mmp <- 1 ; aap <- aap + 1}
      ddm <- as.POSIXct(paste0(aam,"-",mmm,"-","28"), tz=tzo)
      ddp <- as.POSIXct(paste0(aap,"-",mmp,"-","01"), tz=tzo)
      Date <- c(ddm, ddp)
      Annee <- c(aam, aap)
      Mois <- c(mmm, mmp)
      y1 <- tibble (Date, Value = NA, Station = y$Station[1],
                    Sensor = y$Sensor[1], Annee, Mois, saison = j)
      y <- bind_rows(y, y1)
    }
  }
  y <- arrange(y, Date)

  # ecriture fichiers
  # fileo <- NA ; length(fileo) <- nbs
  fileo <- vector(mode="character", length=nbs)
  for (i in 1:nbs){
    tstab <- dplyr::filter(y, saison==i)
    tstab <- dplyr::select(tstab, Date, Value, Station, Sensor)
    fileo[i] <- paste0(dn, "/s", i, "_", bn)
    save(tstab, file=fileo[i])
  }

# retour
  message(nbs," files written:")
  return(fileo)
}

#FIN
