#' @title Infra-daily fixed timestep
#'
#' @author P. Chevallier - Oct 2017 - Dec 2018
#' @description Compute a time-series with a fixed infra-daily timestep starting from an instantaneous time-series
#' - possible option: sum, mean, max or min
#'
#' @param file Instantaneous time-series
#' @param tst Timestep in minutes - must be a divisor of 1440 between 10 and 1440
#' @param op : "S", "M" (default), "Mn" ou "Mx"
#'
#' @details
#' The op parameter give precise the chosen computation method within the interval: sum ("S"), la
#' mean ("M"), minimum ("Mn") or maximum ("Mx").
#'
#' @return A hts time-series file with a fixed timestep. The duuration of the time-step
#' in minutes is added to the file name.
#'
#' @examples \dontrun{
#'
#' f <- t_timestep(f, tst, op="S")
#' }
#'
#'

h_timestep <- function(file,tst,op="M"){
  # suppressWarnings()

  #controle
  Sys.setenv(TZ='UTC')
  cas <- c("M","Mn","Mx","S")
  if(!(op %in% cas[1:4])) stop("Wrong value of op!")
  if (1440%%tst!=0 | tst <10 | tst > 1440)
    stop(warning(tst, "is not a divisor of 1440 mn or <10!"))

  #initialisation
  ptm <- proc.time()
  load(file)
  y <- tstab
  sta <- y$Station[1]
  capt <- y$Sensor[1]
  nfse <- tools::file_path_sans_ext(file)
  fileo <- paste0(nfse,"_",tst,".hts")
  y <- arrange(y, Date)

  # # revision des lacunes
  #   for (i in 1:nrow(tstab)){
  #     if (i == 1) next
  #     if (is.na(tstab$Valeur[i])) tstab$Date[i] <- tstab$Date[i-1]+1
  #   }

  #infrajour
  date.deb <- as.numeric(y$Date[1])
  date.end <- as.numeric(y$Date[nrow(y)])
  time.deb <- (date.deb %/% 86400) * 86400
  time.end <- ((date.end %/% 86400) + 1) * 86400
  ni <- (time.end - time.deb) / (60 * tst)
  message("nb of iterations ",ni,"\n")
  time.calc <- time.deb
  pb <- txtProgressBar(1,ni,style=3)
  i <-1
  indiclac <- TRUE

  # Boucle
  repeat {
    setTxtProgressBar(pb,i)
    time.calc0 <- time.calc
    time.calc <- time.calc0 + (60 * tst)
    if(time.calc > time.end) break
    z <- dplyr::filter(y, as.numeric(Date) > time.calc0 & as.numeric(Date) <= time.calc)
    lz <- nrow(z)
    if (lz == 0) {
      if (indiclac==FALSE) {
        if(op == "S") valeur <- 0 else valeur <- NA
      } else {
        valeur <-NA
        indiclac <- TRUE
      }
    }
    else {
      indiclac <- FALSE
      if (op=="S") valeur <- sum(z$Value)
      if (op=="M") valeur <- mean(z$Value)
      if (op=="Mn") valeur <- min(z$Value)
      if (op=="Mx") valeur <- max(z$Value)
      if (is.na(valeur)) indiclac <- TRUE
    }
    tc <- time.calc
    if (tst == 1440) tc <- tc - 43200
    if (i==1) {
      x <- tibble(Date = tc , Value = valeur)
    } else {
      x0 <- tibble(Date = tc , Value = valeur)
      x <- rbind(x, x0)
    }
    i <-i+1
  }
  message("\n")
  x$Date <- as_datetime(x$Date)
  tstab <- mutate(x, Station = as.factor(sta), Sensor = as.factor(capt))

  #Ecriture
  save(tstab,file=fileo)
  message("Init ", as.character(as.POSIXct(time.deb,origin="1970-1-1")),
      " End ", as.character(as.POSIXct(time.end,origin="1970-1-1")), "\n")
  message("Timestep ", tst, "minutes\n")
  if (op=="S") message("Sum values\n")
  if (op=="M") message("Mean values\n")
  if (op=="Mn") message("Min values\n")
  if (op=="Mx") message("Max values\n")
  texte <- proc.time()-ptm
  texte <- round(texte[1],1)
  message("Execution time : ", texte, " seconds\n")

  # retour
  message("File written with ", nrow(tstab), "rows.
Can be renamed for a future use.\n")
  tstab
  return(fileo)
}
