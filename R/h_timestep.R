#' @title Infra-daily fixed timestep
#'
#' @author P. Chevallier - Oct 2017 - June 2023
#' @description Computes a time-series with a fixed infra-daily timestep starting from an instantaneous time-series
#' - possible option: sum, mean, max or min
#'
#' @param file Instantaneous time-series
#' @param tst Timestep in minutes - must be a divisor of 1440 between 10 and 1440
#' @param op  "S", "M" (default), "Mn" ou "Mx"
#' @param shift time shift for computing daily data in hours (default = 0)
#'
#' @details
#' The op parameter give precise the chosen computation method within the interval: sum ("S"), la
#' mean ("M"), minimum ("Mn") or maximum ("Mx").
#'
#' In the case of a daily timestep (tst = 1440), the parameter shift allows to shift the time interval.
#' For example if shift = 6, the date is computed from 6am until 6am the following day. The result is
#' dated in the middle of the interval, i.e. if shift = 6; the datetime is 18.
#'
#' @return A hts time-series file with a fixed timestep. The duration of the time-step
#' in minutes is added to the file name.
#'
#' @examples \dontrun{
#'
#' f <- h_timestep(f, tst, op="S", shift = 6)
#' }
#'
#'

h_timestep <- function(file,tst,op="M", shift=0){

  #controle
  Sys.setenv(TZ='UTC')
  cas <- c("M","Mn","Mx","S")
  if(!(op %in% cas[1:4])) stop("Wrong value of op!")
  if (1440%%tst!=0 || tst <10 || tst > 1440)
    stop(warning(tst, "is not a divisor of 1440 mn or <10!"))
  shift <- trunc(shift)
  if (shift < 0 || shift > 23)
    stop(warning("the shift value must be in the interval [0-23]"))

  #initialisation
  ptm <- proc.time()
  load(file)
  y <- tstab
  sta <- y$Station[1]
  capt <- y$Sensor[1]
  nfse <- tools::file_path_sans_ext(file)
  fileo <- paste0(nfse,"_",tst,".hts")
  y <- arrange(y, Date)

  #infrajour
  date.deb <- as.numeric(y$Date[1])
  date.end <- as.numeric(y$Date[nrow(y)])
  if (tst == 1440) td <- (date.deb %/% 86400) * 86400 + 
    (shift * 3600) else td <- (date.deb %/% 86400) * 86400
  if (tst == 1440) te <- ((date.end %/% 86400) + 1) * 86400 + 
    (shift * 3600) else te <- ((date.end %/% 86400) + 1) * 86400
  te <- as.integer(te-td)
  ni <- te / (60 * tst)
  message("nb of iterations ",ni)
  yd <- as.integer(y$Date)-td
  yv <- as.numeric(y$Value)
  if (op == "S") iop <- 1
  if (op == "M") iop <- 0
  if (op == "Mn") iop <- -2
  if (op == "Mx") iop <- 2
  
  #Boucle cpp pour calcul valeur
  xv <- u_timestep (te, yd, yv, tst, iop)
  
  #Calcul date
  xd <- vector(mode="integer", length = ni)
  for (i in 1:ni) xd[i] <- td + (i-1)* tst * 60
  if (tst == 1440) xd = xd-43200
  
  #Ecriture
  x <- tibble(Date=as_datetime(xd), Value=xv)
  tstab <- mutate(x, Station = as.factor(sta), Sensor = as.factor(capt))
  save(tstab,file=fileo)
  message("Init ", as.character(as.POSIXct(td,origin="1970-1-1")),
      " End ", as.character(as.POSIXct(te+td,origin="1970-1-1")))
  message("Timestep ", tst, " minutes")
  if (op=="S") message("Sum values")
  if (op=="M") message("Mean values")
  if (op=="Mn") message("Min values")
  if (op=="Mx") message("Max values")
  texte <- proc.time()-ptm
  texte <- round(texte[1],1)
  message("Execution time : ", texte, " seconds")

  # retour
  message("File written with ", nrow(tstab), "rows.
  Can be renamed for a future use.\n")
  return(fileo)
}
