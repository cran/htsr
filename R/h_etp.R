#' @title Compute the potential evapotranspiration with several methods
#'
#' @author P. Chevallier - April 2020
#'
#' @description ETP calculation
#'
#' @details f_temp and f_relh are mandatory in all cases.
#' @details For the Turc method, f_radg is needed.
#' @details For the Penman-Monteith method, f_atmp, f_wvel, h and z are needed. If
#' f_radn is not available, lat, f_tmin and ftmax are also needed.
#' @details The Turc method only works with a monthly frequency.
#'
#' @source  Hingray, B., Picouet, C., Musy A.,
#' Hydrologie, une science pour l'ingénieur, Presses Polytechniques et Universitaires
#' Romandes, 2008,
#' @source  Allen, R.G., L.S. Pereira, D. Raes, and M. Smith. 1998.
#' Crop Evapotranspiration. Guidelines for Computing Crop Water Requirements.
#' FAO Irrigation and Drainage Paper 56. 300p
#' @source  Er-Raki, S., A. Chehbouni,
#' S. Khabba, V. Simonneaux, L. Jarlan, A. Ouldbba, J. C. Rodriguez,
#' and R. Allen. 2010. “Assessment of Reference Evapotranspiration Methods in
#' Semi-Arid Regions: Can Weather Forecast Data Be Used as Alternate of Ground
#' Meteorological Parameters?” Journal of Arid Environments 74 (12): 1587–96.
#' https://doi.org/10.1016/j.jaridenv.2010.07.002.

#'
#' @param method Method "Turc", "Penman-Monteith", "Priestley-Taylor", "Makkink", "Heargraves-Samani"
#' @param freq Frequency "day", "month"
#' @param f_temp File of air temperature in degC, mandatory
#' @param f_tmin File of air min temperature in degC
#' @param f_tmax File of air max temperature in degC
#' @param f_relh File of relative humidity in percent, mandatory
#' @param f_radg File of global radiation in W/m2
#' @param f_radn File of net radiation in W/m2
#' @param f_atmp File of atmospheric pressure in hPa
#' @param f_wvel File of wind velocities in m/s
#' @param lat Latitude in deg
#' @param alt Altitude in m
#' @param albedo Albedo
#' @param z Anemometer high in m
#'
#' @return An hts files resulting of the operation with a name composed as:
#' @return <J or M><EtpTu>_<Station_id>.hts for the Turc method,
#' @return <J or M><EtpPM>_<Station_id>.hts for the Penman-Monteith method,
#' @return <J or M><EtpPT>_<Station_id>.hts for the Priestley-Taylor method
#' @return <J or M><EtpMa>_<Station_id>.hts for the Makkink method
#' @return <J or M><EtpHS>_<Station_id>.hts for the Heargraves-Samani method

h_etp <- function (
  method = c("Turc", "Penman-Monteith", "Priestley-Taylor", "Makkink", "Heargraves-Samani"),
  freq = c("day", "month"), f_temp, f_relh = NA, f_radg = NA, f_radn = NA, f_atmp = NA,
  f_wvel = NA, f_tmin = NA, f_tmax = NA, lat = NA, alt = NA, albedo = NA, z = NA) {


  #initialisation generale
  atmp <- delta <- ea <- es <- fracjour <- ra <- radn <- relh <- rs <- NULL
  temp <- tmax <- tmin <- wvel <- NULL
  if(freq == "day" && method %in% c("Turc")) {
    warning("The", method, "method is not convenient for a daily computation.\n")
    warning("The frequency is changed as monthly!")
    freq = "month"
  }
  if(!file.exists(f_temp) || is.na(f_temp)) stop("\nThe file f_temp is mandatory.\n")
  dn <- dirname(f_temp)

  # construction matrice de base

  #temp
  load(f_temp)
  Station_id <- as.character(tstab$Station[1])
  x <- tibble(Date=tstab$Date, temp=tstab$Value)

  # Calcul de la fraction de jour dans l'annee
  u_fracjour <- function(date){
    an <-as.character(lubridate::year(date))
    valentin <- paste0(an,"-12-31")
    dval <- as.numeric(strftime(as.Date(valentin), format = "%j"))
    jour <- lubridate::date(date)
    doy <- as.numeric(strftime(jour, format = "%j"))
    fracjour <- doy/dval
    return(fracjour)
  }
  x <- mutate(x, fracjour =u_fracjour(Date))

  #relh
  if (method %in% c("Turc", "Penman-Monteith", "Priestley-Taylor", "Makkink")){
    load(f_relh) ; y <- tibble(Date=tstab$Date, relh=tstab$Value)
    x <- left_join(x,y)
  }

  #radg
  if (method %in% c("Turc")){
    load(f_radg) ; y <- tibble(Date=tstab$Date, radg=tstab$Value)
    x <- left_join(x,y)
  }

  #ra
  if (method %in% c("Penman-Monteith", "Priestley-Taylor", "Makkink",
    "Heargraves-Samani")){

    #function u_ra
    u_ra <- function(fracjour, lat) {
      lat <- pi / 180 * lat
      dr <- 1 + (0.033 * cos(2 * pi * fracjour))
      pd <- 0.409 * sin((2 * pi * fracjour) -1.39)
      oms <- acos(-tan(lat)*tan(pd))
      ra <- 24 * 60 * 0.0820 / pi * dr * ((oms * sin(lat) * sin (pd)) + (cos(lat) * cos(pd) * sin (oms)))
      return (ra)
    }
    x <- mutate (x, ra = u_ra(fracjour, lat))
  }

  #atmp,gamma, delta, es, ea
  if (method %in% c("Penman-Monteith", "Priestley-Taylor", "Makkink")){
    load(f_atmp) ; y <- tibble(Date=tstab$Date, atmp=tstab$Value/10)
    x <- left_join(x,y)
    x <- mutate (x,
      delta = 4098*(0.6108* exp((17.27*temp)/(temp+237.3)))/((temp+237.3)^2),
      gamma = 0.665*atmp*10^-3,
      es = 0.6108*exp((17.27*temp)/(temp+237.3)),
      ea = 0.6108*exp((17.27*temp)/(temp+237.3)) * relh / 100)
  }

  #wvel
  if (method %in% c("Penman-Monteith")){
   load(f_wvel) ; y <- tibble(Date=tstab$Date, wvel=tstab$Value)
    x <- left_join(x,y)
    x$wvel <- x$wvel * 4.87 / log (67.8 * z - 5.42)
  }

  #tmin, tmax
  if (method == "Heargraves-Samani" ||
    (method %in% c("Penman-Monteith", "Priestley-Taylor") && is.na(f_radn))){
    if(freq == "month") {
      res <- h_month(f_tmin, op="M", rmna = TRUE)
      f_tmin <- as.character(res[[1]])
      res <- h_month(f_tmax, op="M", rmna = TRUE)
      f_tmax <- as.character(res[[1]])
    }
    load(f_tmin) ; y <- tibble(Date=tstab$Date, tmin=tstab$Value)
    x <- left_join(x,y)
    load(f_tmax) ; y <- tibble(Date=tstab$Date, tmax=tstab$Value)
    x <- left_join(x,y)
  }


  #rs
  if (method %in% c("Penman-Monteith", "Makkink")){
    x <- mutate (x, rs = (0.75 + (2 * 10^-5 * alt)) * ra)
  }

  #radn
  if (method %in% c("Penman-Monteith", "Priestley-Taylor")){

    # si f_radn existe
    if(!is.na(f_radn)){
      load(f_radn) ; y <- tibble(Date=tstab$Date, radn=tstab$Value)
      x <- left_join(x,y)
    } else {

    # sinon function u_rn
    u_radn <- function(ra, alt, albedo, tmin, tmax, ea) {
      rs <- (0.75 + (2 * 10^-5 * alt)) * ra
      rns <- (1 - albedo) * rs
      rnl <- 2.4515 * 10^-9 * ((tmax+273)^4 + (tmin+273)^4) * (0.34 - 0.14 * ea)
      radn <- rns - rnl
      return (radn)
    }
    x <- mutate (x, radn = u_radn(ra, alt, albedo, tmin, tmax, ea))
    }
  }

  # calcul Turc
  if(method == "Turc"){
    Value <- NA; length(Value) <- nrow(x)
    t1 <- x$temp
    for (i in 1:nrow(x)){
      if(!is.na(t1[i]) && !is.na(x$radg[i]) &&!is.na(x$relh[i])){
        if(t1[i]<0) Value [i]<- 0
        else {
          if(!is.na(t1[i])) {
            if(x$relh[i] > 50)
              Value [i]<- 0.13*((x$radg[i]*2.065)+50)*t1[i]/(t1[i]+15)
            else
              Value[i] <- 0.13*((x$radg[i]*2.065)+50)*t1[i]/(t1[i]+15)*(1+(50-x$relh[i])/70)
          }
        }
      }
    }
    etp <- transmute(x, Date, Value)
  }

  # calcul Penman-Monteith
  if(method == "Penman-Monteith"){
    etp <- transmute(x, Date, Value = ((0.408 * delta * radn) +
        (gamma * (900 / (temp + 273)) * wvel * (es- ea))) /
        (delta + (gamma * (1 + (0.34 * wvel)))))
  }

  # calcul Priestley-Taylor
  if(method == "Priestley-Taylor"){
    etp <- transmute(x, Date,
      Value = 0.408 * 1.26 * delta * radn / (delta + gamma))
  }

  # calcul Makkink
  if(method == "Makkink"){
    etp <- transmute(x, Date,
      Value = (0.408 * 0.61 * delta * rs / (delta + gamma)) - 0.12)
  }

    # calcul Heargraves-Samani
  if(method == "Heargraves-Samani"){
    etp <- transmute(x, Date,
      Value = 0.408 * 0.0023 * (temp + 17.8)* (tmax-tmin)^0.5 * ra)
  }

  # resultat
  if(freq=="day") pref <- "J" else pref <- "M"
  if(method == "Turc") sen <- paste0(pref,"EtpTu")
  if(method == "Penman-Monteith") sen <- paste0(pref,"EtpPM")
  if(method == "Priestley-Taylor") sen <- paste0(pref,"EtpPT")
  if(method == "Makkink") sen <- paste0(pref,"EtpMa")
  if(method == "Heargraves-Samani") sen <- paste0(pref,"EtpHS")

  tstab <- mutate(etp, Station = Station_id, Sensor = sen)
  fileo <- paste0(dn,"/",sen,"_",Station_id,".hts")
  save(file = fileo, tstab)
  return(message("File written ",fileo))
}


