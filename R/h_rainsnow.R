#' @title Share the solid and liquid precipitations with a temperature criteria
#'
#' @author P. Chevallier - Oct 2017- Feb 2019
#'
#'
#' @description The precipitations are shared with a linear bevel between two temperature
#'  values
#'
#' @details The two time-series must be previously restricted to the same interval of time.
#'
#' The two temperature thresholds can be equal.
#'
#' @details The temperature time-series must be complete with no gap. Gaps are allowed
#' in the precipitation time-series.
#'
#' Is the station id is NA, the station id of the file fta is used.
#'
#' @param fpr Precipitation file name
#' @param fta Temperature file name
#' @param ta0 Low temperature threshold
#' @param ta1 High temperature threshold
#' @param sta Station id. (default = NA)
#'
#' @return 2 hts files, one with the liquid precipitation (prefix rn_) and one with
#' the solid precipitation (prefix sn_).

h_rainsnow <- function (fpr,fta,ta0,ta1,sta=NA) {

# initialisation
  Sensor <- Station <- TA <- Value <- prrain <- prsnow <- NULL
  dn <- dirname(fpr)
  bn <- basename(fpr)
  load(fta) ; zta <- tstab
  if(is.na(sta)) sta <- as.factor(zta$Station[1])
  zta <- select(zta, Date, TA = Value)
  load(fpr)
  zpr <- arrange(tstab, Date)
  date.deb <- min(zpr$Date)
  date.fin <- max(zpr$Date)
  zta <- dplyr::filter(zta, Date >= date.deb & Date <= date.fin)
  zp <- left_join(zta,zpr, by = "Date")
  zp$Station <- sta
  zp$Sensor <- as.factor(zp$Sensor)

# calcul
  zp <- mutate (zp, prsnow = ifelse(TA < ta0, Value, ifelse(TA > ta1,
    Value * 0, Value * (ta1-TA)/(ta1 - ta0))))
  zp <- mutate (zp, prrain = ifelse(TA > ta1, Value, ifelse(TA < ta0,
    Value * 0, Value * (TA-ta0)/(ta1 - ta0))))

  # ecriture fichiers
  tstab <- select(zp, Date, prsnow, Station, Sensor)
  colnames(tstab)[2] <- "Value"
  tstab$Sensor <- as.factor("Psnow")
  save(tstab, file = paste0(dn,"/sn_",bn))
  message("File written: ", paste0(dn,"/sn_",bn), "\n")
  tstab <- select(zp, Date, prrain, Station, Sensor)
  colnames(tstab)[2] <- "Value"
  tstab$Sensor <- as.factor("Prain")
  save(tstab, file = paste0(dn,"/rn_",bn))
  message("File written: ", paste0(dn,"/rn_",bn), "\n")

  return ()

}
# FIN
