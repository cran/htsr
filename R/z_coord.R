#' @title Coordinate utility
#'
#' @author P. Chevallier - Jan 2019 / Nov 2020
#'
#' @description Convert numeric coordinates in character coordinates
#'
#' @details Only one of both parameters ncoord (numeric) and ccoord (character) must be filled,
#' the other one remaining NA. The type of coordinate (Lat or Lon) is compulsory.
#'
#' @details The character coordinate must be organized in one string with 4 fields (degrees,
#' minutes, seconds, direction) separated with blanks (space or tab). Within each field,
#' no blanks are allowed to share the numeric value and the unit character. For
#' the unit character,  the only following letters are allowed: letter d/m/s.
#' For direction, the only the following letters are allowed: N/n/W/w/S/s/E/e.
#'
#' Example: "25d 18m 56.2s S"
#'
#'
#' @param ncoord Numeric coordinate
#' @param ccoord Character coordinate
#' @param type Lat / Lon

#' @return Coordinates in characters
#'
#'
#'
#'
z_coord <- function(ncoord=NA,ccoord=NA, type) {

  if (!(type %in% c("Lat","Lon")))
    return(warning("\ntype must be Lat or Lon.\n"))
  if ((is.na(ncoord) && is.na(ccoord)) || (!(is.na(ncoord)) && (!(is.na(ccoord)))))
    return(warning("\nOne of both parameters must be NA\n"))

  # numeric > character
  if (is.na(ccoord)){
    ncoord <- as.numeric(ncoord)
    if((type == "Lon") && (ncoord < -180 || ncoord > 180))
      return(warning("\nLongitude must be > -180 and < 180.\n"))
    if((type == "Lat") && (ncoord < -90 || ncoord > 90))
      return(warning("\nLongitude must be > -180 and < 180.\n"))
    if(type == "Lon") {
      if (ncoord < 0) {
        dl <- "W"
        ncoord <- -ncoord
      } else dl <- "E"
    } else{
      if (ncoord < 0) {
        dl <- "S"
        ncoord <- -ncoord
      } else dl <- "N"
    }
    deg <- as.integer(ncoord)
    mit <- as.integer((ncoord-deg)*60)
    sec <- round(as.numeric((ncoord-deg-mit/60)*3600), digits = 1)
    ccoord <- as.character(paste0(deg,"d ",mit,"m ",sec,"s ", dl))
    rt <- list(ncoord, ccoord, type)

  # character > numeric
  } else {

    x <- str_split_fixed(ccoord, " ",4)
    dl <- x[1,4]
    if (!(dl %in% c("N","n","W","w","S","s","E","e")))
      return(warning("\nNot allowed direction letter.\n"))
    x <- x[1,1:3]
    if (str_detect(x[1],"d")==FALSE)
      return(warning("\nNot allowed unit letter for degrees.\n"))
    if (str_detect(x[2],"m")==FALSE)
      return(warning("\nNot allowed unit letter for minutes.\n"))
    if (str_detect(x[3],"s")==FALSE)
      return(warning("\nNot allowed unit letter for seconds.\n"))
    x1 <- stringr::str_replace_all(x,"[:alpha:]","")
    x1 <- stringr::str_replace_all(x1,"[:blank:]","")
    deg <- as.numeric(x1[1])
    mit <- as.numeric(x1[2])
    sec <- as.numeric(x1[3])
    ncoord <- deg + mit/60 + sec/3600
    if(dl %in% c("S","s","W","w")) ncoord <- -ncoord
    rt <- list(ncoord, ccoord, type)
  }
  return(rt)
}
