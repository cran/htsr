#' @title Conditional extraction of a time-series regarding another one
#'
#' @author P. Chevallier - Oct 2017-Jan 2019
#'
#' @description The series to proceed is the first of the list, the conditional series
#' the second. Only the common record dates are kept.
#'
#' @details If the condition on the file 2 value is not respected, the value of
#' file 1 is changed as NA.
#'
#' @details The condition has 3 options : x< ("inf"), x>= ("sup"), < x <=
#' ("between"). In case of error or by default, "inf" is considered.
#' In the cases "inf" and "sup", only one threshold is used (thrhd1) ;
#' in the case "between", two thresholds are needed (thrhd1 < thrhd2).
#'
#' @details The output file is the name of the fist file with a cd_ prefix.
#'
#' @param files Liste de 2 file names
#' @param condition Liste 3 objects : oper ("sup" or "inf" or "between"),
#' thrhd1 < thrhd2 ; default is c("inf",0,NA)
#'
#'
#' @examples \dontrun{
#'
#' f <- h_condition(c(f1,f2), c("between", O, 2))
#' }
#'



h_condition <- function (files, condition) {

  Sensor <- Station <- Value1 <- NULL

  #initialisation
  if (length(files) != 2)
    return(warning("\nTwo files are needed in the files list.\n"))
  c1 <- as.character(condition[1])
  c2 <- as.numeric(condition[2])
  c3 <- as.numeric(condition[3])
  if(!(c1 %in% c("sup", "inf", "between")))
    return(warning("\nCondition[1] must be 'sup', 'inf' or 'between.\n"))
  if(!(typeof(c2) %in% c("integer","double")))
    return(warning("\nCondition[2] must be a numeric value.\n"))
  if((c1) == "between" && !(typeof(c3) %in% c("integer","double")))
    return(warning("\nWith 'between' condition[3] must be a numeric value.\n"))

  dn <- dirname (files[1])
  bn <- basename (files[1])

  files <- h_common(files)

# loading files
  load(files[1])
  z <- dplyr::arrange(tstab,Date)
  load(files[2])
  z <- dplyr::mutate(z, Value2=tstab$Value)

# applying condition
  if (c1 == "inf") {
    val <- ifelse(z$Value <= c2, z$Value2, NA)
    z <- dplyr::mutate(z, Value1=val)
  }
  if (c1 == "sup"){
    val <- ifelse(z$Value >= c2,z$Value2,NA)
    z <- dplyr::mutate(z, Value1=val)
  }
  if (c1 == "between"){
    val <- ifelse(z$Value>=c2 & z$Value>=c3, z$Value2,NA)
    z <- dplyr::mutate(z, Value1=val)
  }

# return
  file.remove(files)
  tstab <- dplyr::select(z, Date, Value1, Station, Sensor)
  colnames(tstab) <- c("Date","Value","Station","Sensor")
  fileo <- paste0(dn,"/cd_",bn)
  save(tstab, file=fileo)
  message("\nFile written: ", fileo,"\n")

  return (fileo)
}
# FIN
