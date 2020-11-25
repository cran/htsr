#' @title Convert an hts file in another format (xls, xlsx, csv or hdsm)
#'
#' @author P. Chevallier - octobre 2017 - novembre 20120
#'
#' @description
#' Converter in formats hts, xls, xlsx, ods, text and hdsm. The processed file
#' (except hdsm) must have 4 columns : Date, Value,
#' Station, Sensor.
#'
#' @details
#'
#' 'form_start' = csv or csv2 is for instance not accepted. It could be converted previously
#' in xls or xlsx format.
#'
#' 'form_start' = hdsm is for instance experimental
#'
#' In the hdsm case, one must precise a station, a sensor, a phase
#' (output: liquid or solid) and a variable to be extracted
#'
#' If output = "liquid", the available variables are:
#' "Psoil_km3", "Psoil_mm", "ETP_km3", "ETP_mm", "Etr_km3",
#' "Plake_km3", "Evap_km3", "Infil_km3", "Runoff_m3/s", "Drain_m3/s", "Q_m3/s",
#' "Q_mm", "Slake_km2", "Slandice_km2", "SoilLiq_mm", "dSoilLiq_km3", "VLake_km3",
#' "dVLake_mm", "Flux_mm"
#' If output = "solid", the available variables are:
#' "Psoil_km3", "Psoil_mm", "Pliq_km3", "Psol_km3",
#' "Melts_km3", "Subli_km3", "Melti_km3", "Q_km3", "Q_m3/s", "Slandicekm2",
#' "Sca_km2", "Sca_<charactere pourcent>", "Swe_km3", "Swe_mm", "dSwe_km3",
#' "SoilIce_km3", "SoilIce_mm", "dSoilIce_km3"
#'
#'
#' @param f Hts file
#' @param form_start Initial format ("hts" (default) or "xls" or
#' "xlsx" or "hdsm")
#' @param form_end Final format ("hts" or "xls" or
#' "xlsx" (default) or "hdsm") or "csv" (separator , & decimal .)
#' or "csv2" (separator ; and decimal ,)
#' @param sta Station name [case hdsm]
#' @param sen Sensor name [case hdsm]
#' @param output Phase liquid (default) or solid [case hdsm]
#' @param variable Variable [case hdsm]
#'
#'
#' @return A table in the requested format with 4 columns: Date, Value,
#' Station, Sensor
#'
#'
#' @examples \dontrun{
#' f <- f_convert(f, "xlsx", "hts")
#' }
#'
#'

# fonction ts_convert

f_convert <- function(f, form_start="hts", form_end="xlsx", sta = NA, sen = NA, output = NA, variable = NA){
  # suppressWarnings()

# initialisation
  Date_1 <- NULL
  load(file=system.file("extdata/settings.RData",package="htsr"))
  Sys.setenv(TZ='UTC')
  ptm <- proc.time()
  nfse <- tools::file_path_sans_ext(f)
  nfe <- tools::file_ext(f)
  cas <- c("hts","xls","xlsx","csv","csv2", "hdsm")
  if(!(form_start %in% c(cas[1:3], cas[6])))
    return(warning("\nInitial format not accepted.\n"))
  if(!(form_end %in% cas[1:7]))
    return(warning("\nFinal format not accepted.\n"))

# form_start = hts
  if((form_start %in% cas[1:2])){
    if(!(nfe %in% cas[1:2]))
      return(warning("\nThe extension of the initial format isn't hts.\n"))
    load(f)
    tstab <- as_tibble(tstab)
    if (form_end =="hts"){
      fileo <- paste0(nfse,".ts")
      save(tstab, file = fileo)
    }
    if(form_end == "xls" | form_end == "xlsx"){
      fileo <- paste0(nfse,".xlsx")
      if (form_end=="xls"){
        if (nrow(tstab)>65335) warning("\nToo much lines! Writing in xlsx.")
        else fileo <- paste0(nfse,".xls")
      }
      tstab$Date <- as.numeric(tstab$Date) /86400 + 25569
      WriteXLS::WriteXLS(tstab,ExcelFileName=fileo,row.names=FALSE)
    }
    if(form_end == "csv"){
      fileo <- paste0(nfse,".csv")
      tstab$Date <- as.character.POSIXt(tstab$Date)
      write.csv(tstab, file=fileo, row.names=FALSE)
    }
    if(form_end == "csv2"){
      fileo <- paste0(nfse,".csv")
      tstab$Date <- as.character.POSIXt(tstab$Date)
      write.csv2(tstab, file=fileo, row.names=FALSE)
    }
  }

  # form_start = Excel
  if (form_start == "xls" | form_start == "xlsx") {
    if(nfe != "xls" & nfe != "xlsx")
      return(warning("\nThe initial file isn't an Excel file.\n"))
    tstab <- readxl::read_excel(f)
 #   tstab$Date <- as.POSIXct((tstab$Date - 25569) * 86400, origin = "1970-01-01", tz = "UTC")
    if (form_end =="hts"){
      fileo <- paste0(nfse,".hts")
      tstab <- as_tibble(tstab)
      save(tstab, file = fileo)
    }
    if(form_end == "csv"){
      fileo <- paste0(nfse,".csv")
      tstab$Date <- as.character(tstab$Date)
      write.csv(tstab, file=fileo, row.names=FALSE)
    }
    if(form_end == "csv2"){
      fileo <- paste0(nfse,".csv")
      tstab$Date <- as.character(tstab$Date)
      write.csv2(tstab, file=fileo, row.names=FALSE)
    }
  }

  # form_start = hdsm
  if (form_start == "hdsm"){
    if(is.na(sta)||is.na(sen)||is.na(variable)||is.na(output))
      warning("\nIn the hds case, station, capteur, phase and variable must be given")
    if(is.na(sta)) sta <- readline("Station: ")
    if(is.na(sen)) sen <- readline("Sensor: ")
    if(is.na(output)) output <- readline("Phase (liquid/solid: ")
    if(is.na(variable)) variable <- readline("Variable : ")
    if((!(output %in% c("liquid", "solid"))))
      return(warning("\nThe phase can only be liquid or solid!\n"))
    if(output == "liquid") {
      ll <- c("Psoil_km3", "Psoil_mm",
        "ETP_km3", "ETP_mm", "Etr_km3", "Plake_km3", "Evap_km3", "Infil_km3",
        "Runoff_m3/s", "Drain_m3/s", "Q_m3/s", "Q_mm", "Slake_km2", "Slandice_km2",
        "SoilLiq_mm", "dSoilLiq_km3", "VLake_km3", "dVLake_mm",
        "Flux_mm")
      if ((variable %in% ll)) {
        for (i in 1:length(ll)) {if (ll[i] == variable) k <- i}
        ll[9:11] <- c("Runoff_m3_s", "Drain_m3_s", "Q_m3_s")
      }
      else return(warning("\nVariable", variable, "not allowed!\n"))
    }
    if(output == "solid"){
      ll <- c("Psoil_km3", "Psoil_mm",
        "Pliq_km3", "Psol_km3", "Melts_km3", "Subli_km3", "Melti_km3", "Q_km3",
        "Q_m3/s", "Slandicekm2", "Sca_km2", "Sca_%", "Swe_km3", "Swe_mm",
        "dSwe_km3", "SoilIce_km3", "SoilIce_mm", "dSoilIce_km3")
      if((variable %in% ll)) {
        for (i in 1:length(ll)) {if (ll[i] == variable) k <- i}
        ll[9] <- "Runoff_m3_s"
      }
      else return(warning("\nVariable", variable, "not allowed!\n"))
    }
    if(output == "liquid") x <- read_table(f, skip = 6, n_max = length(read_lines(f))-16,
      col_names = c("Date_1", "Date_2", ll))
    if(output == "solid") x <- read_table(f, skip = 6, n_max = length(read_lines(f))-16,
      col_names = c("Date_1", "Date_2", ll))
    x$Date_1 <- as.POSIXct(x$Date_1, origin = "1970-01-01", format = "%d/%m/%Y", tz = "UTC") + 43200
    tstab <- select(x, Date = Date_1, Value = ll[k])
    tstab <- mutate(tstab, Station = as.factor(sta), Sensor = as.factor(sen))
    if (form_end =="hts"){
      fileo <- paste0(nfse,".hts")
      tstab <- as_tibble(tstab)
      save(tstab, file = fileo)
    }
    if(form_end == "csv"){
      fileo <- paste0(nfse,".csv")
      write.csv(tstab, file=fileo, row.names=FALSE)
    }
    if(form_end == "csv2"){
      fileo <- paste0(nfse,".csv")
      write.csv2(tstab, file=fileo, row.names=FALSE)
    }
    if(form_end == "xls" | form_end == "xlsx"){
      fileo <- paste0(nfse,".xlsx")
      if (form_end=="xls"){
        if (nrow(tstab)>65335) warning("\nToo much lines! Writing in xlsx.")
        else fileo <- paste0(nfse,".xls")
      }
      WriteXLS::WriteXLS(tstab,ExcelFileName=fileo,row.names=FALSE)
    }
  }

# retour
  texte <- proc.time()-ptm
  texte <- round(texte[1],1)
  message("\nExecution time: ", texte, " seconds\n")
  message("\nFile written with ", nrow(tstab), " lines\n")
  return(fileo)
}



#FIN
