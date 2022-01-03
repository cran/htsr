#' @title Convert a HDSM output file in hts files for plotting
#'
#' @author P. Chevallier - October 2017 - December 2021
#'
#' @param filein HDSM output file
#' @param station station name (default="sta")
#' @param var variable name
#'
#' @details
#'
#' Three types of HDSM output files are treated ; two in blind mode : hydro_d and snow_d ;
#' one in calval mode, which includes an observed and simulated variable.
#'
#' In blind mode, the variable name must be chosen between the following ones:
#' "Psoil_km3", "Psoil_mm", "ETP_km3", "ETP_mm", "Etr_km3", "Plake_km3", "Evap_km3",
#' "Infil_km3", "Runoff_m3/s", "Drain_m3/s", "Q_m3/s", "Q_mm", "Slake_km2", "Slandice_km2",
#' "SoilLiq_mm", "dSoilLiq_km3", "VLake_km3", "dVLake_mm", "Flux_mm", "Pliq_km3", "Psol_km3",
#' "Melts_km3", "Subli_km3", "Melti_km3", "Q_km3", "Slandicekm2", "Sca_km2", "Sca_%",
#' "Swe_km3", "Swe_mm", "dSwe_km3", "SoilIce_km3", "SoilIce_mm", "dSoilIce_km3"
#'
#' The variable name is affected to the sensor of the hts file.
#'
#' In calval mode, the variable can take any value.
#'
#' The function calls the sp_plothts function, displaying graphically the hts file(s) and allowing to personalize
#' the plot.
#'
#' @seealso \code{\link{f_convert_hts2hdsm}} \code{\link{ps_plothts}}
#'
#' @return
#' In calval mode, two hts files : one "obs" and one "simul".
#'
#' In blind mode, one hts file.

f_convert_hdsm2hts <- function(filein, station = "sta", var)
{

# initialisation
  dn <- dirname(filein)
  nfse <- tools::file_path_sans_ext(filein)

  # messages
  if(tools::file_ext(filein) != "dat")
    return(warning("\nThe file extension must be dat.\n"))

  y <- as.list(read_table(filein, skip = 2, n_max=1, col_names = FALSE, cols(.default = col_character())))
  if ("obs_date" %in% y) {  # calval

    # table
    y <- read_table(filein, skip = 3, col_names = FALSE, cols(.default = col_double(),
      X1 = col_date(format ="%d/%m/%Y")))
    colnames(y) <- c("date1", "obs_date", "obs_data", "simul_data", "obs_cumul", "simul_cumul")
    y$date1 <- as_datetime(y$date1)+86400/2

    # writing files
    sen <- paste0(var, "_obs")
    tstab <- tibble (y$date1, y$obs_data, as.factor(station), as.factor(sen))
    colnames(tstab) <- c("Date", "Value", "Station", "Sensor")
    fileo1 <- paste0 (dn,"/",station,"_",sen, ".hts")
    save(file=fileo1, tstab)
    message("\nFile", fileo1, " written with ", nrow(tstab), " records")
    sen <- paste0(var, "_simul")
    tstab <- tibble (y$date1, y$simul_data, as.factor(station), as.factor(sen))
    colnames(tstab) <- c("Date", "Value", "Station", "Sensor")
    fileo2 <- paste0 (dn,"/",station,"_",sen, ".hts")
    save(file=fileo2, tstab)
    (message("File", fileo2, " written with ", nrow(tstab), " records"))
    ps_plothts(c(fileo1,fileo2))
    return()

  }
  else
  { # blind

    # headers
    y <- as.list(read_table(filein, skip = 4, n_max=1, col_names = FALSE, cols(.default = col_character())))
    cas <- 0
    if ("ETP_mm" %in% y) cas <- 1
    if ("Pliq_km3" %in% y) cas <- 2
    if (cas == 0) return( message("\nThis file cannot be processed\n"))
    if (cas == 1) {
      test <- var %in% c("Psoil_km3", "Psoil_mm", "ETP_km3", "ETP_mm", "Etr_km3", "Plake_km3", "Evap_km3",
                         "Infil_km3", "Runoff_m3/s", "Drain_m3/s", "Q_m3/s", "Q_mm", "Slake_km2", "Slandice_km2",
                         "SoilLiq_mm", "dSoilLiq_km3", "VLake_km3", "dVLake_mm", "Flux_mm")
      if (test == FALSE) return(cat("\nThe selected variable does not exist in this file.\n"))
      nomcol <- c("date1", "date2",
                "Psoil_km3", "Psoil_mm", "ETP_km3", "ETP_mm", "Etr_km3", "Plake_km3", "Evap_km3",
                "Infil_km3", "Runoff_m3/s", "Drain_m3/s", "Q_m3/s", "Q_mm", "Slake_km2", "Slandice_km2",
                "SoilLiq_mm", "dSoilLiq_km3", "VLake_km3", "dVLake_mm", "Flux_mm")
      }
    if (cas == 2) {
      test <- var %in% c("Psoil_km3", "Psoil_mm", "Pliq_km3", "Psol_km3", "Melts_km3", "Subli_km3",
                         "Melti_km3", "Q_km3", "Q_m3/s", "Slandicekm2", "Sca_km2", "Sca_%", "Swe_km3" ,
                         "Swe_mm", "dSwe_km3", "SoilIce_km3", "SoilIce_mm", "dSoilIce_km3")
      if (test == FALSE) return(cat("\nThe selected variable does not exist in this file.\n"))
      nomcol <- c("date1", "date2",
                "Psoil_km3", "Psoil_mm", "Pliq_km3", "Psol_km3", "Melts_km3", "Subli_km3",
                "Melti_km3", "Q_km3", "Q_m3/s", "Slandicekm2", "Sca_km2", "Sca_%", "Swe_km3" ,
                "Swe_mm", "dSwe_km3", "SoilIce_km3", "SoilIce_mm", "dSoilIce_km3")
      }
    headers <- read_lines (filein, skip = 3, n_max=1)
    yeardeb <- as.numeric(str_sub(headers, 62, 65))
    yearfin <- as.numeric(str_sub(headers, 68, 71))
    nbrecords <- as.numeric(lubridate::date(paste0(yearfin+1,"-01-01"))-lubridate::date(paste0(yeardeb,"-01-01")))
    varcor <- str_replace(var, "/", "p")
    fileo <- paste0(nfse,"_",varcor,".hts")

    # table
    y <- read_table(filein, skip = 6, n_max=nbrecords, col_names = FALSE, cols(.default = col_double(),
      X1 = col_date(format ="%d/%m/%Y")))
    colnames(y) <- nomcol
    y$date1 <- as_datetime(y$date1)+86400/2

    # tstab
    i <-0
    repeat {
      i <- i+1
      if (nomcol[i]== var) break
      if (i > length(nomcol)) return(cat("\nvar is not recognized\n"))
    }
    colval <- y[,i]

    # writing output
    tstab <- tibble (y$date1, as.vector(colval), as.factor(station), as.factor(var))
    colnames(tstab) <- c("Date", "Value", "Station", "Sensor")
    save(file=fileo, tstab)
    message("File", fileo, " written with ", nrow(tstab), " records")
    ps_plothts(fileo)
    return()
  }
}
#FIN
