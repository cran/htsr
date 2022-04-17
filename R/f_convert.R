#' @title Convert an hts file in another format (xls, xlsx or csv) and vice-versa
#'
#' @author P. Chevallier - October 2017 - December 2021
#'
#' @description
#' Converter in formats hts, xls, xlsx and text (csv et csv2)
#'
#' @details
#'
#' 'form_start' = csv or csv2 is for instance not accepted. It could be converted previously
#' in xls or xlsx format.
#'
#' @param file Hts file
#' @param form_start Initial format ("hts" (default) or "xls" or
#' "xlsx")
#' @param form_end Final format ("hts" or "xls" or
#' "xlsx" (default) or "csv" (separator , & decimal .)
#' or "csv2" (separator ; and decimal ,)
#'
#' @return A file in the requested format with 4 columns: Date, Value,
#' Station, Sensor
#'
#' @examples \dontrun{
#' f_convert(file,  "xlsx", "hts")
#' }
#'


# fonction ts_convert

f_convert <- function(file,  form_start="hts", form_end="xlsx")
{

# initialisation
  Date_1 <- NULL
  load(file=system.file("extdata/settings.RData",package="htsr"))
  Sys.setenv(TZ='UTC')
  ptm <- proc.time()
  nfse <- tools::file_path_sans_ext(file)
  nfe <- tools::file_ext(file)
  cas <- c("hts","xls","xlsx","csv","csv2")
  if(!(form_start %in% c(cas[1:3]))) return(warning("\nInitial format not accepted.\n"))
  if(!(form_end %in% cas[1:5])) return(warning("\nFinal format not accepted.\n"))

# form_start = hts
  if((form_start %in% cas[1:2])){
    if(!(nfe %in% cas[1:2]))
      return(warning("\nThe extension of the initial format isn't hts.\n"))
    load(file)
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
    tstab <- readxl::read_excel(file)
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

# retour
  texte <- proc.time()-ptm
  texte <- round(texte[1],1)
  message("\nExecution time: ", texte, " seconds\n")
  message("\nFile written with ", nrow(tstab), " lines\n")
  return(fileo)
}

#FIN
