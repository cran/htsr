#' @title Create a data base
#'
#' @author P. Chevallier - Jan 2019
#'
#' @description Create htsr sqlite data base
#'
#' @details If the data base already exists and bku is TRUE, a backup is
#' automatically generated.
#'
#' @details If cr_table is TRUE, The following tables are also created:
#' ST (stations), SS (sensors), WL (water levels), DI (discharges), PR
#' (Precipitations), WE (weather) and QU (quality)
#'
#' @param db.sqlite Full name of the data base
#' @param cr_table Create the basis tables : TRUE (default), FALSE
#' @param bku Operate a backup if db.sqlite exists : TRUE (default) / FALSE

#' @return a new data base
#'
#'

d_create <- function(db.sqlite, cr_table = TRUE, bku = TRUE){

  f <- db.sqlite

  if(file.exists(f)== TRUE) {
    if (bku==TRUE) d_backup(f)
    file.remove(f)
  }
  conn <- dbConnect(SQLite(), db.sqlite)
  dbDisconnect(conn)
  if (cr_table == TRUE) {
    d_table(db.sqlite, "ST", op = "C", bku = FALSE)
    d_table(db.sqlite, "SS", op = "C", bku = FALSE)
    d_table(db.sqlite, "WL", op = "C", bku = FALSE)
    d_table(db.sqlite, "DI", op = "C", bku = FALSE)
    d_table(db.sqlite, "WE", op = "C", bku = FALSE)
    d_table(db.sqlite, "PR", op = "C", bku = FALSE)
    d_table(db.sqlite, "QU", op = "C", bku = FALSE)
  }
  return(message("\nBase created or overwritten: ", f))
}

