#' @title Compact a data base
#'
#' @author P. Chevallier - Jan 2019
#'
#' @description Compact htsr sqlite data base
#'
#' @param db.sqlite Full name of the data base

#' @return
#' New data base or overwritten data base. Note that the created data base is empty.
#'

d_compact <- function(db.sqlite) {

  # Creation
  if(!file.exists(db.sqlite)== TRUE)
    return (warning ("The base ",db.sqlite, " doesn't exist."))
  d_backup(db.sqlite)
  conn <- dbConnect(SQLite(), db.sqlite)
  dbExecute(conn, "VACUUM")
  dbDisconnect(conn)
  message ("\nBase ",db.sqlite, " compacted.")
}


