#' @title Compact a data base
#'
#' @author P. Chevallier - Jan 2019
#'
#' @description Compact htsr sqlite data base
#'
#' @param fsq Full name of the data base

#' @return
#' New data base or overwritten data base. Note that the created data base is empty.
#'

d_compact <- function(fsq) {

  # Creation
  if(!file.exists(fsq)== TRUE)
    return (warning ("The base ",fsq, " doesn't exist."))
  d_backup(fsq)
  conn <- dbConnect(SQLite(), fsq)
  dbExecute(conn, "VACUUM")
  dbDisconnect(conn)
  message ("\nBase ",fsq, " compacted.")
}


