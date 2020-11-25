#' @title Backup a data base
#'
#' @author P. Chevallier - Jan 2019 / Nov 2020
#'
#' @description Back a htsr sqlite data base
#'
#' @param db.sqlite Full name of the data base

#' @return
#' A saved data base with extension .bak
#'
#'
#'# fonction backup
d_backup <- function(db.sqlite) {
  nfe  <- tools::file_ext(db.sqlite)
  nfse <- nfse <- tools::file_path_sans_ext(db.sqlite)
  bk <- paste0(nfse,".",nfe,".bak")
  conn <- dbConnect(RSQLite::SQLite(), db.sqlite)
  sqliteCopyDatabase(conn,to = bk)
  dbDisconnect(conn)
  message("\nData base backuped.")
}
