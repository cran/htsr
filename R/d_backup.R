#' @title Backup a data base
#'
#' @author P. Chevallier - Jan 2019 / Nov 2020
#'
#' @description Back a htsr sqlite data base
#'
#' @param fsq Full name of the data base

#' @return
#' A saved data base with extension .bak
#'
#'
#'
# function backup
d_backup <- function(fsq) {
  nfe  <- tools::file_ext(fsq)
  nfse <- nfse <- tools::file_path_sans_ext(fsq)
  bk <- paste0(nfse,".",nfe,".bak")
  conn <- dbConnect(RSQLite::SQLite(), fsq)
  sqliteCopyDatabase(conn,to = bk)
  dbDisconnect(conn)
  message("\nData base backuped.")
}
