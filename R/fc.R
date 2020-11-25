#' @title Short-cut for file.choose
#'
#' @author P. Chevallier
#'
#' @description Short-cut for file.choose
#'
#' @return A filename

fc <- function() {
  f <- file.choose()
  return(f)
}
