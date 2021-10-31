#' @title Plot wind roses
#'
#' @author P. Chevallier - Dec 2019
#'
#' @description Plot wind roses using the "data_wind" tibble created with the
#' function \code{\link{d_wind}}.
#'
#' @details
#' For a detailed description of all parameters see \code{\link[openair]{windRose}}
#'
#' @param data_wind Name of the tibble containing the wind data
#' @param ws.int Size of speed intervals
#' @param angle Value in percent of the range unit
#' @param grid.line Value in percent of the grid line frequency
#' @param type Type of plot: "default", "year" or "month"
#' @param breaks Number of speed intervals
#' @param offset Size in percent of the central hole
#' @param paddle Shape of the basic elements: if FALSE, polar,
#' if TRUE, rectangular
#' @param key.position Position of the legend
#'
#' @seealso
#' \code{\link{d_wind}}, \code{\link[openair]{windRose}}
#'
#' @examples
#' \dontrun{
#' p_wind (data_wind = data_wind)
#' }
#'
#' @return
#' A wind rose plot

#function p_wind
p_wind <- function(data_wind, ws.int=0.5, angle=45, grid.line=10,
  type="default", breaks=5, offset=5, paddle=FALSE,
  key.position = "right"){

  coln <- colnames(data_wind)
  ws <- coln[5]
  wd <- coln[4]

  # wind rose using openair
  openair::windRose(mydata=data_wind, ws = ws, wd = wd, ws2 = NA, wd2 = NA,
    ws.int = ws.int, angle = angle, type = type, bias.corr = TRUE,
    cols = "default", grid.line = grid.line, width = 1, seg = NULL, auto.text = TRUE,
    breaks = breaks, offset = offset, normalise = FALSE, max.freq = NULL,
    paddle = paddle, key.header = NULL, key.footer = "(m/s)",
    key.position = key.position, key = TRUE, dig.lab = 5, statistic =
                      "prop.count", pollutant = NULL, annotate = FALSE, angle.scale =
                      315, border = NA)
  return()
}

