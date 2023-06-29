#' @title Plot bars or points
#'
#' @author P. Chevallier - Oct 2017 - Mar 2020
#'
#' @description Application of the function
#' \code{\link{p_bar}} for plotting points or bars. The resulting plot
#' can be saved as .png, .jpg or .pdf files.
#'
#' @param nbst Number of time-series to be plotted (default 1)
#' @param rpal Color palette settings 0 (default); 1 (mapalette) or 2 (manual)
#' @param savefig Save plot as png (default FALSE)
#' @param width Plot width (x100 pixels) (default = 8)
#' @param height Plot height (x100 pixels) (default = 6)
#' @param fileo Name of the plot file including extension (png, jpg or pdf) default = "plot.png")
#'
#' @seealso \code{\link{p_line}},  \code{\link{p_bar}}
#'
#' @details
#' The number of time-series to be plotted is limited to 8, with option rpal = 0, 12 otherwise.
#'
#' If savefig=TRUE, the plot is saved in the working directory. Following the
#' chosen extension, the file is formatted as .png, .jpg or .pdf. The default is
#'  "plot.png".
#'
#' @return A ggplot2 object.
#'
#' @examples \dontrun{
#'
#' p <- p_line_app(filelist = c(foo1, foo2),pset=TRUE, pfil=TRUE, rpal=1, fileo="plot23.pdf")
#' }
#'
#'




p_bar_app <- function (nbst,rpal= 0, savefig=FALSE, width= 8,
                       height= 6, fileo="plot.png") {
  # suppressWarnings()

  # initialisation
  tzo <- mapalette <- conf <- fil <- NULL
  load(file=system.file("extdata/settings.RData",package="htsr"))
  Sys.setenv(TZ=tzo)
  if (rpal != "0" && rpal != "1" && rpal != "2")  stop ("Palette choice must be 0, 1 or 2!")
  if (rpal == "0") {
    if (nbst > 8) stop("Only 8 time-series are allowed!")
    pal <- palette.colors(8, "ggplot2")
  }
  if (rpal == "2" || rpal == 1){
    if (nbst > 12) stop("Only 12 time-series are allowed!")
  }
  if (rpal == "1") pal <- mapalette

  # config graph and file
  load(file=system.file("extdata/settings.RData",package="htsr"))
  title <- as.character(conf$title[1])
  type <- as.character(conf$yaxis_label[1])
  rnorm <- as.logical(conf$norm_val[1])
  rtime <- as.logical(conf$fix_time[1])
  start <- as.POSIXct(conf$dstart[1], format="%Y-%m-%d", origin = "1970-01-01", tz = tzo)
  end <- as.POSIXct(conf$dend[1], format="%Y-%m-%d", origin = "1970-01-01", tz = tzo)
  rfixy <- as.logical(conf$fix_scale[1])
  y.down <- as.numeric(conf$ymin[1])
  y.up <- as.numeric(conf$ymax[1])
  smooth <- as.logical(conf$lin_trend[1])
  fct <- as.logical(conf$facet[1])
  filei <- as.character(fil$filename)
  serlab <- as.character(fil$series_id)
  if (rpal == "2") pal <- as.character(fil$color)

  # appel des fonctions du trace
  p <- p_bar(nbst=nbst, filei=filei,serlab=serlab,title=title,type=type,
             rnorm=rnorm,rtime=rtime,start=start,end=end,rfixy=rfixy,
             y.down=y.down,y.up=y.up, pal=pal, fct = fct)
  if(rtime == TRUE) p <- p +
    coord_cartesian(xlim = c(as.POSIXct(start, format="%Y-%m-%d", origin = "1970-01-01", tz = tzo)
                             , as.POSIXct(end, format="%Y-%m-%d", origin = "1970-01-01", tz = tzo)))
  message("Plot p created ! \n")
  if (savefig==TRUE) {
    ggsave(fileo, width=width, height=height, dpi=100)
    message("Plot saved in the file: ", fileo, "\n")
  }

  return(p)
}
# fin

