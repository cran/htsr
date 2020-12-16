#' @title Line plot
#'
#' @author P. Chevallier - Apr 2015 - Mar 2020
#'
#' @description Line plot based on htsr time-series. The parameters can be
#' setted by \code{\link{p_line_app}}. For a step by step operation the function
#' \code{\link{ps_plothts}} is more convenient.

#' @details For a full description of the settings, see \code{\link{p_line_app}}
#'
#' @details If the number of files existing in the setting file is higher than the number of
#' processed time-series nbst, only the nbst first files are processed.
#'
#' @details If fct is TRUE, the plot is presented in facet shape, each facet corresponding
#' to a file.
#'
#' @param nbst Number of files to process
#' @param filei List of the file names to process
#' @param serlab List of the time-series labels to process
#' @param title Title of the plot
#' @param type Title of the y axis
#' @param rnorm Normalized values  (TRUE / FALSE)
#' @param rtime Reduce the plotting interval (TRUE / FALSE)
#' @param start Start date - "YYYY-MM-DD"
#' @param end End date  - "YYYY-MM-DD"
#' @param rfixy Fix the y scale (TRUE / FALSE)
#' @param y.down Min y - value
#' @param y.up Max y - value
#' @param pal List of colors
#' @param linet List of line type
#' @param linew Line size
#' @param rppt Plot the points (TRUE / FALSE)
#' @param smooth Trend fitting (TRUE / FALSE)
#' @param pointt List of point type
#' @param points Point size
#' @param fct Plot facets (TRUE / FALSE)
#'
#' @seealso \code{\link{p_bar}} for plotting bars and
#'  \code{\link{p_line_app}} for setting the plot parameters
#'
#' @return a ggplot2 object
#'
#' @examples \dontrun{
#'
#' filei <- c("foo1.xlsx","foo2.xlsx")
#' serlab <- c("station1", "station2")
#' p_line(filei, serlab)
#' }
#'
#'

p_line <- function(nbst, filei, serlab, title, type,
           rnorm, rtime, start, end, rfixy, y.down, y.up,
           pal, linet, linew,
           rppt = FALSE, pointt = NA, points = NA,
           smooth, fct){

  #Initialisation
  tzo <- tstab <- valeur <- Legend <- NULL
  Sys.setlocale(category = "LC_TIME" , locale = "en_US.UTF-8")
  load(file=system.file("extdata/settings.RData",package="htsr"))
  Sys.setenv(TZ=tzo)
  if (length(pal) < nbst) stop ("length palette must equal or higher than nbst!\n")

# Loop for each track
  for (i in 1:nbst) {

    # Building the file

    # bn.ext <- tools::file_ext(filei[i])
    # if (bn.ext != "hts") stop("The file is not an htsr file!\n")

    message("Reading the file ",filei[i], "\n")
    load(filei[i])
    y <- data.frame(tstab$Date,tstab$Value)
    colnames(y) <- c("date","valeur")
    z <- zoo(y$valeur,y$date)
    if (rtime==TRUE) z <- window (z, start = start, end = end)
    if (length(z)==0)
      stop (paste("The time-series", serlab[i],"has no data.\n"))
    ya <- data.frame(index(z),coredata(z))
    colnames(ya) <- c("date","valeur")

    # Normalized values
    moy <- mean (ya$valeur, na.rm=TRUE)
    sigma <- sd (ya$valeur, na.rm=TRUE)
    if (rnorm==TRUE) ya$valeur <- (ya$valeur -moy)/sigma

    # Building data.frame
    yb <- cbind (ya, serlab[i])
    colnames(yb) <- c("date","valeur","Legend")
    if (i==1) x <- yb else x <- rbind (x, yb)
  }

  # Plotting
  p <- ggplot (x, aes(x=date, y= valeur, colour = Legend, size = Legend, linetype = Legend)) +
    geom_line(na.rm = TRUE)
  if (fct == TRUE) p = p + facet_grid (Legend ~ ., scales = "free_y")
  p = p + scale_colour_manual(values=pal) + scale_linetype_manual(values = linet) +
    scale_size_manual(values = linew)

  # Fitting a trend
  if (smooth==TRUE)
    p = p + stat_smooth(method=lm, se=FALSE)

  # Ecriture des labels
  p <- p + theme(panel.background=element_rect(fill="white", colour="grey30"),
                 panel.grid.major=element_line(colour="grey30"),
                 panel.grid.minor=element_line(colour=NA))

  p <- p +
    ylab(type) + xlab("") +
    ggtitle(title)  +
    theme (axis.text.x = element_text(size=20),
           axis.text.y = element_text(size=20),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20),
           plot.title=element_text(face="bold", size=20))

  # Redimensionner l'ordonnee
  if(rfixy==TRUE) p <- p + ylim(as.numeric(y.down),as.numeric(y.up))

  # Ecriture de l'ordonnee
  p <- p+ theme(legend.position="bottom") +
            theme(legend.text=element_text(size =16))
  p <- p+ theme(legend.title=element_text(size =16, face="bold"))



  #Trace final
#  show(p)
  return(p)
}
