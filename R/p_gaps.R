#' @title Plot of data inventory
#'
#' @author P. Chevallier - Nov 2017 - Sep 2023
#'
#' @description This function plot an inventory of the data from one or several station(s)-sensor(s).
#' It is based on the .gap files provided by the function \code{\link{f_properties}}. It allows
#' to highlight the gaps in time-series.
#'
#' @details The inventories are represented with lines displayed bottom-up in the order of the files list.
#' They are labeled with the station_sensor ids.
#'
#' Colors are the default colors of ggplot2. For a black & white plot, precise BW = TRUE
#'
#' The margin value is a reserved space for writing the label at the end of each line.
#' Default value is 0.1 of the difference between the minimum and the maximum date. It shall be adjusted
#' following the length of the labels.
#'
#' @param files List of series to plot (hts files)
#' @param title Plot title, default is "Inventory"
#' @param BW Black & white plot TRUE / FALSE (default)
#' @param margin Reserved space for label writing - default is 0.1
#'
#' @seealso \code{\link{f_properties}}.
#'
#' @return A ggplot2 object
#'
#'
p_gaps <- function (files, title = "Inventory", BW = FALSE, margin = 0.1){

#	requireNamespace("directlabels", quietly = "TRUE")
	requireNamespace("directlabels")


  valeur <- stacapt <- NULL

# constitution du fichier de trace
  nbf <- length(files)
  for (i in 1:nbf) {
  	f_properties(files[i], gaps = TRUE)
  	f <- paste0(tools::file_path_sans_ext(files[i]),".gap")
    load(f)
    if (i == 1) {
      z <- ze
    } else {
      ze$valeur <- ze$valeur * i
      z <- rbind (z,ze)
    }
  }

# Trace
  xmin <- min(z$date)
  xmax <- max(z$date)
  xfin <- xmax + (xmax-xmin)*margin
  if (BW == FALSE){
    p <- ggplot(z, aes(x=date, y=valeur, colour=stacapt)) +
      geom_line(size=2)
    p <- p + scale_colour_discrete(guide = 'none')
  } else {
    p <- ggplot(z, aes(x=date, y=valeur, group=stacapt)) +
      geom_line(size=2, color = "black")
  }
  p <- p + coord_cartesian(xlim= c(xmin, xfin))
  p <- p + ggtitle(title) +
    theme(plot.title=element_text(face="bold", size=20))
  p <- p +
    theme (axis.text.x = element_text(size=20),
      axis.text.y = element_blank(),
      axis.title.x = element_text(size=20),
      axis.title.y = element_blank(),
      plot.title=element_text(face="bold", size=20),
      legend.title=element_blank(),
      legend.text=element_blank())
  p <- p + theme(panel.background=element_rect(fill="white", colour="grey30"),
        panel.grid.major.x = element_line(colour="grey30"))
  p <- p + ylim(0,nbf+1)
  p <- p + directlabels::geom_dl(aes(label = stacapt), method = list(directlabels::dl.combine("last.points"),
        cex = 1.5))

  show(p)
  return(p)
}
