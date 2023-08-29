#' @title Bar plot
#'
#' @author P. Chevallier - Apr 2015 - Aug 2023
#'
#' @description Bar plot based on htsr time-series. Prior to the execution of the function,
#' the parameters must be set by a function generating a "settings.RData" file in the working 
#' directory: \code{\link{z_set}} or \code{\link{ps_plothts}}.

#' @details The function deosn't have parameter. For a full description of the settings, see \code{\link{z_set}}
#'
#' @seealso \code{\link{p_line}} for plotting bars and
#'  \code{\link{z_set}} for setting the plot parameters
#'
#' @return a ggplot2 object
#'

# library(tidyverse)

p_bar <- function(){

	# settings
  fil <- tstab <- Value <- conf <- Legend <- NULL
  
	if (!file.exists ("settings.RData")) 
		warning("A function creating settings.RData in the working dir must be run before p_line()")
	
	load("settings.RData")
	
	nf <- nrow(fil)
	pal <- palette.colors(n=nf, palette = palette)
	
	# Loop for each track
	for (i in 1:nf) {
		message("Reading the file ", fil$file.names[i], "\n")
		load(fil$file.names[i])
		y <- select(tstab, Date, Value)
		
		if (conf[4])  {
			y <- filter(y, Date >= as_date(as.numeric(conf[5]))) 
			y <- filter(y, Date <= as_date(as.numeric(conf[6])))
		}
		if (nrow(y)==0)
			stop (paste("The time-series", fil$plot.label[i],"has no data.\n"))
		
		# Normalized values
		moy <- mean (y$Value, na.rm=TRUE)
		sigma <- sd (y$Value, na.rm=TRUE)
		if (conf[3]==TRUE) y$Value <- (y$Value -moy)/sigma
		
		# Building data.frame
		y <- mutate (y, Legend = as.factor(fil$plot.label[i]))
		if (i==1) x <- y else x <- bind_rows (x, y)
	}
	
  # Trace du graphe
  p <- ggplot (x, aes(x=Date, y= Value, fill=Legend)) +
      geom_bar(stat = "identity", position = "dodge", na.rm = TRUE)
  p <- p + scale_fill_manual(values=pal)

  if (conf[10]==TRUE)
  	p = p + stat_smooth(method=lm, se=FALSE)

  if (conf[11]) p = p + facet_grid (Legend ~ ., scales = "free_y")
  
  # Ecriture des labels
  p <- p + theme(panel.background=element_rect(fill="white", colour="black"),
                 panel.grid.major=element_line(colour="black"),
                 panel.grid.minor=element_line(colour=NA))
  p <- p +
    ylab(conf[2]) + xlab("") +
    ggtitle(conf[1])  +
    theme (axis.text.x = element_text(size=20),
           axis.text.y = element_text(size=20),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20),
           plot.title=element_text(face="bold", size=20))

  # Redimensionner l'ordonnee
  if(conf[7]==TRUE) p <- p + ylim(as.numeric(conf[8]),as.numeric(conf[9]))

  # Ecriture l'ordonnee
  p <- p+ theme(legend.position="bottom") +
            theme(legend.text=element_text(size =16))
  p <- p+ theme(legend.title=element_text(size =16, face="bold"))

  return(p)
}
