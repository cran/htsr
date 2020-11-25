#' @title Boxplot of the 12 months of a time-series.
#'
#' @author P. Chevallier - Nov 2017 -Feb 2019
#'
#' @param file File name of the time-series
#' @param title Title plot (default = Title)
#' @param axeY Title of y-axis (default  Y-axis)
#' @param savefig Save plot file TRUE / FALSE (default)
#' @param fileo Name of the plot file with extension png, jpg or pdf
#' @param width Plot width (x 100 pixels), default = 8
#' @param height Plot heights (x 100 pixels), default = 6
#'
#'
#' @return A ggplot2 object
#'
#'

p_box_month <- function (file, title="Title", axeY = "Y-axis", savefig = FALSE,
                         fileo = "plot.png", width=8, height=6) {

  tstab <- Mois <- Value <- NULL

  # constitution du fichier de trace
  mois <- c("J","F","M","A","M","J", "J","A","S","O","N","D")
  load(file)
  xm <- tstab
  xm <- dplyr::mutate(xm, Annee = lubridate::year(Date), Mois = lubridate::month(Date))
  xm <- dplyr::transmute(xm, Mois, Value)
  xm1 <- dplyr::filter(xm, !is.na(Value))
  xm1 <- dplyr::arrange(xm1, by = Mois)

  # trace
  p <- ggplot(xm1, aes(x = as.factor(Mois), y = Value, group = Mois)) + geom_boxplot(na.rm=TRUE)
  p <- p + theme(panel.background=element_rect(fill="white", colour="grey30"),
                 panel.grid.major.y=element_line(colour="grey30", linetype = "dotted"))
  p <- p + ylab(axeY) + xlab("month") +
    theme (axis.text.x = element_text(size=20),
           axis.text.y = element_text(size=20),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20),
           plot.title=element_text(face="bold", size=20))
  p <- p + ggtitle(title)
  p <- p + scale_x_discrete(labels=mois)

  if (savefig==TRUE) {
    ggsave(fileo, width=width, height=height, dpi=100)
    message("\nPlot saved in file: ", fileo, "\n")
  }

  show(p)
  return(p)
}
# FIN
