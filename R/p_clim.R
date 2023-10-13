#' @title Plot climatologies in hydrological year
#'
#' @author P. Chevallier - Feb 2017 - Sep 2023
#'
#' @description This function processes climatology hts files created with
#' \code{\link{hs_tstep}}.
#'
#' @param files List of climatology file names
#' @param type Type: "line" (default) or bar"
#' @param hydro.month Starting month or the hydrological year (default = 1)
#' @param title Title of the plot (default = "Title")
#' @param yaxis Title of y-axis (default = "Value")
#' @param y.down Down limit of y-axis (default = NA)
#' @param y.up Up limit of y-axis (default = NA)
#' @param rpal Choice of a color palette TRUE/FALSE(default)
#' @param pal Color choice or mapalette (default)
#' @param legend.l List of text to be displayed in the plot legend (default = NA)
#'
#' @details
#'
#' The parameter tyoe allows to display a line graph or a bar graph.
#'
#' The parameter hydro.mont fixes the starting month of the hydrological year.
#'
#'  The y-axis scale can be fixed with y.down and y.up.
#'
#'  By default, the color palette is the R one. It can be change with a color
#'  list in the pal parameter or choosing mapalette (default in pal)
#'
#'  Par default station_sensor ids are displayed in the legend.l list. But it can be changed
#'  entering a list of texts in legend.l,, which must have the same length as the
#'  file number.
#'
#' @return
#' A ggplot2 object.
#'
#'

p_clim <- function (files, type="line", hydro.month=1, title="Title",
                       yaxis="Value", y.down=NA, y.up=NA, rpal=FALSE,
                       pal=mapalette, legend.l=NA){

  mapalette <- tstab <- ref <- NULL

# preparation
  if (type!="bar" & type !="line") type <- "line"
  nbst <- length(files)
  if (length (legend.l) != nbst)
    return(warning ("\nThe legend.l length must be the same as the file number.\n"))
  ms <- 1:12
  nommois <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
                 "Nov","Dec")
  nommois_ms <- nommois ; ms1 <- ms+hydro.month-1
  if (hydro.month!=1) {
    for (i in 1:12) {
      if (i+hydro.month-1<=12){
        nommois_ms[i] <- nommois[(i-1+hydro.month)]
      } else {
        nommois_ms[i] <- nommois[(i-13+hydro.month)]
      }
    }
  }

  df_clim <- NULL
  reference <- vector("list",nbst)
  for (i in 1:nbst){
    load(files[i])
    if (is.na(legend.l))
      reference[i] <- paste(tstab$Station[1],"_",tstab$Sensor[i],sep="")
    else
      reference[i] <- legend.l[i]
    mens <- tstab$Value
    mens1 <- NA ; length(mens1) <- 12
    for (j in 1:12){
      if (j+hydro.month-1<=12) mens1[j] <- mens[j+hydro.month-1]
      else mens1[j] <- mens[j+hydro.month-13]
    }
    df_clim0 <- data.frame(ms1,mens1,reference[i])
    colnames(df_clim0) <- c("ms1","mens1","ref")
    df_clim <-rbind(df_clim,df_clim0)
  }



# Trace
  if (type=="line") {
    p <- ggplot(df_clim, aes(x=(ms1), y=mens1, colour=ref))+
    geom_line(size=1)
    if (rpal==TRUE) p <- p+ scale_colour_manual(values=pal)
  } else {
    p <- ggplot(df_clim, aes(x=(ms1), y=mens1, fill=ref)) +
    geom_bar(stat="identity", position="dodge")
    if (rpal==TRUE) p <- p+ scale_fill_manual(values=pal)
  }
  p <-p + ggtitle(title) +
    theme(plot.title=element_text(face="bold", size=20))
  p <- p+ xlab("Month") + ylab(yaxis) +
    scale_x_continuous(breaks=(ms1),
      labels=nommois_ms) +
    theme (axis.text.x = element_text(size=20),
      axis.text.y = element_text(size=20),
      axis.title.x = element_text(size=20),
      axis.title.y = element_text(size=20),
      plot.title=element_text(face="bold", size=20),
      legend.position = "bottom",
      legend.title=element_text(size=0),
      legend.text=element_text(size=16))
  p <- p + theme(panel.background=element_rect(fill="white", colour="grey30"),
    panel.grid.major=element_line(colour="grey30"),
    panel.grid.minor=element_line(colour=NA))
  if(is.na(y.down)==FALSE & is.na(y.up)==FALSE) {
    p <- p + ylim(y.down,y.up) + ylab(yaxis)
  }
  show(p)
  return(p)
}
