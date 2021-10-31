#' @title Scatter plot of 2 or more time-series
#'
#' @author P. Chevallier - Oct 2017-Jan 2019
#'
#' @description The reference time-series is the first of the list. The scatter plot
#' regards only the common dates of the series. In addition to the plot, a linear function
#' is adjusted forcing or not the interception by the origin.
#'
#' @param files List of file names to proceed
#' @param intercept.zero TRUE/FALSE (default) force the interception by origin
#' @param remove.zero TRUE / FALSE (default) remove the records with Value = 0
#' (e.g. precipitations)
#' @param lg.axis Legend list for axis x & y (default = NA)
#' @param title Title of the plot (default: Title)
#'
#' @return a table named "result" with 5 columns : variable name, size of the sample,
#' correlation coefficient, regression line slope, interception
#'
#' @examples \dontrun{
#'
#' result <- p_scatter(files = c("foo1.RData","foo2.RData"),
#'           intercept.zero = TRUE)
#' }
#'



p_scatter <- function (files, intercept.zero=FALSE,
                        remove.zero=FALSE, lg.axis=c(NA,NA), title="Title") {

  y <- sensor <- NULL

  files <- h_common(files)

  #initialisation
  n <- length(files)
  capsta <- as.character(NA) ; length(capsta) <- n
  for (i in 1:n){
    load(files[i])
    tstab <- dplyr::arrange(tstab,Date)
    if(NA %in% tstab$Value)
      return(warning("\nNA value is not allowed for this function in any file."))
    cast <- as.factor(paste0(tstab$Sensor,"_",tstab$Station))
    capsta[i] <- levels(cast)
    if (i==1) z <- tstab
    z <- dplyr::mutate(z,sensor=tstab$Value)
    colnames(z)[i+4] <- capsta[i]
  }

# Suppression des lignes nulles
  if (remove.zero==TRUE){
    for (i in 1:n) z <- dplyr::filter(z, capsta[i]!=0)
  }

# Constitution du fichier de trace
  for (i in 6:ncol(z)){
    x <- tibble::as_tibble(nth(z,5))
    colnames(x) <- "x"
    zz <- dplyr::mutate(x, y=nth(z,i), sensor=capsta[i-4])
    if (i == 6) xy <- zz
    else  xy <- dplyr::bind_rows(xy,zz)
  }

  #Trace
  if (is.na(lg.axis[1])) lg.axis <- c(capsta[1],"Y axis")

  p <- ggplot(xy, aes(x=x,y=y, color=sensor)) + geom_point()

  p <- p + theme(panel.background=element_rect(fill="white", colour="grey30"),
                 panel.grid.major=element_line(colour="grey30"),
                 panel.grid.minor=element_line(colour=NA))
  p <- p +
    ylab(lg.axis[2]) + xlab(lg.axis[1]) +
    theme (axis.text.x = element_text(size=20),
           axis.text.y = element_text(size=20),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20),
           plot.title=element_text(face="bold", size=20))
  p <- p +
    ggtitle(title)  +
    theme (axis.text.x = element_text(size=20),
           axis.text.y = element_text(size=20),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20),
           plot.title=element_text(face="bold", size=20))
  p <- p+ theme(legend.position="bottom") +
          theme(legend.text=element_text(size =16))
  p <- p+ labs (colour="") +
    theme(legend.title=element_text(size =16, face="bold"))

#  show(p)

  #Ajustement modele lineaire

    if (intercept.zero == FALSE) {
      p1 <- p + geom_smooth(method=lm, formula = y ~ x, fullrange=TRUE, se= FALSE)
      show(p1)
    } else {
      p1 <- p + geom_smooth(method=lm, formula = y ~ 0 + x, fullrange=TRUE, se= FALSE)
      show(p1)
    }

    captlist <- capsta[-1]
    nval <-NA ; length(nval) <- length(captlist)
    r2 <- NA ; length(r2) <- length(captlist)
    r2ad <- NA ; length(r2ad) <- length(captlist)
    coeff <-NA ; length(coeff) <- length(captlist)
    intercept <- NA ; length(intercept) <- length(captlist)

    for (i in 1:length(captlist)){
      xy1 <- dplyr::filter(xy, sensor == captlist[i])
      if (intercept.zero == FALSE) linmod <- lm(y ~ x, data = xy1)
      else  linmod <- lm(y ~ x + 0, data = xy1)
      nval[i] <- nrow(xy1)
      if (intercept.zero == FALSE) {
        intercept[i] <- linmod$coefficients[1]
        coeff[i] <- linmod$coefficients[2]
      }
      else {
        intercept[i] <- 0
        coeff[i] <- linmod$coefficients
      }
      blob <- summary(linmod)
      r2[i] <- round(blob$r.squared, 4)
      r2ad[i] <- round(blob$adj.r.squared,4)
    }

  aj <- data.frame(captlist,nval,intercept,coeff,r2,r2ad)
  aj <- as_tibble(aj)
  message ("\nLinear adjustment of the series ",capsta[1],"\n")
  return (aj)
}
# FIN
