#' @title Plot the hypsometry curve of one or more basins
#'
#' @author P. Chevallier - Sep 2017- Jan 2019
#'
#' @description Plot the hypsometry curve of one or more basins
#'
#' @param file_mnt Raster file list of elevation model of basin(s)
#' @param abbrev List of abbreviated basin name(s)
#' @param prop TRUE / FALSE (defaut) plot a proportion curve of altitude ranges
#' @param range Width of altitude range (default = 50m)
#' @param fact Exagerating factor of the areas (default=5)
#' @param title Title of the plot (default = Title)
#' @param savefig Save the plot in png (default FALSE)
#' @param width Plot width (x 100 pixels) (default = 8)
#' @param height Plot height (x 100 pixels) (default = 6)
#' @param fileo Name of plot file with extension (default = "plot.png")

#' @details
#' This function uses the "raster" library and the dependencies "sp" and "rgdal",
#' which must be installed.
#'
#' @return An object of ggplot2 class
#'
#'
# DEBUT

p_hypso <- function(file_mnt, abbrev, prop = FALSE, range=50, fact=5, title="Title", savefig=FALSE, width= 8,
                       height= 6, fileo="plot.png" ){

  altitude <- valeur <- type <- NULL

# initialisation et controle
  nbas <- length(file_mnt)
  if(length(abbrev)!=nbas)
    return(warning("\nParameters abbrev and file_mnt must have the same length."))
  for (k in 1:nbas){
    fmnt <- file_mnt[k]
    message("Basin processing: ",abbrev[k],"\n")
# lecture du raster
    mnt <- raster::raster(fmnt)
    a <- raster::getValues(mnt)
    a <- a[is.na(a)==FALSE]
    la <- length(a)
    mina <- min(a,na.rm=TRUE)
    maxa <- max(a,na.rm=TRUE)
    maxat <- maxa + range - maxa %% range
    minat <- mina - mina %% range
    maxat <- maxa + range - maxa %% range
    it <- maxat/50-minat/50-1
    alt <- as.vector(NA) ; length(alt)=it+1 ; alt[1] <- minat

# effectifs par range
    for (j in 1:it+1) alt[j] <- minat + range *(j-1)
    range <- NA ; length(range)=it
    cumul <- NA ; length(cumul)=it
    for (j in 1:it){
      range[j] <- 0 ;
      for (i in 1:la){
        if(a[i]>=alt[j] & a[i]<alt[j+1]) range[j] <- range[j]+1
      }
    }
    cumul[1] <- range[1]
    for (j in 2:it) cumul[j] <- cumul[j-1]+range[j]
    cumul <- cumul/la
    range <- (range/la)

# tableau
    alt <- alt[2:length(alt)]
    d1 <- data.frame (alt,cumul,paste0(abbrev[k],"_fr"))
    colnames(d1) <- c("altitude","valeur","type")
    if(k==1) d <- d1 else d <- rbind(d,d1)
    if(prop==TRUE){
      d2 <- data.frame (alt,range*fact,paste0(abbrev[k],"_pr"))
      colnames(d2) <- c("altitude","valeur","type")
      d <- rbind(d,d2)
    }
  }

# trace
  p <- ggplot(d) + geom_line(aes(x=altitude, y=valeur, colour=type))
  p <- p+ theme(panel.background=element_rect(fill="white", colour="grey30"),
            panel.grid.major=element_line(colour="grey30"),
            panel.grid.minor=element_line(colour=NA))
  if(prop==TRUE) p <- p + xlab("Elevation (m)") + ylab(paste0("Frequency - prop x ",fact))
  else p <- p + xlab("Elevation (m)") + ylab("Frequency")
  p <- p+ theme (axis.text.x = element_text(size=20),
            axis.text.y = element_text(size=20),
            axis.title.x = element_text(size=20),
            axis.title.y = element_text(size=20),
            plot.title=element_text(face="bold", size=20)) + ggtitle(title) +
          theme(legend.title=element_blank())
  p <- p+ theme(legend.position="bottom") +
          theme(legend.text=element_text(size =16))

# sauvegarde
  if (savefig==TRUE) {
    ggsave(fileo, width=width, height=height, dpi=100)
    message("Plot saved in the file: ", fileo, "\n")
  }

 show(p)

  return(p)
}

#FIN
