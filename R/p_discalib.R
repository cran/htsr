#' @title Plot calibration curves water levels vs discharges
#'
#' @author P. Chevallier - Sep 2017 - Dec 2020
#'
#' @description
#' Experimental function, which is for instance limited to only two calibration curves on the same plot.
#'
#' The function plot the discharges measurements and the corresponding
#' calibration curves starting.
#'
#' Only the "active" discharge measurements are plotted. The parameter plotdism displays
#' them or not.
#'
#' One can zoom on a subpart of the plot using the limit values on the x and y axis.
#'
#' The savefig (default = FALSE by default) parameter allows to save the result i a  png, jpg or pdf
#' file, according to the extension of fout.
#'
#' @param fsq Data base file name
#' @param sta Station Id.
#' @param sen Sensor Id. (default = "IH")
#' @param plotcalib Plot calibrations TRUE (default) / FALSE
#' @param plotdism Plot discharge measurements TRUE (default) / FALSE
#' @param title Plot title (default: Title)
#' @param savefig Save plot in a png file TRUE (default) / FALSE
#' @param width Plot width (x 100 pixels) (default = 8)
#' @param height Plot height (x 100 pixels) (default = 6)
#' @param fout Plot file name (default = "plot.png")
#' @param limx Limit x axis TRUE / FALSE (default)
#' @param limy Limit y axis TRUE / FALSE (default)
#' @param xinf Low value for x (default = NA)
#' @param xsup High value for x (default = NA)
#' @param yinf Low value for y (default = NA)
#' @param ysup High value for y (default = NA)
#'
#'
#'

p_discalib <- function (fsq, sta, sen = "IH", plotcalib= TRUE, plotdism=TRUE,
  title="Title", savefig=FALSE, width= 8, height= 6, fout="plot.png",
  limx =FALSE, limy = FALSE, xinf=NA, xsup=NA, yinf=NA, ysup=NA) {

  u_exp_discalib <- function(fsq, sta, calib=TRUE, dism=TRUE) {

    # initialisation
    tzo <- NULL
    load(file=system.file("extdata/settings.RData",package="htsr"))
    if(calib==FALSE & dism==FALSE)
      return(warning("\nAt least one between calib and dism must be TRUE."))
    Id_Station <- Capteur <- Date <- H <- Q <- Active <- NULL

    # extraction
    # etalonnages
    if(calib==TRUE) {
      conn <- dbConnect(SQLite(),fsq)
      table <- "LC"
      sta1 <- paste("'",as.character(sta),"'",sep="")
      selection <- paste ("SELECT * FROM",
                          table, " WHERE Id_Station =",sta1)
      xt <- dbGetQuery(conn, selection)
      dbDisconnect(conn)

      # controle
      blab <- as.character (xt$Date)
      if(is.na(blab[1])==TRUE)
        return(warning("\nNo calibration data for this station/sensor.\n"))

      # constitution du tableau de sortie
      xt$Date <- as.character(as.POSIXct(xt$Date, origin="1970-01-01"))
      calibtab <- data.frame(xt$Id_Station,xt$Capteur,xt$Capteur_Sortie,xt$Date,
                             xt$H,xt$Q)
      colnames(calibtab) <-c("Id_Station","Sensor","Sen_Out","Date","H","Q")
      calibtab <- as_tibble(calibtab)
    } else calibtab <- NA

    #jaugeages
    if(dism==TRUE) {
      conn <- dbConnect(SQLite(),fsq)
      table <- "DM"
      sta1 <- paste("'",as.character(sta),"'",sep="")
      selection <- paste ("SELECT * FROM",
                          table, " WHERE Id_Station =",sta1)
      xt <- dbGetQuery(conn, selection)
      dbDisconnect(conn)

      # controle
      blab <- as.character (xt$Date)
      if(is.na(blab[1])==TRUE)
        return(warning("\nNo discharge measurement data for this station/sensor.\n"))

      # constitution du tableau de sortie
      xt$Date <- as.character(as.POSIXct(xt$Date, origin="1970-01-01", tz=tzo))
      dismtab <- as_tibble(xt)
      dismtab <- select(dismtab, Id_Station, Capteur, Date, H, Q, Active)
      colnames(dismtab) <-c("Id_Station","Sensor","Date","H","Q","Active")
    } else dismtab <- NA

    # retour
    caldis <- list(calibtab, dismtab)
    return (caldis)
  }
  #FIN FUNCTION
  #----------

  #initialisation
  calibtab <- dismtab <- H1 <- Q1 <- Calibration <- H2 <- Q2 <- NULL
  caldis <- u_exp_discalib(fsq, sta, calib = TRUE, dism = TRUE)

  #controle
  calibtab <- caldis[[1]]
  dismtab <- caldis[[2]]
  if(is.na(calibtab) && is.na(dismtab))
    return(warning("\nWrong file.\n"))
  if(plotdism == TRUE && is.na(dismtab)){
    warning("\nNo discharge measurements in the calib file.")
    plotdism <- FALSE
  }
  if(limx==TRUE & (is.na(xinf)==TRUE | is.na(xsup)==TRUE))
    return(warning("\nVerify x axis limits.\n"))
  if(limy==TRUE & (is.na(yinf)==TRUE | is.na(ysup)==TRUE))
    return(warning("\nVerify y axis limits.\n"))

  # preparation etalonnages
  ld <- NA
  if(plotcalib == TRUE){
    d <- dplyr::filter(calibtab, calibtab$Sensor == sen)
    d <- transmute(d, Date = as_datetime(Date), H1 =as.numeric(d$H),
      Q1 = as.numeric(d$Q), Calibration = NA)
    limd <- as.character(levels(as.factor(d$Date)))
    ld <- length(limd)
    limd <- as_datetime(limd)
    for (i in 1:nrow(d)){
      for (j in 1:ld){
        if(d$Date[i]==limd[j]) d$Calibration[i] <- as.character(limd[j])
      }
    }
  }

# preparation jaugeages
  if(plotdism==TRUE) {
    d2 <- dplyr::filter(dismtab, dismtab$Sensor == sen)
    d2 <- transmute(d2, Date = as_datetime(Date), H2 = as.numeric(d2$H),
      Q2 = as.numeric(d2$Q), active = as.numeric(d2$Active), Calibration = "no_calib")
    for (i in 1:nrow(d2)) {
      if (is.na(d2$H2[i])) d2$active[i] <- 0
      if (is.na(d2$Q2[i])) d2$active[i] <- 0
    }
    d2 <- d2[d2$active==1,]
    if(!is.na(ld)) for (i in 1:nrow(d2)){
      if(d2$Date[i] < limd[1]) next
      if (ld == 1) {
        d2$Calibration[i] <- as.character(limd)
        next
      } else {
        for (j in 1:ld-1){
          if(d2$Date[i] >= limd[j] && d2$Date[i] < limd[j+1])
            d2$Calibration[i] <- as.character(limd[j])
        }
        if(d2$Date[i] >= limd[ld]) d2$Calibration[i] <- as.character(limd[ld])
      }
    }
  }
  d <- bind_rows(d,d2)

# trace etalonnages
  options(warn=-1)
  p <- ggplot(d) + geom_line(aes(x=H1, y=Q1, colour=Calibration))
  p <- p + theme(panel.background=element_rect(fill="white", colour="grey30"),
                 panel.grid.major=element_line(colour="grey30"),
                 panel.grid.minor=element_line(colour=NA))
  if(limx==TRUE) p <- p + xlim(xinf,xsup)
  if(limy==TRUE) p <- p + ylim(yinf,ysup)
  p <- p +
    ylab("Q (m3/s)") + xlab("H (cm)") +
    theme (axis.text.x = element_text(size=20),
           axis.text.y = element_text(size=20),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20),
           plot.title=element_text(face="bold", size=20)) +
    ggtitle(title)
  p <- p+ theme(legend.position="bottom") +
    theme(legend.text=element_text(size =16))

# trace jaugeages
  if(plotdism==TRUE) {
    p <- p + geom_point (aes(x=H2 , y=Q2, colour=Calibration ))
  }

# affichage et sauvegarde
  if (savefig==TRUE) {
    ggsave(fout, width=width, height=height, dpi=100)
    message("Graphique sauve dans le fichier : ", fout, "\n")
  }
  show(p)
  return(p)
}
# FIN
