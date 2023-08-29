library(shiny)
library(shinyFiles)
library(fs)
library(RSQLite)


shinyServer(function(input, output, session) {
  
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())

  shinyFileChoose(input, "file", roots = volumes, session = session, 
    filetypes="sqlite")
  
  # output$filepaths <- renderPrint({
  #   if (is.integer(input$file)) {
  #     cat("No files have been selected (shinyFileChoose)")
  #   } else {
  #     parseFilePaths(volumes, input$file)
  #   }
  # })
  
  observeEvent(input$file, {
    tab <- parseFilePaths(volumes, input$file)
    fsq <- as.character(tab[1,4])
    # output$tab <- renderTable({tab})
    output$fsq <- renderText({paste("Selected sqlite data base:",fsq)})
  })
  
  re <- eventReactive (input$submit, ({
    req(input$file)
    tab <- parseFilePaths(volumes, input$file)
    fsq <- as.character(tab[1,4])
    # output$tab <- renderTable({tab})
    output$fsq <- renderText({paste("Selected sqlite data base:",fsq)})

    sta <- input$Station_id
    sen <- input$Sensor_id
    sta1 <- paste0("'",sta,"'")
    wd <- paste0(dirname(fsq),"/")
 
    fileo <- paste0(wd, input$Sensor_id,"_",
                    input$Station_id,".hts")
    compute <- TRUE
    
    if (sta == ""){
      compute <- FALSE
      rep <- paste0("A station id is mandatory!")
    }
    if (compute && sen == ""){
      compute <- FALSE
      rep <- paste0("A sensor id is mandatory!")
    }
    if (compute) {
      conn <- dbConnect(SQLite(),fsq)
      x <- dbReadTable(conn, "ST")
      nom <- x$Id_Station
      if (!(sta %in% nom)){
        compute <- FALSE
        rep <- paste0("The station ", sta, " is not in the data base!")
      }
      dbDisconnect(conn)
    }
    if (compute) {
      conn <- dbConnect(SQLite(),fsq)
      selection <- paste ("SELECT * FROM SS WHERE Id_station =",sta1)
      x <- dbGetQuery(conn, selection)
      nom <- x$Capteur
      if (!(sen %in% nom)){
        compute <- FALSE
        rep <- paste0("The sentor ", sen, " not exists for the station ", sta, "!")
      }
      dbDisconnect(conn)
    }
    if (compute) {
      tstab <-htsr::d_exp_hts(fsq = fsq,
                              sta,
                              sen,
                              rtime = input$Set_time,
                              dstart=as.character((input$dates[1])),
                              dend=as.character((input$dates[2])),
                              rplot = FALSE)
      save (file=fileo, tstab)
      rep <- paste0("File written: ", fileo)
    }
    
    po <- eventReactive (input$plot, {
      htsr::z_set(file.names= fileo, plot.label = sen,title = sta)
      if (input$plot_opt=="bar") htsr::p_bar()
      else htsr::p_line()
    })
    
    output$file_plot <- renderPlot({po()})
    
    return(rep)
  }))
  
  output$extract_written <- renderText({re()})
  
  observeEvent(input$close, {stopApp()})
  
})