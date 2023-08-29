library(shiny)
library(shinyFiles)
library(tibble)
library(dplyr)
library(lubridate)
library(htsr)
library(waiter)

# tstep <- c("monthly", "daily", "12h", "6h", "3h", "2h", "hourly", "30mn",
#             "10mn", "5mn")
# lmode <- c("average", "max", "min", "sum")

# Define server
shinyServer (function(input, output, session) {
  options(shiny.maxRequestSize=1000*1024^2)
  
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyFileChoose(input, "file", roots = volumes, session = session, 
                  filetypes="hts")
  
  observeEvent(input$file, {
    tabfile <- parseFilePaths(volumes, input$file)
    ff <- tabfile$datapath[1]

    output$ff <- renderText ({paste("selected file:", ff)})
    output$MESS2 <- renderText({
    "The calculation time depends on the number of records and the time step. 
      It can last. Wait for the file writing message to appear. Be patient!"})
    
    observeEvent(input$submit, ({
      tabfile <- parseFilePaths(volumes, input$file)
      ff <- tabfile$datapath[1]
    
      tst <- 1440
      mn <- FALSE
      if(input$ts == "monthly") mn <- TRUE
      if(input$ts == "hourly") tst <- 60
      if(input$ts == "daily") tst <- 1440
      if(input$ts == "5mn") tst <- 5
      if(input$ts == "10mn") tst <- 10
      if(input$ts == "30mn") tst <- 30
      if(input$ts == "2h") tst <- 120
      if(input$ts == "3h") tst <- 180
      if(input$ts == "6h") tst <- 360
      if(input$ts == "12h") tst <- 720
      if(input$mode == "average") op <- "M"
      if(input$mode == "sum") op <- "S"
      if(input$mode == "min") op <- "Mn"
      if(input$mode == "max") op <- "Mx"
      shift <- as.numeric(input$shift)

      # Journalier et infra-journalier
      if (mn) {
        waiter <- waiter::Waiter$new()
        waiter$show()
        on.exit(waiter$hide())
        tst <- 1440
        f <- h_timestep(file=ff, tst=1440, op = op, shift = 0)
        f1 <- h_month(file = f, op = op, ba = NA, rmna = input$rmna, climedit = input$climedit,
                      gapfill = input$gapfill, hts_year = input$hts_year)
        output$MESS1 <- renderText({paste("File written:", f1[1],
          " with eventual accompanying files")})
      } else {
        waiter <- waiter::Waiter$new()
        waiter$show()
        on.exit(waiter$hide())
        f <- h_timestep(file=ff, tst=tst, op = op, shift = shift)
        output$MESS <- renderText({paste("File written:", f)})
      } 
    }))
  })

  observeEvent(input$close, stopApp())
})


