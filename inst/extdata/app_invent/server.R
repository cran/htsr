library(shiny)
library(shinyFiles)
library(htsr)

shinyServer (function(input, output, session) {
  options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
  
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  
  shinyFileChoose(input, "file", roots = volumes, session = session, 
                  filetypes="sqlite")
  
  observeEvent(input$file, {
    tab <- parseFilePaths(volumes, input$file)
    fsq <- as.character(tab[1,4])
    # output$tab <- renderTable({tab})
    output$fsq <- renderText({paste("Selected sqlite data base:",fsq)})
  })
  
  re <- eventReactive(input$station, ({
    req(input$file)

    tab <- parseFilePaths(volumes, input$file)
    fsq <- as.character(tab[1,4])
    
    st <- input$Station_id
    if (st == "")
      st <- NA
    waiter <- waiter::Waiter$new()
    waiter$show()
    on.exit(waiter$hide())
    
    a <-
      htsr::d_inventory(
        fsq = fsq,
        sta_sen = st,
        form.out = NA
      )
    if (is.na(st)) {
      rep <- a[[1]]
    } else {
      rep <- a[[2]]
      if (ncol(rep) > 2) {
        rep$Date_start <- as.character(rep$Date_start)
        rep$Date_end <- as.character(rep$Date_end)
      }
    }
    return(rep)
  }))
  
  output$station <- renderTable({
    re()
  })
  observeEvent(input$close, stopApp())
})
