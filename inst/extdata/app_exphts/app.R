load(file=system.file("extdata/fichier_fsq.RData",package="htsr"))
library(RSQLite)

# Define UI ------
ui <- fluidPage(
  titlePanel("hts file import"),

  sidebarLayout(
    sidebarPanel(
      h4("Sqlite data base:"),
      textOutput("FSQ"),
      br(),
      textInput("Station_id", "Station ID"),
      textInput("Sensor_id", "Sensor ID"),
      checkboxInput("Set_time", "Reduce time interval", value = FALSE),
      dateRangeInput("dates", "Time interval range"),
      actionButton("submit", "Submit"),
      radioButtons("plot_opt", br("Plot the extracted file"),
                   c("line"="line","bar"="bar"), selected = "line"),
      actionButton("plot", "Plot"),
      div("When finished press Done!"),
      actionButton("close", "Done")),

    mainPanel(
      textOutput("extract_written"),
      plotOutput("file_plot")
    )
  )
)


# Define server logic -----
server <- function(input, output) {

  output$FSQ <- renderText({basename(fsq)})

  re <- eventReactive (input$submit, ({
    sta= input$Station_id
    sen= input$Sensor_id
    sta1 <- paste0("'",sta,"'")
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
      htsr::d_exp_hts(fsq = fsq,
                      sta,
                      sen,
                      rtime = input$Set_time,
                      dstart=as.character((input$dates[1])),
                      dend=as.character((input$dates[2])),
                      rplot = FALSE)
      rep <- paste0("File written: ", dirname(fsq),"/",input$Sensor_id,"_",
                   input$Station_id,".hts")
    }
    rep
  }))
  output$extract_written <- renderText({re()})

  po <- eventReactive (input$plot, ({
    nomfic <- paste (dirname(fsq),"/",input$Sensor_id,"_",
                     input$Station_id,".hts",sep="")
    serlab <- as.factor(input$Sensor_id)
    if (input$plot_opt=="bar")
      htsr::p_bar(nbst=1, filei=nomfic,
        serlab=serlab, start=input$dates[1], end=input$dates[2],
        title="", type="Value",rnorm = FALSE, rtime=FALSE, rfixy=FALSE,
        pal = "black", fct = FALSE)
    if (input$plot_opt=="line")
      htsr::p_line(nbst=1, filei=nomfic,
        serlab=serlab, start=input$dates[1], end=input$dates[2],
        title="", type="Value",rnorm = FALSE, rtime=FALSE, rfixy=FALSE,
        pal="black",linet = 1, rppt = FALSE, linew=0.1, smooth = FALSE, fct = FALSE)
  }))
  output$file_plot <- renderPlot({po()})

  observeEvent(input$close, stopApp())
}

# Run the app
shinyApp(ui = ui, server = server)
