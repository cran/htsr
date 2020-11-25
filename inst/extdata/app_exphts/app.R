load(file=system.file("extdata/fichier_fsq.RData",package="htsr"))

# Define UI ------
ui <- fluidPage(
  titlePanel("hts file import"),

  sidebarLayout(
    sidebarPanel(
      h4("Sqlite data base:"),
      textOutput("FSQ"),
      br(),
      textInput("Station_id", "Station ID", value = "Station_id"),
      textInput("Sensor_id", "Sensor ID",  value = "Sensor_id"),
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

  observeEvent(input$submit, ({
    htsr::d_exp_hts(db.sqlite = fsq,
      sta= input$Station_id,
      sen= input$Sensor_id,
      rtime = input$Set_time,
      dstart=as.character((input$dates[1])),
      dend=as.character((input$dates[2])),
      rplot = FALSE)
  }))

  re <- eventReactive (input$submit, ({
    paste0("File written: ", dirname(fsq),"/",input$Sensor_id,"_",
                   input$Station_id,".hts")
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
