#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(shiny)
library(tibble)
library(editData)

# load data
load(file=system.file("extdata/settings.RData",package="htsr"))
# nbst <- as.numeric(readline("Number of files to plot : "))
# filei <- as.character(NA)[1:nbst]
# ser <- as.character(NA)[1:nbst]
# for(i in 1:nbst) {
#   filei[i] <- file.choose()
#   ser[i] <- paste0("ser_id",i)
# }
# myfil <- tibble::tibble(filename = filei, series_id = ser, color = "black",
#                   lineshape = 1, linewidth = 0.2, plotpoint = FALSE, pointshape = 20,
#                   pointsize = 8)
# fil <- myfil
# myconf <- tibble::tibble (title = "Title",  yaxis_label = "Y-axis label",
#   norm_val = FALSE, fix_time = FALSE, dstart = "2010-01-01", dend = "2020-01-01",
#   fix_scale = FALSE, ymin = NA, ymax = NA, lin_trend = FALSE, facet = FALSE)
# conf <- myconf
# save(nbst, fil, conf, mapalette, tzo, file = system.file("extdata/settings.RData",package="htsr"))

# Define UI ------
ui <- fluidPage(

  tabsetPanel(
    tabPanel("Settings",
      splitLayout(
        textInput("title", "Title",conf$title[1]),
        textInput("yaxis", "Y-axis label",conf$yaxis_label[1]),
        br()
      ),
      splitLayout(
        checkboxInput("fixtime", "Fix time interval", FALSE),
        dateRangeInput("daterange", "Time interval",
                       start = conf$dstart[1],
                       end = conf$dend[1]),
        br()
      ),
      splitLayout(
        checkboxInput("fixy", "Fix Y-axis scale", FALSE),
        numericInput("ymin", "Min Y values", NA),
        numericInput("ymax", "Max Y values", NA)
      ),
      splitLayout(
        checkboxInput("normval", "Normalized values", FALSE),
        checkboxInput("trend", "Plot trend", FALSE),
        checkboxInput("facet", "Facet plots", FALSE)
      ),
      br(),
      editData::editableDTUI("table"),
      br(),
      actionButton("fil", "Save"),
      br(),
      div("Save settings and go to next tab")
    ),
    tabPanel("Plot",
      sidebarPanel(
        radioButtons("linebar", "Plot type", choices = c("line" = "line", "bar"="bar"), selected = "line"),
        radioButtons("palette", "Palette", choices = c("default"="default", "mypalette"="mypalette",
          "manual" = "manual"), selected = "default"),
        h5(tags$b("Save plot")),
        checkboxInput("savefig", "", value = FALSE),
        numericInput("pwidth", "Width in 100dpi", value = 8),
        numericInput("pheight", "Height in 100dpi", value = 6),
        textInput("fileo","Plot file name", value = "~/Bureau/rplot.png"),
        actionButton("myplot", "Plot")
      ),
      mainPanel(
        plotOutput("plot"),
        actionButton("close", "Done")
      )
    )
  )
)


# Define server logic -----
server <- function(input, output) {

  # conf
  # observeEvent(input$conf, {
  #   myconf <- tibble (title = input$title,  yaxis_label = input$yaxis,
  #     norm_val = input$normval, fix_time = input$fixtime,
  #     dstart = as.character(input$daterange[1]), dend = as.character(input$daterange[2]),
  #     fix_scale = input$fixy, ymin = input$ymin, ymax = input$ymax,
  #     lin_trend = input$trend, facet = input$facet)
  # })

  # fil
  myfil <- fil
  df2=callModule(editData::editableDT,"table",data=reactive(myfil))

  observeEvent(input$fil, {
    myconf <- tibble (title = input$title,  yaxis_label = input$yaxis,
                      norm_val = input$normval, fix_time = input$fixtime,
                      dstart = as.character(input$daterange[1]), dend = as.character(input$daterange[2]),
                      fix_scale = input$fixy, ymin = input$ymin, ymax = input$ymax,
                      lin_trend = input$trend, facet = input$facet)
    conf <- myconf
    fil <- df2()
    save(nbst, fil, conf, mapalette, tzo, file = system.file("extdata/settings.RData",package="htsr"))
  })

  #myplot
  po <- eventReactive (input$myplot, ({
    load(file = system.file("extdata/settings.RData",package="htsr"))
    if(input$palette == "default") rpal <- 0
    if(input$palette == "mypalette") rpal <- 1
    if(input$palette == "manual") rpal <- 2
    if (input$linebar=="bar")
      htsr::p_bar_app(nbst = input$nf, rpal= rpal, savefig=input$savefig,
        width= input$pwidth, height= input$pheight,
        fileo=input$fileo)
    else
      htsr::p_line_app(nbst = input$nf, rpal= rpal, savefig=input$savefig,
        width= input$pwidth, height= input$pheight,
        fileo=input$fileo)
  }))

  #plot
  output$plot <- renderPlot({po()})
  #
  #close
  observeEvent(input$close, {
    stopApp()
  })

}

# Run the app
shinyApp(ui = ui, server = server)
