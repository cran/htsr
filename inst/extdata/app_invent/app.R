load(file=system.file("extdata/fichier_fsq.RData",package="htsr"))

# Define UI
ui <- fluidPage(

    titlePanel("Data base inventory"),

    sidebarLayout(
        sidebarPanel(width = 3,
            h4("Sqlite data base:"),
            textOutput("FSQ"),
            br(),
            div("Station_id blank: all stations are displayed"),
            div("Station_id filled: its sensors are displayed"),
            textInput("Station_id", "Station ID"),
            actionButton("station", "Submit"),
            br(),
            br(),

            actionButton("close", "Done")),
        mainPanel(
            tableOutput("station"),
        )
    )
)

# Define server
server <- function(input, output) {

    re <- eventReactive(input$station, ({
        st <- input$Station_id
        if (st == "") st <- NA
        a <- htsr::d_inventory(fsq = fsq, sta_sen = st, form.out = NA)
        if(is.na(st)) rep <- a[[1]] else {
            rep <- a[[2]]
            if(ncol(rep) > 2) {
                rep$Date_start <- as.character(rep$Date_start)
                rep$Date_end <- as.character(rep$Date_end)
            }
        }
        return(rep)
    }))

    output$FSQ <- renderText({basename(fsq)})

    output$station <- renderTable({re()})

    observeEvent(input$close, stopApp())
}

# Run the application
shinyApp(ui = ui, server = server)
