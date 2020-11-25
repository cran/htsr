load(file=system.file("extdata/fichier_fsq.RData",package="htsr"))

# Define UI
ui <- fluidPage(

    titlePanel("Data base inventory"),

    sidebarLayout(
        sidebarPanel(
            h4("Sqlite data base:"),
            textOutput("FSQ"),
            br(),
            textInput("Station_id", "Station ID"),
            actionButton("submit", "Submit"),
            div("When finished press Done"),
            actionButton("close", "Done")),
        mainPanel(
            textOutput("wait"),
            tableOutput("table")
        )
    )
)

# Define server
server <- function(input, output) {

    output$FSQ <- renderText({basename(fsq)})

    output$wait <- renderText("Please wait for calculation...")

    re <- eventReactive(input$submit, ({
        if (input$Station_id == "") st <- NA else st <- input$Station_id
        a <- htsr::d_inventory(db.sqlite = fsq, stalist = st, form.out = NA)
        if (input$Station_id == "") tabl <- a[[1]] else tabl <- a[[2]]
        if (input$Station_id != ""){
            tabl$Date_start <- as.character(tabl$Date_start)
            tabl$Date_end <- as.character(tabl$Date_end)
        }
        return(tabl)
    }))
    output$table <- renderTable({re()})
    observeEvent(input$close, stopApp())
}

# Run the application
shinyApp(ui = ui, server = server)
