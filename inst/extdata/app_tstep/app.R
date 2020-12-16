load(file=system.file("extdata/fichier_fhts.RData",package="htsr"))

# Define UI
ui <- fluidPage(

    titlePanel("Calculation of fixed time-step files"),

    h4("Original hts file:"),
    textOutput("FHTS"),
    br(),
    splitLayout(
        radioButtons("ts", "Time-step", c("hourly", "daily", "monthly", "other"),
            selected = "daily"),
        radioButtons("mode", "Mode", c("average", "sum", "max", "min"),
            selected = "average"),
        radioButtons("ifot", "If other", c("5mn", "10mn", "30mn",
            "2h", "3h", "6h", "12h")),
    br()),
    div("For other durations in min, use the function h_timestep"),
    br(),
    div(tags$b("If monthly")),
    checkboxInput("climedit", "climatogy file"),
    checkboxInput("caledit_j", "xlsx daily calendar"),
    checkboxInput("caledit_m", "xlsx monthly calendar"),
    checkboxInput("rmna", "remove NA"),
    checkboxInput("gapfill", "gapfilling"),
    checkboxInput("hts_year", "extract year stat"),
    br(),
    actionButton("submit", "Submit"),
    br(),
    textOutput("mon"),
    textOutput("MESS"),
    textOutput("MESS1"),
    br(),
    actionButton("close", "Done")
)


# Define server
server <- function(input, output) {
    re <- function() {
        tst <- 1440
        mn <- FALSE
        if(input$ts == "monthly") mn <- TRUE
        if(input$ts == "hourly") tst <- 60
        if(input$ts == "daily") tst <- 1440
        if(input$ts == "other") {
            if(input$ifot == "5mn") tst <- 5
            if(input$ifot == "10mn") tst <- 10
            if(input$ifot == "30mn") tst <- 30
            if(input$ifot == "2h") tst <- 120
            if(input$ifot == "3h") tst <- 180
            if(input$ifot == "6h") tst <- 360
            if(input$ifot == "12h") tst <- 720
        }
        if(input$mode == "average") op <- "M"
        if(input$mode == "sum") op <- "S"
        if(input$mode == "min") op <- "Mn"
        if(input$mode == "max") op <- "Mx"

        # Journalier et infra-journalier
        if (!(mn)) {
            f <- h_timestep(file=fhts, tst=tst, op = op)
            output$MESS <- renderText({paste("File written:", f)})
        }

        # Mensuel
        else {
            tst <- 1440
            f <- h_timestep(file=fhts, tst=1440, op = op)
            f1 <- h_month(file = f, op = op, ba = NA, rmna = input$rmna, climedit = input$climedit,
                caledit_j = input$caledit_j, caledit_m = input$caledit_m, gapfill = input$gapfill,
                hts_year = input$hts_year)
            f11 <- f1[[1]]
            output$MESS <- renderText({paste("File written:", f)})
            output$MESS1 <- renderText({paste("File written:", f11, " with eventual accompanying files")})
        }
    }

    output$FHTS <- renderText({basename(fhts)})
    observeEvent(input$submit, ({re()}))
    observeEvent(input$close, stopApp())
}

# Run the application
shinyApp(ui = ui, server = server)
