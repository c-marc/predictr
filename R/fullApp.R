fullApp <- function(...) {
    ui <- fluidPage(
        #darkly, flatly, minty, sketchy
        theme = bslib::bs_theme(bootswatch = "flatly"),
        
        #titlePanel("Demo"),
        #helpText("Interactive plots : left-click to explore relationships."),
        fluidRow(
            column(6, rocUI("plot1")),
            column(6, predUI("plot2"))
        ),
        fluidRow(
            column(offset = 1, 4, tableOutput("res1")),
            column(offset = 2, 4, tableOutput("res2"))
        )
         

    )
    server <- function(input, output, session) {
        # this is horribly slow feature
        #thematic::thematic_shiny()
        
        # module 'roc' tracks FPF and TPF
        # zeallot unstructuring operator
        c(FPF, TPF) %<-% rocServer("plot1")
        #xy <- rocServer("plot1")
        #FPF <- xy$FPF
        #TPF <- xy$TPF

        # module 'pred' tracks prior
        prior <- predServer("plot2", FPF, TPF)
        
        # recompute OR as reactive
        OR <- reactive(getOR(FPF(), TPF()))
        # recompute LRatios as reactive
        lRatios <- reactive(getLRatios(FPF(), TPF()))
        
        res1 <- reactive(tribble(
            ~Measure, ~Value,
            "Sensitivity (TPF)", prettyProp(TPF()),
            "Specificity (1-FPF)", prettyProp(1-FPF()),
            "OR (Test <-> Disease)", prettyRatio(OR()),
            "Positive LR", prettyRatio(lRatios()$positive),
            "Negative LR", prettyRatio(lRatios()$negative)
        ))
        output$res1 <- renderTable(res1())
        
        # recompute LRatios as reactive
        selectedPredValues <- reactive(getPredValues(FPF(), TPF(), prior = prior()))
        
        res2 <- reactive(tribble(
            ~Measure, ~Value,
            "Positive predictive value", prettyProp(selectedPredValues()$posterior[1]),
            "Prior", prettyProp(prior()),
            "Negative predicitve value", prettyProp(selectedPredValues()$posterior[2])
        ))
        output$res2 <- renderTable(res2())
    }
    shinyApp(ui, server, ...)
}
