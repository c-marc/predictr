fullApp <- function(...) {
    ui <- fluidPage(
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
        xy <- rocServer("plot1")
        predServer("plot2", xy)
        
        res1 <- reactive(tribble(
            ~Measure, ~Value,
            "Sensitivity (TPF)", prettyProp(xy()$y),
            "Specificity (1-FPF)", prettyProp(1-xy()$x),
            "Positive LR", prettyRatio(getLRatios(xy()$y, xy()$x)$positive),
            "Negative LR", prettyRatio(getLRatios(xy()$y, xy()$x)$negative)
        ))
        output$res1 <- renderTable(res1())
        
        res2 <- reactive(tribble(
            ~Measure, ~Value,
            "Prior", prettyProp(xy()$y),
            "Positive predictive value", prettyProp(1-xy()$x),
            "Negative predicitve value", prettyProp(1-xy()$x)
        ))
        output$res2 <- renderTable(res2())
    }
    shinyApp(ui, server, ...)
}
