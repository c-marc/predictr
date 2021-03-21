predUI <- function(id){
    ns <- NS(id)
    tagList(
        plotOutput(ns("plot"), click = ns("plot_click"))
    )
}

predServer <- function(id, FPF, TPF) {
    stopifnot(is.reactive(FPF))
    stopifnot(is.reactive(TPF))
    
    moduleServer(id, function(input, output, session) {
        #prior is the producer
        prior <- reactiveVal(.5)
        
        #click x defines prior
        observeEvent(
            input$plot_click,
            {
                #constraint on value: between 0 and 1 (excluding)
                x <- input$plot_click$x
                x <- case_when(
                    x <= 0 ~ 10^-2,
                    x >= 1 ~ 1-10^-2,
                    TRUE ~ x
                )
                prior(x)
            }
        )
        
        output$plot <- renderPlot({
            #data for the 2 curves: PPV ~ prior and NPV ~ prior
            allPredValues <- getPredValues(FPF(), TPF())

            p <- ggplot(allPredValues, aes(x = prior, y = posterior, group = type)) +
                geom_line() +
                geom_abline(intercept = 0, slope = 1, colour = "grey") +
                #coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = F) + 
                coord_cartesian(xlim = c(0,1), ylim = c(0,1)) + 
                scale_x_continuous(labels = scales::percent) +
                scale_y_continuous(labels = scales::percent) +
                labs(x = "Prior probability", y = "Predictive value") +
                theme_bw()
            
            #data for the selected prior()
            selectedPValues <- getPredValues(FPF(), TPF(), prior = prior())
            
            #add layers
            p + 
                geom_point(x = prior(), y = prior()) +
                geom_segment(
                    data = selectedPValues,
                    aes(x = prior, y = prior, xend = prior, yend = posterior),
                    arrow = arrow(length = unit(0.2,"cm"))
                )
        }, res = 96)

        #return prior as a reactive
        prior
    })
}

#Test
predApp <- function(...) {
    ui <- fluidPage(
        predUI("test0")
    )
    server <- function(input, output, session) {
        predServer("test0", ...)
    }
    shinyApp(ui, server)
}
#predApp(reactiveVal(list(x=.2,y=.5)))
