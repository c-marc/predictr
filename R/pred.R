predUI <- function(id){
    ns <- NS(id)
    tagList(
        plotOutput(ns("plot"), click = ns("plot_click"))
    )
}

predServer <- function(id, xy_ext) {
    stopifnot(is.reactive(xy_ext))
    
    moduleServer(id, function(input, output, session) {
        xy <- reactiveVal(list(x = .5, y = .5))
        observeEvent(
            input$plot_click,
            xy(input$plot_click)  
        )
        
        output$plot <- renderPlot({
            FPF <- xy_ext()$x
            TPF <- xy_ext()$y
            p <- getPredValues(FPF, TPF) %>%
                ggplot(aes(x = prior, y = posterior, group = type)) +
                geom_line() +
                geom_abline(intercept = 0, slope = 1, colour = "grey") +
                coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = F) + 
                theme_bw()
            
            prior <- xy()$x
            selectedPValues <- getPredValues(FPF, TPF, prior = prior)
            p + 
                geom_point(x = prior, y = prior) +
                geom_segment(
                    data = selectedPValues,
                    aes(x = prior, y = prior, xend = prior, yend = posterior),
                    arrow = arrow(length = unit(0.2,"cm"))
                )
        }, res = 96)

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
