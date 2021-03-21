rocUI <- function(id){
    ns <- NS(id)
    tagList(
        plotOutput(ns("plot"), click = ns("plot_click"))
    )
}

rocServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        xy <- reactiveVal(list(x = .5, y = .5))
        observeEvent(
            input$plot_click,
            xy(input$plot_click)  
        )
        
        output$plot <- renderPlot({
            FPF <- xy()$x
            TPF <- xy()$y
            ggplot()+
                geom_point(aes(x = FPF, y = TPF)) +
                geom_line(data = getIsoOR(FPF, TPF), aes(x = FPF, y = TPF)) +
                geom_abline(data = getIsoLR(FPF, TPF), aes(intercept = intercept, slope = slope), linetype = 2) +
                geom_abline(intercept = 0, slope = 1, colour = "grey") +
                coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = F) + 
                theme_bw()
        }, res = 96)
        
        #return as reactive (and not values xy())
        xy
    })
}

#Test
rocApp <- function() {
    ui <- fluidPage(
        rocUI("roc0")
    )
    server <- function(input, output, session) {
        rocServer("roc0")
    }
    shinyApp(ui, server)
}
#rocApp()
