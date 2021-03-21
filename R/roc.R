rocUI <- function(id){
    ns <- NS(id)
    tagList(
        plotOutput(ns("plot"), click = ns("plot_click"))
    )
}

rocServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Track FPF and TPF
        # default just a little bit better than .5
        FPF <- reactiveVal(.4)
        TPF <- reactiveVal(.6)
        
        # click updates FPF and TPF
        observeEvent(
            input$plot_click,
            {
                # constraints between 0 and 1 (excluding)
                x <- input$plot_click$x
                x <- case_when(
                    x <= 0 ~ 10^-2,
                    x >= 1 ~ 1-10^-2,
                    TRUE ~ x
                )
                # constraints between 0 and 1 (excluding)
                y <- input$plot_click$y
                y <- case_when(
                    y <= 0 ~ 10^-2,
                    y >= 1 ~ 1-10^-2,
                    TRUE ~ y
                )
                # comment if you want the lower triangle to be clickable
                if(y < x)  {x <- y <- (x+y)/2}
                FPF(x)
                TPF(y)
            }
        )
        
        output$plot <- renderPlot({
            isoOR <- getIsoOR(FPF(), TPF())
            isoLR <- getIsoLR(FPF(), TPF())
            
            ggplot()+
                geom_point(aes(x = FPF(), y = TPF())) +
                geom_line(data = isoOR, aes(x = FPF, y = TPF)) +
                geom_abline(data = isoLR, aes(intercept = intercept, slope = slope), linetype = 2) +
                geom_abline(intercept = 0, slope = 1, colour = "grey") +
                #coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = F) + 
                coord_cartesian(xlim = c(0,1), ylim = c(0,1)) + 
                
                theme_bw()
        }, res = 96)
        
        #return as reactive (and not values xy())
        list(FPF = FPF, TPF = TPF)
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
