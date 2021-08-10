# Module UI
histogramUI <- function(id) {
    tagList(
        sidebarLayout(
            sidebarPanel(
                selectInput(NS(id, "var"), 
                            "Variable", 
                            choices = names(mtcars)),
                
                numericInput(NS(id, "bins"), 
                             "bins", 
                             value = 10, 
                             min = 1)
            ),
            mainPanel(plotOutput(NS(id, "hist"))
            )
        )
    )
}

# Module server
histogramServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        data <- reactive(mtcars[[input$var]])
        output$hist <- renderPlot({
            hist(data(), 
                 breaks = input$bins, 
                 main = input$var)
        }, res = 96)
    })
}

