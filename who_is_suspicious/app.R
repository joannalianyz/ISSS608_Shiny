library(shiny)
library(visNetwork)
library(tidyverse)
library(igraph)

# Define UI with panel 
ui <- navbarPage(
    theme = bslib::bs_theme(bootswatch = "flatly"), # to select a nice theme in the future
    
    "You be the judge!",
    
    tabPanel("Starting Point", aboutUI("about")),
    
    tabPanel(
        "What are they spending?", 
        tabsetPanel(
            tabPanel("Histogram", histogramUI("hist")),  
            tabPanel("Inferential", statplotUI('statplot'))
        )
    ), 
    
    tabPanel(
        "What emails do they send?", 
        tabsetPanel(
            tabPanel("Network Viz", emailUI("email")),  
            tabPanel("Load Email Headers", loadUI('load'))
        )
    ),
    
    tabPanel("What is their profile?", parsetUI("parset")
    ),
    
    susUI('sus'), 
    verbatimTextOutput("info")
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    input_var <- susServer("sus")
    
    histogramServer("hist")
    
    statplotServer("statplot")

    emailServer("email", load)
    
    load <- loadServer("load")
    
    parsetServer("parset", input_var)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
