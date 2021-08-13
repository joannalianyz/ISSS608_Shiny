library(shiny)
library(visNetwork)
library(tidyverse)

# Load Data 

gastech_employee <- read_rds('Data/EmployeeRecords_Clean.rds')

# Define UI with panel 
ui <- navbarPage(
    # theme = bslib::bs_theme(bootswatch = "flatly"), to select a nice theme in the future
    
    "You be the judge!",
    
    tabPanel("Starting Point", "Module for starting point", verbatimTextOutput("info")),
    
    navbarMenu(
        "What are they spending?",    
        tabPanel("EDA", histogramUI("hist")),  
        tabPanel("inferential", statplotUI('statplot'))
    ),
    
    tabPanel("What emails do they send?", emailUI("email")
    ),
    
    tabPanel("What is their profile?", parsetUI("parset")
    ),
    
    susUI('sus')
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    input_var <- susServer("sus")
    
    histogramServer("hist")
    
    statplotServer("statplot")

    emailServer("email")
    
    parsetServer("parset", input_var)
    
    output$info <- renderPrint({
        paste(input$sus_peeps)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
