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
        tabPanel("EDA", "module for CC histogram and DT"),  
        tabPanel("inferential", statplotUI('statplot'))
    ),
    
    tabPanel("What emails do they send?", emailUI("email")
    ),
    
    tabPanel("TEST", histogramUI("hist")  ## USING HISTOGRAM AS AN EXAMPLE 
    ),
    
    tabPanel("What is their profile?", parsetUI("parset")
    ),
    
    selectInput(
        inputId = 'sus_peeps', 
        label = "Who do you think is suspicious?",
        choices = gastech_employee$Name,
        selected = c("Isia Vann", "Hennie Osvaldo", "Edvard Vann", "Loreto Bodrogi"), 
        multiple = TRUE,
        width = '100%'
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    histogramServer("hist")
    
    statplotServer("statplot")

    emailServer("email")
    
    parsetServer("parset", input$sus_peeps)
    
    output$info <- renderPrint({
        paste(input$sus_peeps)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
