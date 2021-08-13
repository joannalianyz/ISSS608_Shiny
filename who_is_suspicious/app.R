library(shiny)
library(visNetwork)

# Load Data 


# Define UI with panel 
ui <- navbarPage(
    # theme = bslib::bs_theme(bootswatch = "flatly"), to select a nice theme in the future
    
    "You be the judge!",
    
    tabPanel("Starting Point", "Module for starting point"),
    
    navbarMenu(
        "What are they spending?",    
        tabPanel("EDA", "module for CC histogram and DT"),  
        tabPanel("inferential", statplotUI('statplot'))
    ),
    
    tabPanel("What emails do they send?", emailUI("email")
    ),
    
    tabPanel("TEST", histogramUI("hist")  ## USING HISTOGRAM AS AN EXAMPLE 
    ),
    
    tabPanel("What is their profile?", "Module for Parallel set - profile"
    ),
    
    selectInput(
        'sus_peeps', 
        "Who do you think is suspicious?",
        choices = c("A","B","C","D"),
        selected = c("A", "B"), 
        multiple = TRUE,
        width = '100%'
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    histogramServer("hist")
    
    statplotServer("statplot")

    emailServer("email")
    
}

# Run the application 
shinyApp(ui = ui, server = server)
