library(tidyverse)
library(clock)

GasTech_df <- read_csv("Data/Final.csv")

GasTech_df$Date <- date_time_parse(GasTech_df$Date,
                                   zone = "",
                                   format = "%m/%d/%Y")
GasTech_df$CarID <- as_factor(GasTech_df$CarID)

# Module UI
histogramUI <- function(id) {
    tagList(
        sidebarLayout(
            sidebarPanel( 
                selectInput(
                    NS(id, "employment"),
                    label = "Employment Type",
                    choices = c("Security", "Engineering", "Information Technology", "Facilities", "Executive" ),
                    selected = "Security",
                    multiple = TRUE
                ),
                
                selectInput(
                    NS(id, "category"),
                    label = "Category of spending",
                    choices = c("Company", "Food", "Gas", "Leisure", "Retail" ),
                    selected = "Food",
                    multiple = TRUE
                ),
                
                selectInput(
                    NS(id, "week"),
                    label = "Weekday vs Weekend",
                    choices = c("Weekday", "Weekend"),
                    selected = "Weekday"
                ),
                    
                sliderInput(
                    NS(id, "bins"),
                    label = "No. of bins:",
                    min =5,
                    max = 20,
                    value= c(10)
                ),
                
                checkboxInput(
                    NS(id,"showDT"),
                    label = "Show data table",
                    value = TRUE)
                ),
                
            mainPanel(
                plotOutput(NS(id, "hist")),
                DT::dataTableOutput(NS(id,"cctable"))
            )
        )
    )
}

# Module server
histogramServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        output$hist <- renderPlot({
            
            data <- GasTech_df %>%
                filter(Week == input$week) %>% 
                filter(Employment_Type %in% input$employment) %>%
                filter(Category %in% input$category)
            
            hist(data$Price, 
                 breaks = input$bins, 
                 main = input$week)
        }, res = 96)
        
        output$cctable <- DT::renderDataTable({
            
            data <- GasTech_df %>%
                filter(Week == input$week) %>% 
                filter(Employment_Type %in% input$employment) %>%
                filter(Category %in% input$category)
            
            if(input$showDT){
                DT::datatable(data = data %>% select(1:10),
                              options= list(pageLength = 10),
                              rownames = FALSE)    
        }
    })
    }
)}