library(tidyverse)
library(clock)

GasTech_df <- read_csv("Data/Final.csv")

GasTech_df$Date <- date_time_parse(GasTech_df$Date,
                                   zone = "",
                                   format = "%m/%d/%Y")
GasTech_df$CarID <- as_factor(GasTech_df$CarID)

# df <- GasTech_df %>% 
#     group_by(Employment_Type, Category, "Weekday/Weekend", Location, Day_of_Week) %>% 
#     summarize(total_spent = sum(Price))

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
                    inputId = "show_data",
                    label = "Show data table",
                    value = TRUE
                )
            ), 
            mainPanel(
                plotOutput(NS(id, "hist"))
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
                filter(Employment_Type %in% input$employment)
            
            hist(data$Price, 
                 breaks = input$bins, 
                 main = input$week)
        }, res = 96)
    })
}

