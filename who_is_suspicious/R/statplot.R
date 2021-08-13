library(ggstatsplot)
library(tidyverse)
library(clock)

GasTech_df <- read_csv("data/Final.csv")
GasTech_df$Date <- date_time_parse(GasTech_df$Date,
                                zone = "",
                                format = "%m/%d/%Y")
GasTech_df$CarID <- as_factor(GasTech_df$CarID)

statplotUI <- function(id) {
  tagList(
    titlePanel("Credit card spending patterns of GASTech employees"),    
    sidebarLayout(        
      sidebarPanel(            
        selectInput(NS(id, "variable"),
                    label = "Category to compare:",
                    choices = c("Employment_Type", "Weekday/Weekend", "Category", "Day_of_Week", "Location"),
                    selected = "Employment_Type"),

        sliderInput(NS(id, "prices"),
                    label = "Price range:",
                    min = 0,
                    max = 10000,
                    value= c(1000)),
      ),
      mainPanel(
        plotOutput(NS(id, "statplot"))
      )
    )
  )
}

#Server 
statplotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$statplot <- renderPlot({
      ggbetweenstats(
        data = GasTech_df,
        x = !!sym(input$variable),
        y = !!sym(input$prices),
        title = paste("Distribution of CC spend across",input$variable)
      )
    })
    
  })
}
