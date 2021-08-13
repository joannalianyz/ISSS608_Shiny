library(ggstatsplot)
library(tidyverse)
library(clock)

GasTech_df <- read_csv("data/Final.csv")
GasTech_df$Date <- date_time_parse(GasTech_df$Date,
                                zone = "",
                                format = "%m/%d/%Y")
GasTech_df$CarID <- as_factor(GasTech_df$CarID)
# 
# GasTech_df = data.frame(GasTech)

# df <- GasTech_df %>% 
#   group_by(Employment_Type, Category, Weekday.Weekend) %>% 
#   summarise(total_spent = sum(Price))

# ggbetweenstats(
#   data = GasTech_df,
#   x = Employment_Type,
#   y = Price,
#   title = "Distribution of CC spend across locations"
# )
# 
# ggbetweenstats(
#   data = GasTech_df,
#   x = Category,
#   y = Price,
#   title = "Distribution of CC spend across locations"
# )



statplotUI <- function(id) {
  tagList(
    titlePanel("GPS & credit card data of GASTech employees"),    
    sidebarLayout(        
      sidebarPanel(            
        selectInput(NS(id, "variable"),
                    label = "Category to compare:",
                    choices = c("Employment_Type", "Weekday/Weekend", "Category", "Day_of_Week", "Location"),
                    selected = "Employment_Type"),
        # selectInput(inputId = "variable2",
        #             label = "Category:",
        #             choices = unique(GasTech$Category),
        #             selected = "Food"),
        # selectInput(inputId = "variable3",
        #             label = "Weekday/Weekend:",
        #             choices = unique(GasTech$`Weekday/Weekend`),
        #             selected = "Weekday"),
        sliderInput(NS(id, "bins"),
                    label = "Number of Bins",
                    min = 5,
                    max = 20,
                    value= c(10)),
        checkboxInput(NS(id, "show_data"),
                      label = "Show data table",
                      value = TRUE)
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
        y = Price,
        title = paste("Distribution of CC spend across",input$variable)
      )
    })
    
  })
}
