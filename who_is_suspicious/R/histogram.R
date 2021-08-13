library(tidyverse)
library(clock)

GasTech_df <- read_csv("ProjectShiny/Final.csv")
GasTech_df$Date <- date_time_parse(GasTech_df$Date,
                                   zone = "",
                                   format = "%m/%d/%Y")
GasTech_df$CarID <- as_factor(GasTech_df$CarID)

df <- GasTech_df %>% 
  group_by(Employment_Type, Category, Weekday.Weekend, Location, Day_of_Week) %>% 
  summarise(total_spent = sum(Price))


# Module UI
histogramUI <- function(id) {
  tagList(
    titlePanel("Credit card data of GASTech employees"),    
    sidebarLayout(        
      sidebarPanel(            
        selectInput(NS(id, "variable"),
                    label = "Category to compare:",
                    choices = c("Employment_Type", "Weekday/Weekend", "Category", "Day_of_Week", "Location"),
                    selected = "Employment_Type"),
        
        sliderInput(NS(id, "bins"),
                    label = "No. of bins:",
                    min =5,
                    max = 20,
                    value= c(10)),
        checkboxInput(inputId = "show_data",
                      label = "Show data table",
                      value = TRUE)
      ),
      mainPanel(plotOutput(NS(id, "hist")),
                DT::dataTableOutput(outputId = "cctable")
      )
    )
  )
}

# Module server
histogramServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(df[[input$variable]])
    output$hist <- renderPlot({
      hist(data(),
           breaks = input$bins, 
           main = paste("Histogram of CC spend across",input$variable))
    }, res = 96)
  })
    output$cctable <- DT::renderDataTable({
       if(input$show_data){
          DT::datatable(data = GasTech %>% select(1:10),
                        options= list(pageLength = 10),
                        rownames = FALSE)    
      }
   })
}

shinyApp(histogramUI,histogramServer)
