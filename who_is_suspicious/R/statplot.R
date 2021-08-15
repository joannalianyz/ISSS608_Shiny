library(ggstatsplot)
library(tidyverse)
library(clock)

GasTech_df <- read_rds("Data/final_cc_employee.rds")
GasTech_df$Date <- date_time_parse(GasTech_df$Date,
                                   zone = "",
                                   format = "%m/%d/%Y")
GasTech_df$CarID <- as_factor(GasTech_df$CarID)

choices <- c("Employment_Type", "Category", "Day_of_Week", 'Citizenship', 'Gender', 
             'Age_bin', 'EmploymentYears_bin', 'YearsAftDischarge_bin')
choices_names <- c("Department", "Category", "Day of Week", "Citizenship", 'Gender', 
                   "Age", "Years in Employment", "Years After Military Discharge")
names(choices) <- choices_names

statplotUI <- function(id) {
  tagList(
    h4(tags$b("Credit card spending patterns of GASTech employees")),    
    sidebarLayout(        
      sidebarPanel(            
        selectInput(
          NS(id, "variable"),
          label = "Category to compare:",
          choices = choices_names,
          selected = "Department"),
        
        sliderInput(
          NS(id, "price"),
          label = "Range of Prices:",
          min = 1,
          max = max(GasTech_df$Price),
          value= c(1,600)
        )
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
    
    # Q <- quantile(GasTech_df$Price, probs=c(.25, .75), na.rm = FALSE)
    # iqr <- IQR(GasTech_df$Price)
    # data <- subset(GasTech_df, GasTech_df$Price > (Q[1] - 1.5*iqr) & GasTech_df$Price < (Q[2]+1.5*iqr))
  
    
    output$statplot <- renderPlot({
      data <- GasTech_df %>%
        filter(Price >= strtoi(input$price)[1] &  Price <= strtoi(input$price)[2] )
      ggbetweenstats(
        data = data,
        x = !!sym(choices[[input$variable]]),
        y = Price,
        xlab = input$variable, 
        title = paste("Distribution of CC spend across",input$variable)
      )
    })
    
  })
}

