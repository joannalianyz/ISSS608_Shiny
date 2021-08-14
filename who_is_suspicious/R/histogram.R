library(tidyverse)
library(clock)

GasTech_df <- read_rds("Data/final_cc_employee.rds") 

GasTech_df$Date <- date_time_parse(GasTech_df$Date,
                                   zone = "",
                                   format = "%m/%d/%Y")
GasTech_df$CarID <- as_factor(GasTech_df$CarID)

# Module UI
histogramUI <- function(id) {
    tagList(
        h4(tags$b("Histogram/Datatable of Credit Card expenditure of GASTech Employees")),
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
                    NS(id, "emp_yrs"),
                    label = "No. of Years in Employment:",
                    min = 1,
                    max = 24,
                    value= c(1, 24)
                ),
                
                sliderInput(
                    NS(id, "age"),
                    label = "Age:",
                    min = 22,
                    max = 66,
                    value= c(22,66)
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
                filter(Category %in% input$category) %>% 
                filter(EmploymentYears >= strtoi(input$emp_yrs)[1] &  EmploymentYears <= strtoi(input$emp_yrs)[2] ) %>%
                filter(Age >= strtoi(input$age)[1] &  Age <= strtoi(input$age)[2] )
            
            hist(data$Price, 
                 breaks = input$bins, 
                 main = input$week)
        }, res = 96)
        
        output$cctable <- DT::renderDataTable({
            
            data <- GasTech_df %>%
                filter(Week == input$week) %>% 
                filter(Employment_Type %in% input$employment) %>%
                filter(Category %in% input$category) %>% 
                filter(EmploymentYears >= strtoi(input$emp_yrs)[1] &  EmploymentYears <= strtoi(input$emp_yrs)[2] ) %>%
                filter(Age >= strtoi(input$age)[1] &  Age <= strtoi(input$age)[2] )
            
            if(input$showDT){
                DT::datatable(data = data %>% select(1:10),
                              options= list(pageLength = 10),
                              rownames = FALSE)    
        }
    })
    }
)}