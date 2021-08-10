#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages = c('tmap','clock','leaflet','mapview','DT', 'ggiraph', 'plotly', 'shiny', 'tidyverse', 'dplyr','tibbletime', 'lubridate', 'rgdal', 'readr','gapminder','igraph','tidygraph', 'ggraph','forcats')
for(p in packages){
    if(!require(p, character.only = T)){    
        install.packages(p)  
    }  
    library(p, character.only = T)
}


GasTech <- read_csv("Final.csv")
GasTech$Date <- date_time_parse(GasTech$Date,
                                zone = "",
                                format = "%m/%d/%Y")
GasTech$CarID <- as_factor(GasTech$CarID)
glimpse(GasTech)

GasTech_df = data.frame(GasTech)

df <- GasTech_df %>% 
    group_by(Location, Employment_Type, Category, Weekday.Weekend) %>% 
    summarise(total_spent = sum(Price))

# Define UI for application

ui <- fluidPage(    
    titlePanel("What are the employees spending on?"),    
    sidebarLayout(        
        sidebarPanel(            
            selectInput(inputId = "variable",
                        label = "Employee Category:",
                        choices = unique(df$`Employment_Type`),
                        selected = "Security"),
            selectInput(inputId = "variable2",
                        label = "Category:",
                        choices = unique(df$Category),
                        selected = "Food"),
            selectInput(inputId = "variable3",
                        label = "Weekday/Weekend:",
                        choices = unique(df$Weekday.Weekend),
                        selected = "Weekday"),
            sliderInput(inputId = "bins",
                        label = "Number of Bins",
                        min = 5,
                        max = 20,
                        value= c(10)),
            checkboxInput(inputId = "show_data",
                          label = "Show data table",
                          value = TRUE)
        ),
        mainPanel(plotOutput(outputId = "Plot"),
                  DT::dataTableOutput(outputId = "cctable")
        )
    )
)


server <- function(input, output) {
    
    d <- reactive({
        df %>%
            filter(total_spent,
                   Location,
                   Employment_Type == input$variable,
                   Category == input$variable2,
                   Weekday.Weekend == input$variable3)
    }) 
    
    output$Plot <- renderPlot({
        
        x1    <- df$total_spent
        x1    <- na.omit(x1)
        bin <- seq(min(x1), max(x1), length.out = input$bins + 1)
        hist(x1, breaks = bin, col = "#75AADB", border = "white",
             xlab = "Credit card spending",
             main = "Histogram of credit card spending")
    })
        output$cctable <- DT::renderDataTable({
            if(input$show_data){
                DT::datatable(data = GasTech_df %>% select(1:11),
                              options= list(pageLength = 10),
                              rownames = FALSE)    
    }
})
}

# Run the application 

shinyApp(ui = ui, server = server)