#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages = c('ggstatsplot','clock','leaflet','mapview','DT', 'ggiraph', 'plotly', 'shiny', 'tidyverse', 'dplyr','tibbletime', 'lubridate', 'rgdal', 'readr','gapminder','igraph','tidygraph', 'ggraph','visNetwork','mapview','forcats')
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
      group_by(Employment_Type, Category, Weekday.Weekend) %>% 
      summarise(total_spent = sum(Price))

ggbetweenstats(
  data = GasTech_df,
  x = Employment_Type,
  y = Price,
  title = "Distribution of CC spend across locations"
)

ggbetweenstats(
  data = GasTech_df,
  x = Category,
  y = Price,
  title = "Distribution of CC spend across locations"
)

# Define UI for application

ui <- fluidPage(    
    titlePanel("GPS & credit card data of GASTech employees"),    
    sidebarLayout(        
        sidebarPanel(            
            selectInput(inputId = "variable",
                        label = "Employee Type:",
                        choices = unique(GasTech$Employment_Type),
                        selected = "Security"),
            selectInput(inputId = "variable2",
                        label = "Category:",
                        choices = unique(GasTech$Category),
                        selected = "Food"),
            selectInput(inputId = "variable3",
                        label = "Weekday/Weekend:",
                        choices = unique(GasTech$`Weekday/Weekend`),
                        selected = "Weekday"),
            selectInput(inputId = "variable4",
                        label="Confidence Level:",
                        choices=c("90%"=0.1,
                                  "95%"=0.05,
                                  "99%"=0.01,
                                  "99.9%"=0.001),
                        selected = "90%"),
            checkboxInput(inputId = "show_data",
                          label = "Show data table",
                          value = TRUE)
        ),
        mainPanel(plotOutput("distPlot"),
                  DT::dataTableOutput(outputId = "cctable")
        )
    )
)


server <- function(input, output) {
        output$distPlot <- renderPlotly({
          ggbetweenstats(
            data = GasTech_df,
            x = Employment_T,
            y = Price,
            title = "Distribution of CC spend across locations"
          )
          
          ggbetweenstats(
            data = GasTech_df,
            x = Category,
            y = Price,
            title = "Distribution of CC spend across locations"
          )
})
        output$cctable <- DT::renderDataTable({
            if(input$show_data){
                DT::datatable(data = GasTech %>% select(1:10),
                              options= list(pageLength = 10),
                              rownames = FALSE)    
            }
        })
}

# Run the application 

shinyApp(ui = ui, server = server)