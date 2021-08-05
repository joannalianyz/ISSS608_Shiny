#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages = c('DT', 'tmap','clock','leaflet','mapview','DT', 'ggiraph', 'plotly', 'shiny', 'tidyverse', 'dplyr','tibbletime', 'lubridate', 'rgdal', 'readr', 'sf', 'raster','igraph','tidygraph', 'ggraph','visNetwork','mapview','forcats')

for(p in packages){
    if(!require(p, character.only = T)){    
        install.packages(p)  
    }  
    library(p, character.only = T)
}

gps <- read_csv("gps.csv")

gps$Timestamp <-  date_time_parse(gps$Timestamp,
                                  zone = "",
                                  format = "%m/%d/%Y %H:%M")
gps$Day  = get_day(gps$Timestamp)
gps$Hour = get_hour(gps$Timestamp)
gps$id<- as_factor(gps$CarID)

bgmap <- raster("MC2-tourist_modified.tif")
tmap_mode("plot")
tm_shape(bgmap) +
    tm_raster(bgmap,
              legend.show = FALSE)

tm_shape(bgmap) +
    tm_rgb(bgmap, r = 1,g = 2,b = 3,
           alpha = NA,
           saturation = 1,
           interpolate = TRUE,
           max.value = 255)

Abila_st <- st_read(dsn = "Geospatial",
                    layer = "Abila")

gps_sf <- st_as_sf(gps, 
                   coords = c("long", "lat"),
                   crs= 4326)
gps_path <- gps_sf %>%
    group_by(CarID) %>%
    summarize(m = mean(Timestamp), 
              do_union=FALSE) %>%
    st_cast("LINESTRING")

p = npts(gps_path, by_feature = TRUE)
gps_path2 <- cbind(gps_path, p)
df_gps_path2 = data.frame(gps_path2)

GasTech <- read_csv("C:/joannalianyz/draft/Final.csv")


# Define UI for application

ui <- fluidPage(    
    titlePanel("GPS & credit card data of GASTech employees"),    
    sidebarLayout(        
        sidebarPanel(            
            radioButtons("EmpName", " Employee Name:",
                        c("Nils Calixto",
                          "Lars Azada",
                          "Felix Balas",
                          "Ingrid Barranco",
                          "Isak Baza",
                          "Linnea Bergen",
                          "Elsa Orilla",
                          "Lucas Alcazar",
                          "Gustav Cazar",
                          "Ada Campo-Corrente",
                          "Axel Calzas",
                          "Hideki Cocinaro",
                          "Inga Ferro",
                          "Lidelse Dedos",
                          "Loreto Bodrogi",
                          "Isia Vann",
                          "Sven Flecha",
                          "Birgitta Frente",
                          "Vira Frente",
                          "Stenig Fusil",
                          "Hennie Osvaldo",
                          "Adra Nubarron",
                          "Varja Lagos",
                          "Minke Mies",
                          "Kanon Herrero",
                          "Marin Onda",
                          "Kare Orilla",
                          "Isande Borrasca",
                          "Bertrand Ovan",
                          "Felix Resumir",
                          "Sten Sanjorge Jr.",
                          "Orhan Strum",
                          "Brand Tempestad",
                          "Edvard Vann",
                          "Willem Vasco-Pais"),
            selected = "Edvard Vann"),
                        ),
        mainPanel(width = 12,
                  DT::dataTableOutput("mytable")
        )
    )
)


server <- function(input, output){
    observe({
        x <- input$inRadioButtons
    output$mytable = DT::renderDataTable(Final,
                                         filter = "top",
                                         options = list(
                                             pageLength = 10)
    )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)