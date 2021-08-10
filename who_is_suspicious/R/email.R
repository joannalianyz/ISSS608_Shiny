library(tidyverse)

## Load Data ----
# Nodes
gastech_nodes <- read_rds("data/email/gastech_employees.rds") %>% 
  mutate(title = paste(label, CurrentEmploymentTitle,' ')) %>%
  rename(group = CurrentEmploymentType,
         citizenship = CitizenshipCountry) %>% 
  select(id, label, group, title, citizenship)

# Edges 
gastech_emails <- read_rds("data/email/gastech_emails.rds")

gastech_edges <- gastech_emails %>% 
  group_by(source, target) %>%
  summarize(weight=n()) %>%
  filter(weight>1) %>%
  ungroup() %>% 
  rename(from = source, to = target) %>% 
  rowid_to_column("id")


## app ----
emailUI <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId='dept', 
          label = "Department", 
          choices = gastech_nodes$group, 
          multiple=FALSE
        )
      ),
      mainPanel(visNetworkOutput(NS(id, "email")))
    )
  )
}


emailServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$email <- renderVisNetwork({
    
      visNetwork(gastech_nodes, gastech_edges, width = "100%", height = '100%') %>%
        visIgraphLayout(layout = "layout_with_fr") %>%
        visLegend() %>%
        visEdges(smooth = FALSE, arrows = 'to') %>%
        visLayout(randomSeed = 123)
    })
  })
  
}

# Define server logic required to draw a histogram
# server <- function(input, output) {
#   

#   
#   
# }