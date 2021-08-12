library(tidyverse)

## Load Data ----

email_headers <- read_csv("data/email/email_headers.csv")

# Nodes
gastech_nodes <- read_rds("data/email/gastech_employees.rds") %>% 
  mutate(title = paste(label, CurrentEmploymentTitle,' ')) %>%
  rename(group = CurrentEmploymentType,
         citizenship = CitizenshipCountry) %>% 
  select(id, label, group, title, citizenship)

# Edges 
gastech_edges <- read_rds("data/email/gastech_emails.rds")%>% 
  rename(from = source, to = target)  %>% 
  left_join(email_headers, by='Subject') 

# layout 
layout_list <- c("layout_with_fr", 'layout_nicely', 'layout_with_sugiyama', "layout_in_circle")
names(layout_list) <- c("Fruchterman Reingold", "Nicely", "Sugiyama", "Circle")

## app ----
emailUI <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = NS(id, 'dept'), 
          label = "Department", 
          choices = unique(gastech_nodes$group), 
          selected = unique(gastech_nodes$group), 
          multiple= TRUE
        ),
        selectInput(
          inputId = NS(id, 'email_type'), 
          label = "Email Type", 
          choices = unique(email_headers$EmailType), 
          multiple=FALSE
        ),
        sliderInput(
          inputId = NS(id, 'freq'), 
          label = "Weight", 
          min = 1,
          max = 20,
          value = 5,
        ),
        selectInput(
          inputId = NS(id, 'layout'), 
          label = "Network Graph Layout", 
          choices = names(layout_list), 
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
      
      selected_nodes <- gastech_nodes %>%
        filter(group %in% input$dept)
      
      selected_edges <- gastech_edges %>%
        filter(EmailType %in% input$email_type) %>%
        filter(from %in% selected_nodes$id) %>%
        filter(to %in% selected_nodes$id ) %>%
        group_by(from, to) %>%
        summarize(weight=n()) %>%
        filter(weight>=input$freq) %>%
        ungroup() %>% 
        rowid_to_column("id")
      
      visNetwork(selected_nodes, selected_edges, width = "100%", height = "150%") %>%
        visIgraphLayout(layout = layout_list[[input$layout]] ) %>%
        visLegend() %>%
        visEdges(smooth = FALSE, arrows = 'to') %>%
        visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
                   nodesIdSelection = TRUE) %>% 
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