library(tidyverse)
library(parsetR)
library(shiny)

## Load Data ----
employees <- read_rds('Data/EmployeeRecords_Clean.rds')
cat_cols <- c("Is Suspicious", "Gender", "Department", "Citizenship")

## app ----
parsetUI <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(NS(id, "lvl1"),
                    label = "Category Lvl1",
                    choices = cat_cols,
                    selected = "Is Suspicious"),
        
        selectInput(NS(id, "lvl2"),
                    label = "Category Lvl2",
                    choices = cat_cols,
                    selected = "Department"),
        
        selectInput(NS(id, "lvl3"),
                    label = "Category Lvl3",
                    choices = cat_cols,
                    selected = "Gender")
      ),
      mainPanel(
        parsetOutput(NS(id, "parset")),
        verbatimTextOutput(NS(id, "info"))
        
      )
    )
  )
}


parsetServer <- function(id, ui_input) {
  moduleServer(id, function(input, output, session) {

    output$parset <- renderParset({
      
      possible_cols <- c(input$lvl1, input$lvl2, input$lvl3)
      
      selected_employees <- reactive( {
        df <- employees %>%
          mutate("Is Suspicious" = ifelse(Name %in% ui_input$sus_peeps(), 'Suspicious', 'Not Suspcious'))
        return(df)
      })
      
      parset(
        selected_employees() ,
        dimensions = possible_cols,
        tension = 0.5)

    })

    output$info <- renderPrint({
      paste(ui_input$sus_peeps())

    })

  })

}