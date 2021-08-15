library(tidyverse)
library(parsetR)
library(shiny)

## Load Data ----
employees <- read_rds('Data/EmployeeRecords_Clean.rds') %>% 
  select(-c(Age)) %>%
  rename(Age = Age_bin, 
         `Years After Discharge` = YearsAftDischarge_bin, 
         `Year in Employment` = EmploymentYears_bin)
cat_cols <- c("Is Suspicious", "Gender", "Department", "Citizenship", "Age",
              "Year in Employment", "Years After Discharge",
              "Total Expenditure During Weekdays", "Total Expenditure During Weekends",
              "Total Expenditure For Food", "Total Expenditure For Leisure", "Total Expenditure For Retail", 
              "Total Expenditure For Company", "Total Expenditure For Gas")

## app ----
parsetUI <- function(id) {
  tagList(
    h4(tags$b("Parallel Set View of Profile of GASTech Employees")),
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
                    selected = "Gender"),
        
        selectInput(NS(id, "lvl4"),
                    label = "Category Lvl4",
                    choices = cat_cols,
                    selected = "Age"),
        
        selectInput(NS(id, "lvl5"),
                    label = "Category Lvl5",
                    choices = cat_cols,
                    selected = "Citizenship")
      ),
      mainPanel(
        parsetOutput(NS(id, "parset"))
        
      )
    )
  )
}


parsetServer <- function(id, ui_input) {
  moduleServer(id, function(input, output, session) {

    output$parset <- renderParset({
      
      possible_cols <- c(input$lvl1, input$lvl2, input$lvl3, input$lvl4, input$lvl5)
      
      selected_employees <- reactive( {
        df <- employees %>%
          mutate("Is Suspicious" = ifelse(Name %in% ui_input$sus_peeps(), 'Suspicious', 'Not Suspcious'))
        return(df)
      })
      
      parset(
        selected_employees() ,
        dimensions = possible_cols,
        tension = 0.5,
        width = "80%")

    })

  })

}