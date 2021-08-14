
gastech_employee <- read_rds('Data/EmployeeRecords_Clean.rds')


susUI <- function(id) {
  tagList(
    selectInput(
      NS(id, 'sus_peeps'), 
      label = tags$b("Who do you think is suspicious?"),
      choices = gastech_employee$Name,
      selected = c("Isia Vann", "Hennie Osvaldo", "Edvard Vann", "Loreto Bodrogi"), 
      multiple = TRUE,
      width = '100%'
    )
  )
}

susServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(
      list(sus_peeps = reactive({input$sus_peeps}))
    )
  })
}