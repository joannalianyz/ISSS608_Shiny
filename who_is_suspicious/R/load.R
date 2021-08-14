library(tidyverse)

## Load Data ----

email_headers <- read_csv("data/email/email_headers.csv")

loadUI <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        tags$b("Download email headers to edit"), br(), 
            
        downloadButton(NS(id,"downloadData"), "Download"),  
        
        br(), br(), br(),
        
        fileInput(
          NS(id, "upload"), 
          "Upload your version of email headers", 
          buttonLabel = "Upload..."
        ),
        
        textInput(
          NS(id, "delim"), 
          "Delimiter (leave blank to guess)", 
          ""
        )
      ), 
    
      mainPanel(
            
      )
    )
  )
}


loadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$downloadData <- downloadHandler(
      
      filename = function() {
        "email_headers.csv"
      },
      
      content = function(file) {
        write.csv(email_headers, file, row.names = FALSE)
      }
    )
    
  })
  
}
