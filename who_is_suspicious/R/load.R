library(shiny)
library(tidyverse)
library(tidytext)

## Load Data ----

email_headers <- read_csv("data/email/email_headers.csv")

loadUI <- function(id) {
  tagList(
    h4(tags$b("Download/Upload Email Headers: You decide what Email Type it is")), 
    sidebarLayout(
      sidebarPanel(
        tags$b("Download email headers to edit"), br(), 
            
        downloadButton(NS(id,"downloadData"), "Download"),  
        
        br(), br(), br(),
        
        fileInput(
          NS(id, "upload"), 
          "Upload your version of email headers", 
          buttonLabel = "Upload..."
        )
      ), 
    
      mainPanel(
        plotOutput(NS(id, "top_words")),
        DT::dataTableOutput(NS(id,"table"))
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
    
    
    userFile <- reactive({
      # If no file is selected, don't do anything
      validate(need(input$upload, message = FALSE))
      input$upload
    })
    
    # The user's data, parsed into a data frame
    dataframe <- reactive({
      if (is.null(input$upload)) {
        email_headers
      } else {
        read.csv(userFile()$datapath,
                 quote = "\"")
      }

    })
    
    
    output$table <- DT::renderDataTable({
      dataframe()
    })
    
    output$top_words <- renderPlot({
      
      token_words <- dataframe() %>%
        mutate(text = tolower(Subject)) %>% 
        unnest_tokens(word, text) %>%
        filter(str_detect(word, "[a-z']$"), # only keep words. exclude all numeric. 
               !word %in% stop_words$word) # to remove stop words 
      
      token_words %>%
        count(word, EmailType) %>%
        group_by(EmailType) %>%
        top_n(10,n) %>%
        ungroup() %>%
        mutate(word = fct_reorder(word,n)) %>%
        ggplot(aes(x = word, y=n, fill=EmailType)) +
        geom_col(show.legend = FALSE) + 
        facet_wrap( ~ EmailType, scales = "free_y") +
        coord_flip() + 
        ggtitle("Email Subject Word Count by Each Email Type")
      
    })
    return(dataframe)  
    # return(list(email_df = dataframe))
  })
  
}
