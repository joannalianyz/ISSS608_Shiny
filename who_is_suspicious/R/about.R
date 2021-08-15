aboutUI <- function(id) {
  tagList(
    fluidRow(
      column(
        width = 12,
        h4(tags$b("The story")), 
        tags$div(
          "The scenario orginates from ", 
          tags$a(href="https://vast-challenge.github.io/2021/index.html", "Vast Challenge 2021"),
          "where in January, 2014, the leaders of GAStech are celebrating their new-found fortune as a 
          result of the initial public offering of their very successful company. 
          In the midst of this celebration, several employees of GAStech go missing. 
          An organization known as the Protectors of Kronos (POK) is suspected in the disappearance, 
          but things may not be what they seem. It appears that certain employees of GAStech may be 
          involved in the disappearance. Hence, this app can help you make that analysis for you to 
          decide who are the suspicious GAStech employees..."
        )
      ),
      column(
        width = 12,
        h4(tags$b("Starting point")), 
        "This application features findings from various types of data sources describing credit card transactions, 
        email correspondance and employee records of GASTech Employees. Click through each tab on the Navigation 
        Panel on top to go through each data set and make your own judgement. When you find someone suspicious, 
        Place their name in the input bar below. After you have completed your analysis, you can click on the tab 
        'What is their profile' to see the suspicious ones characteristics and perhaps discover more from there...", 
        "Good luck, have fun exploring the application!",
        br(), br(), br()
      )
    )
  )
}
  
  
  
  
  