library(shiny)


ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file
      shiny::fileInput("file", "Choose CSV File"),
      
      # Select Date Range
      shiny::dateRangeInput("Dates",
                            "Dates:",
                            start = "2020-04-01",
                            end = Sys.Date(),
                            min = "2010-01-01",
                            max = Sys.Date(),
                            format = "yyyy/mm/dd",
                            separator = "-"),
      
      shiny::radioButtons("time", "Choose Frequency", 
                          choices = c('daily', 'weekly', 'monthly', 'yearly')),
      
      shiny::uiOutput("row_stocks"),
      shiny::uiOutput("column_names")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      
      shiny::uiOutput("contents")
      
    )
    
  )
)