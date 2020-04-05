library(shiny)
library(shinythemes)


ui <- fluidPage(
  
  shiny::navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,
                    title = "Stock Analysis",
                    
                    
                    # Start of tab panel #1 -------------------------------------------------------------------------------                    
                    shiny::tabPanel("Data Collection",
                                    
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
                                        
                                        shiny::radioButtons("time", "Choose Frequency:", 
                                                            choices = c('daily', 'weekly', 'monthly', 'yearly')),
                                        
                                        shiny::uiOutput("row_stocks"),
                                        
                                        shiny::conditionalPanel(condition = "input.panel == 'tab_2'",
                                                                shiny::uiOutput("column_names"),
                                                                checkboxInput("smooth", "Add Smooth Line:"))
                                        
                                      ),
                                      
                                      # Main panel for displaying outputs ----
                                      mainPanel(
                                        
                                        shiny::uiOutput("contents")
                                        
                                      ))
                                    
                    ),
                    # End of tab panel # 1 ------------------------------------------------------------------------------------------
                    
                    # Start of tab panel #2 -----------------------------------------------------------------------------------------
                    shiny::tabPanel("Data Exploration",
                                    
                                    # Sidebar layout with input and output definitions ----
                                    sidebarLayout(
                                      
                                      # Sidebar panel for inputs ----
                                      sidebarPanel(
                                        
                                       
                                        
                                      ),
                                      
                                      # Main panel for displaying outputs ----
                                      mainPanel(
                                        
                                        
                                        #shiny::uiOutput("contents")
                                        
                                      ))
                                    
                    )
                    
  )
)