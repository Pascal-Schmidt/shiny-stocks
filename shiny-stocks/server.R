#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggthemes)
library(tidyverse)
library(janitor)
library(BatchGetSymbols)

server <- function(input, output, session) {
  
  data <- reactive({
    
    req(input$file)
    req(input$Dates)
    req(input$time)
    
    file_extension <- unlist(stringr::str_split(input$file, pattern = "\\.")) %>%
      .[length(.)]
    
    if(file_extension == "csv") {
      
      # pull data into shiny application
      (BatchGetSymbols(tickers = read.csv(input$file$datapath) %>%
                         dplyr::pull(),
                       first.date = input$Dates[1],
                       last.date = input$Dates[2],
                       freq.data = input$time,
                       cache.folder = file.path(tempdir(),
                                                'BGS_Cache'))  %>%
         .[[2]] %>%
         dplyr::ungroup() %>%
         dplyr::select(price.open, price.high, price.low, price.close,
                       volume, price.adjusted, ref.date, ticker) %>%
         dplyr::mutate(ref.date = as.character(ref.date)) %>%
         janitor::clean_names())
      
      
    } else {
      
      stop(paste0("File Extension is ", file_extension, ", csv expected"))
      
    }
    
  })
  
  
  # Table of stocks displayed
  # Table is reactive and changes depending on the selected stocks
  # Table is made reaktive through the output$row_stocks, renderUI function
  output$stock_data_table <- renderTable({
    if(is.null(data())){return ()}
    
    selection <- paste0(input$stocks_selected, collapse = "|")
    if(length(input$stocks_selected != 0)) {
      
      data() %>%
        dplyr::filter(stringr::str_detect(ticker, pattern = selection))
      
    }
    
  })
  
  output$stock_data_plot <- renderPlot({
    if(is.null(data())){return ()}
    
    selection <- paste0(input$stocks_selected, collapse = "|")
    if(length(input$stocks_selected != 0)) {
      
      data() %>%
        dplyr::filter(stringr::str_detect(ticker, pattern = selection)) -> df
      
    }
    
    ggplot(df, aes_string(x = "ref_date", y = input$y, 
                          col = "ticker", group = "ticker")) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F) +
      xlab("Date") +
      ylab(input$y) +
      theme_economist() +
      ggtitle("Stock Development Over Time") 
    
    
  })
  
  
  
  different_stocks <- reactive({
    if(is.null(data())){return ()}
    
    data() %>%
      dplyr::distinct(ticker) %>%
      dplyr::pull() -> all_stocks
    return(all_stocks)
    
    
  })
  
  output$row_stocks <- renderUI({
    if(is.null(data())){return ()}
    
    shiny::selectInput("stocks_selected", "Select your stocks", 
                       selected = different_stocks(),
                       choices = different_stocks(), multiple = TRUE)
    
  })
  
  output$column_names <- renderUI({
    if(is.null(data())){return ()}
    
    shiny::selectInput("y", "Columns:", 
                       choices = names(data()))
    
  })
  
  
  output$contents <- renderUI({
    if(is.null(data())){return ()}
    
    
    tabsetPanel(tabPanel("Stocks", value = 1, tableOutput("stock_data_table")),
                tabPanel("Plots", value = 2, plotOutput("stock_data_plot")))
    
    
    
  })
  
}