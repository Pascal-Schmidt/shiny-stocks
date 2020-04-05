library(shiny)
library(shinythemes)
library(ggthemes)
library(tidyverse)
library(janitor)
library(BatchGetSymbols)

server <- function(input, output, session) {
  
  ##### get path to data #####  
  input_file <- reactive({
    
    req(input$file)
    req(input$Dates)
    req(input$time)
    
    file_extension <- unlist(stringr::str_split(input$file, pattern = "\\.")) %>%
      .[length(.)]
    
    if(file_extension == "csv") {
      
      # pull data into shiny application
      data_file <- input$file
      
    } else {
      
      stop(paste0("File Extension is ", file_extension, ", csv expected"))
      
    }
    
    return(data_file)
    
  })
  # ---------------------------------------------------------------------------------
  
  ##### get data #####
  data <- reactive({
    
    req(input$file)
    req(input$Dates)
    req(input$time)
    
    # pull data into shiny application
    (BatchGetSymbols(tickers = read.csv(input_file()$datapath) %>%
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
    
  })
  
  
  ##### output table #####
  output$stock_data_table <- renderTable({
    if(is.null(data())){return ()}
    
    selection <- paste0(input$stocks_selected, collapse = "|")
    if(length(input$stocks_selected != 0)) {
      
      data() %>%
        dplyr::filter(stringr::str_detect(ticker, pattern = selection))
      
    }
    
  })
  # ------------------------------------------------------------------------
  
  ###### outputs line chart of stocks #####
  output$stock_data_plot <- renderPlotly({
    if(is.null(data())){return ()}
    
    selection <- paste0(input$stocks_selected, collapse = "|")
    data() %>%
      dplyr::filter(stringr::str_detect(ticker, pattern = selection)) %>%
      dplyr::mutate(ref_date = lubridate::ymd(ref_date)) -> df
    
    if(length(input$stocks_selected != 0) & !(input$smooth)) {
      
      plotly::ggplotly(
        
        ggplot(df, aes_string(x = "ref_date", y = input$y, 
                              col = "ticker", group = "ticker")) +
          geom_point() +
          geom_line() +
          xlab("") +
          ylab(input$y) +
          theme_wsj() +
          ggtitle("Stock Development Over Time") 
      )
      
    } else if (length(input$stocks_selected) != 0 & input$smooth){
      
      plotly::ggplotly(
        
        ggplot(df, aes_string(x = "ref_date", y = input$y, 
                              col = "ticker", group = "ticker")) +
          geom_point() +
          geom_line() +
          geom_smooth(se = F) +
          xlab("") +
          ylab(input$y) +
          theme_tufte() +
          ggtitle("Stock Development Over Time") 
      )
      
    } else {
      
      ggplot() +
        ggtitle("No Stock Selected") +
        theme_solid() +
        theme(plot.title = element_text(size = 20, hjust = 0.5))
      
    }
    
    
    
  })
  # -----------------------------------------------------------------
  
  
  ##### based on data input, get all tickers as character vector #####
  different_stocks <- reactive({
    if(is.null(data())){return ()}
    
    data() %>%
      dplyr::distinct(ticker) %>%
      dplyr::pull() -> all_stocks
    return(all_stocks)
    
    
  })
  # -------------------------------------------------------------------
  
  ##### based on tickers, create a dropdown menu with imported ticker data #####
  output$row_stocks <- renderUI({
    if(is.null(data())){return ()}
    
    shiny::selectInput("stocks_selected", "Select your stocks", 
                       selected = different_stocks(),
                       choices = different_stocks(), multiple = TRUE)
    
  })
  # ----------------------------------------------------------------------------
  
  ##### based on column names of data, add drop down menue to select column for plotting #####
  output$column_names <- renderUI({
    if(is.null(data())){return ()}
    
    col_names <- data() %>%
      dplyr::select(price_open, price_high, price_low, price_close,
                    volume, price_adjusted) %>%
      colnames()
    
    shiny::selectInput("y", "Columns:",
                       choices = col_names)

  })
  # ------------------------------------------------------------------------------------------
  
  ##### after data import, create two tabs #####
  output$contents <- renderUI({
    if(is.null(data())){return ()}
    
    tabsetPanel(id = "panel", 
      tabPanel("Stocks", tableOutput("stock_data_table")),
      tabPanel("Plots", value = "tab_2", plotOutput("stock_data_plot")))
    
  })
  # --------------------------------------------
  
  
  output$panel_2 <- renderUI({
    if(is.null(data())){return (print("No data available"))}
    
    tabsetPanel(tabPanel("Plots", plotlyOutput("stock_data_plot")))
    
  })
  
}


