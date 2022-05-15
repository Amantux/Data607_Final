#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(sentimentr)
library(RedditExtractoR)
library(TTR)
library(curl)
library(tidyverse)
library(quanteda)
library(readr)

library(dplyr)

library(tm)

library(SnowballC)
library(DT)


master <- TTR::stockSymbols(exchange = c("AMEX", "NASDAQ", "NYSE", "ARCA", "BATS", "IEX"))[,c('Name', 'Symbol')]
my_autocomplete_list = setNames(master, c("label", "value"))

ui <- fluidPage(
  sidebarPanel(
    #Stock Items Autocomplete
    selectizeInput(
      inputId = 'StockFilter',
      label = 'Enter Symbol',
      choices = NULL,
      selected = NULL,
      multiple = TRUE, # allow for multiple inputs
      options = list(create = TRUE) # if TRUE, allows newly created inputs
    ),
    selectInput("tp", "Time Periord to Search:", c("hour" = "hour", "day" = "day", "week" = "week", "month" = "month", "year" = "year", "all" = "all")),
    selectInput("sm", "Sort Method:", c("relevance" = "relevance", "comments" = "comments", "new" = "new", "hot" = "hot", "top" = "top")),
    textInput(
      inputId = "maxPull",
      label = "Enter Maximum Number of Posts to pull: ",
    ),
    textInput(
      inputId = "term",
      label = "Enter Search Term: ",
    ),
    actionButton(
      inputId = "submit_search",
      label = "Submit"
    ),
    sliderInput("obs", "Number of observations:",
                min = 0, max = 100000, value = 100
    ),
    dateRangeInput("dates", label = h3("Date range")),
    #add in ordinal or random sample
    #add in user selectable random sample
    checkboxGroupInput("variable", "Variables to show:",
                       c("Cylinders" = "cyl",
                         "Transmission" = "am",
                         "Gears" = "gear")),
    actionButton(
      inputId = "Execute",
      label = "Run"
    ),
    
  ),
  
  mainPanel(
  #tableOutput("rawData"),
  DT::dataTableOutput("prettyTable"),
  tableOutput("data"),
  textOutput("tp"),
  textOutput("StockFilter"),
  textOutput("sm"),
  textOutput("term"),
  textOutput("date"),
  textOutput("maxPull"),
  plotOutput("plot"),
  DT::dataTableOutput("stockdata")
)
)

server <- function(input, output, session) {
  #Populating Various Menu Items and Stuff
  
  updateSelectizeInput(session, 'StockFilter',
                       choices = my_autocomplete_list,
                       server = TRUE
  )
  
  
  output$data <- renderTable({
    head(mtcars[, c("mpg", input$variable), drop = FALSE], input$obs)
  }, rownames = TRUE)
  output$StockFilter <- renderText({input$StockFilter})
  output$tp <- renderText({input$tp})
  output$sm <- renderText({input$sm})
  output$maxPull <- renderText({input$maxPull})
  output$term <- renderText({input$term})
  output$date <- renderPrint({ input$dates })
  all_posts <- data.frame()
  if (is.null(all_posts) || dim(all_posts)[1] == 0) { 
    all_posts <- find_thread_urls(subreddit="wallstreetbets", sort_by="new", period="hour")
  }
  
  slimData <- function() {
    slim_posts <- head(all_posts, n=max({input$maxPull}, 1))
    slim_posts$index <- 1:nrow(slim_posts)
    corp <- corpus(slim_posts, docid_field = "index", text_field = "text")
    x <- kwic(tokens(corp, remove_punct = TRUE, remove_numbers = TRUE), 
              pattern = master$Symbol,
              window = 8, case_insensitive = FALSE,
    )
    x$index = x$docname
    add_In_Date <- slim_posts[c("index","date_utc")]
    rownames(add_In_Date) <- NULL
    target <- as.data.frame(x)
    target$sentence = paste(target$pre, target$post)
    target <- merge(target,add_In_Date,by="index")
    target$sentence <- str_replace_all(target$sentence, pattern = '[:punct:]',  replacement =" ")
    target
  }
  
  output$rawData <- renderTable(slimData())
  output$prettyTable = DT::renderDataTable({
    slimData()
  })
  
  observeEvent(
    eventExpr = input[["submit_search"]],
    handlerExpr = {
      #simulation code can go here
    }
  )
  observeEvent(
    eventExpr = input[["Execute"]],
    handlerExpr = {
      print("Test me")
    }
  )

  output$data <- renderTable({
    head(mtcars[, c("mpg", input$variable), drop = FALSE], input$obs)
  }, rownames = TRUE)

}

shinyApp(ui, server)

