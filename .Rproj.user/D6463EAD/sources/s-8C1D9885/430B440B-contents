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


ui <- fluidPage(
  sidebarPanel(
    selectInput("tp", "Time Periord to Search:", c("hour" = "hour", "day" = "day", "week" = "week", "month" = "month", "year" = "year", "all" = "all")),
    selectInput("sm", "Sort Method:", c("relevance" = "relevance", "comments" = "comments", "new" = "new", "hot" = "hot", "top" = "top")),
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
  tableOutput("data"),
  textOutput("tp"),
  textOutput("sm"),
  textOutput("term"),
  textOutput("date"),
  plotOutput("plot"),
  tableOutput("stockdata")
)
)

server <- function(input, output, session) {
  output$data <- renderTable({
    head(mtcars[, c("mpg", input$variable), drop = FALSE], input$obs)
  }, rownames = TRUE)
  output$tp <- renderText({input$tp})
  output$sm <- renderText({input$sm})
  output$term <- renderText({input$term})
  output$date <- renderPrint({ input$dates })
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
  output$plot <- renderPlot(plot({
    master <- TTR::stockSymbols(exchange = c("AMEX", "NASDAQ", "NYSE", "ARCA", "BATS", "IEX"))[,c('Name', 'Symbol')]
    posts <- find_thread_urls(subreddit="wallstreetbets", sort_by="new", period="hour")
    posts_test <- head(posts, n=500L)
    posts_test$index <- 1:nrow(posts_test)
    corp <- corpus(posts_test, docid_field = "index", text_field = "text")
    x <- kwic(tokens(corp, remove_punct = TRUE, remove_numbers = TRUE), 
              pattern = master$Symbol,
              window = 8, case_insensitive = FALSE,
    )
    x$index = x$docname
    add_In_Date <- posts_test[c("index","date_utc")]
    rownames(add_In_Date) <- NULL
    target <- as.data.frame(x)
    target$sentence = paste(target$pre, target$post)
    target
    target_augment <- merge(target,add_In_Date,by="index")
    target_sent <- get_sentences(target$sentence)
    out <- with(target_augment[target_augment$keyword == "ABNB",], sentiment_by( get_sentences(target_augment[target_augment$keyword == "ABNB",]), c("date_utc","pattern")))
    plot(out)
  }))
  output$data <- renderTable({
    head(mtcars[, c("mpg", input$variable), drop = FALSE], input$obs)
  }, rownames = TRUE)
  output$stockdata <- renderTable({
    master <- TTR::stockSymbols(exchange = c("AMEX", "NASDAQ", "NYSE", "ARCA", "BATS", "IEX"))[,c('Name', 'Symbol')]
    posts <- find_thread_urls(subreddit="wallstreetbets", sort_by="new", period="hour")
    posts_test <- head(posts, n=500L)
    posts_test$index <- 1:nrow(posts_test)
    corp <- corpus(posts_test, docid_field = "index", text_field = "text")
    x <- kwic(tokens(corp, remove_punct = TRUE, remove_numbers = TRUE), 
              pattern = master$Symbol,
              window = 8, case_insensitive = FALSE,
    )
    x$index = x$docname
    add_In_Date <- posts_test[c("index","timestamp")]
    rownames(add_In_Date) <- NULL
    target <- as.data.frame(x)
    target$sentence = paste(target$pre, target$post)
    target_augment <- merge(target,add_In_Date,by="index")
  }, rownames = TRUE)
}

shinyApp(ui, server)

