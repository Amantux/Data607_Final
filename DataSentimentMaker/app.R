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

library(BatchGetSymbols)

library(dplyr)

library(tm)

library(SnowballC)
library(DT)

library(plotly)
library(ggplot2)


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
    #selectInput("tp", "Time Periord to Search:", c("hour" = "hour", "day" = "day", "week" = "week", "month" = "month", "year" = "year", "all" = "all")),
    #selectInput("sm", "Sort Method:", c("relevance" = "relevance", "comments" = "comments", "new" = "new", "hot" = "hot", "top" = "top")),
    textInput(
      inputId = "maxPull",
      label = "Enter Maximum Number of Posts to pull: ",
    ),
    #textInput(
    #  inputId = "term",
    #  label = "Enter Search Term: ",
    # ),
    # actionButton(
    #  inputId = "submit_search",
    #  label = "Submit"
    #),
    #sliderInput("obs", "Number of observations:",
    #            min = 0, max = 100000, value = 100
    #),
    #dateRangeInput("dates", label = h3("Date range")),
    #add in ordinal or random sample
    #add in user selectable random sample
    #checkboxGroupInput("variable", "Variables to show:",
    #                   c("Cylinders" = "cyl",
    #                     "Transmission" = "am",
    #                     "Gears" = "gear")),
    #actionButton(
    #  inputId = "Execute",
    #  label = "Run"
    # ),
    downloadButton("downloadData", "Download"),
    
  ),
  
  mainPanel(
  #tableOutput("rawData"),
  DT::dataTableOutput("prettyTable"),
  #textOutput("tp"),
  #textOutput("StockFilter"),
  #textOutput("sm"),
  #textOutput("term"),
  #textOutput("date"),
  #textOutput("maxPull"),
  #plotOutput("plot"),
  plotlyOutput("plotlyplot"),
  #DT::dataTableOutput("stockdata")
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
  target <- data.frame()
  slimData <- function() {
    slim_posts <- head(all_posts, n=max({input$maxPull}, 10))
    slim_posts$index <- 1:nrow(slim_posts)
    corp <- corpus(slim_posts, docid_field = "index", text_field = "text")
    x <- kwic(tokens(corp, remove_punct = TRUE, remove_numbers = TRUE), 
              pattern = master$Symbol,
              window = 8, case_insensitive = FALSE,
    )
    print("I'm Here3")
    x$index = x$docname
    add_In_Date <- slim_posts[c("index","date_utc")]
    rownames(add_In_Date) <- NULL
    target <- as.data.frame(x)
    target$sentence = paste(target$pre, target$post)
    target <- merge(target,add_In_Date,by="index")
    target$sentence <- str_replace_all(target$sentence, pattern = '[:punct:]',  replacement =" ")
    target
  }
  
  sentimentData <- function() {
    if (is.null(target) || dim(target)[1] == 0) { 
     target <- slimData()
    }
    target$sentiment <- sentiment(target$sentence)$sentiment
    sentimentHolder <- target[c("keyword","date_utc", "sentiment")]
    sentimentHolder = setNames(sentimentHolder, c("ticker", "ref.date","sentiment"))
    sentimentHolder <- sentimentHolder %>% group_by(ticker, ref.date) %>% summarise(mean(sentiment))
    
    sentimentHolderback <- sentimentHolder
    sentimentHolder$ref.date = as.Date(sentimentHolder$ref.date)
    sentimentHolder
    max(sentimentHolder$ref.date)+2
    min(sentimentHolder$ref.date)
    l.out <- BatchGetSymbols(tickers = unique(sentimentHolder$ticker),
                             first.date = as.Date(min(sentimentHolder$ref.date)),
                             last.date = min(as.Date(max(sentimentHolder$ref.date)+2,Sys.Date())), do.cache=FALSE)
    l.out
    priceData <- l.out$df.tickers 
    mergedData <- merge(priceData, sentimentHolder, all.x = TRUE)
    mergedData$`mean(sentiment)`[is.na(mergedData$`mean(sentiment)`)] <- 0
    mergedData$sentiment <- mergedData$`mean(sentiment)`
    #summary(comparedf(mergedData, priceData, by = "ticker"))
    mergedData
  }
  
  mergedData <- data.frame()
  plotlyMaker <- function() {
    if (is.null(mergedData) || dim(mergedData)[1] == 0) { 
      mergedData <- sentimentData()
    }
    dateSentiment <- mergedData[c("ref.date","ticker", "sentiment")]
    print(head(dateSentiment))
    t <- ggplot(dateSentiment[dateSentiment$sentiment != 0,]) + aes(x=ref.date, y=sentiment, color = ticker) + geom_boxplot() + stat_summary(fun = "median",
                                                                                                                                             geom = "point",
                                                                                                                                             color = "Orange") +               
      stat_summary(fun = "mean",
                   geom = "point", 
                   colour = "red")
    
    plot1 <- ggplotly(t)
    datePrice <- mergedData[c("ref.date","ticker", "price.open", "price.close")]
    plot2 <- ggplotly(ggplot(datePrice, aes(x = ref.date, y = price.open, colour = ticker)) +
                        geom_line(), )
    plot3 <- ggplotly(ggplot(datePrice, aes(x = ref.date, y = price.close, colour = ticker)) +
                        geom_line())
    fig <- subplot(plot1, plot2, plot3, nrows = 3, shareX = TRUE) %>%  layout(hovermode = "x unified")
  }
  output$plotlyplot <- renderPlotly(plotlyMaker())
  
  output$rawData <- renderTable(slimData())
  output$prettyTable = DT::renderDataTable({
    sentimentData()
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
  output$downloadData <- downloadHandler(
    filename <- function() {
      paste("Data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(sentimentData(), file, row.names = FALSE)
    }
  )

  output$data <- renderTable({
    head(mtcars[, c("mpg", input$variable), drop = FALSE], input$obs)
  }, rownames = TRUE)

}

shinyApp(ui, server)

