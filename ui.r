library(shiny)

shinyUI(fluidPage(
  titlePanel("TwitterWordcloud"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select the hashtags you wish to have made into a wordcloud. 
               They will be displayed in the area to the right."),
    
      textInput("hash1", label = "First Hashtag", "finals"),
      textInput("hash2", label = "Second Hashtag", "exam"),
      textInput("hash3", label = "Third Hashtag", "test"),
      numericInput("total", label = "Number of Tweets you want to try and get", 10, min = 10),
      dateRangeInput("dates", label = h3("Date range")),
    actionButton("goBabyGo", "Plot that!")),
      
    
    mainPanel(plotOutput("plot"),
              plotOutput("plot2"),
              plotOutput("plot3"))
  )
))