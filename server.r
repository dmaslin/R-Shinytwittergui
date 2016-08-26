# server.R
library(base64enc)
library(bitops)
library(RCurl)
library(RJSONIO)
library(ROAuth)
library(twitteR)
library(stringr)
library(tm)
library(NLP)
library(ggplot2)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
api_key <- 'sjjI4w33PqZz6AR2noLKPziqT' 
api_secret <- 'ZmzBPig0L3KHEVEnS4PZ2PAwy0UsZNttDXTwqCfxhR2fBkZFqt'
token <- '2419123201-mv8GCAvaN3TQ6IgODq1XNyMbZIUxqO7Fc5WecG2' 
token_secret <- 'YCKkImPYyrseusHbCEebPuvmsOf00M5f9dIiWE5N6y3fU'
setup_twitter_oauth(api_key, api_secret, token, token_secret)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#



shinyServer(function(input, output) {
  observeEvent(input$goBabyGo, {
  output$plot <- renderPlot({
    
    tweets <- searchTwitter(input$hash1, n=input$total,since = as.character(input$dates[1]), until = as.character(input$dates[2]), lang="en")
    
    tweets_corpus <- twListToDF(tweets)
    
    
    #clean the corpus
    
    data <- str_replace_all(tweets_corpus$text,"[^[:graph:]]", " ")
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Building a R corpus
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    corpus <- VCorpus(VectorSource(data))
    
    
    #corpus[[2]][1]
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #use tm package to clean the corpus
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    
    #remve the stop words
    #the function stopwords reports a list of about 175 words
    stopwords()[1:10]
    
    #get the first document in the corpus
    
    
    #remove the white space again
    corpus <- tm_map(corpus, stripWhitespace)
    
    writeLines(as.character(corpus[[2]]))
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Bag of Words
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #split a string on the space: bag.of.words <- strsplit(text, " ")
    
    DTM <- DocumentTermMatrix(corpus)
    bag.of.words <- as.vector(DTM$dimnames$Terms)
    DTM <- weightTfIdf(DTM, normalize = TRUE)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Stemming using Porter's algorithm
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    corpus.stem <- tm_map(corpus, stemDocument)
    DTM.stem <- DocumentTermMatrix(corpus.stem)
    bag.of.words.stem <- as.vector(DTM.stem$dimnames$Terms)
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Create a word cloud using weighted tf-idf
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    DTM.tfidf <- DocumentTermMatrix(corpus,
                                    control = list(tokenize=MC_tokenizer, weighting=function(x)
                                      weightTfIdf(x, normalize = TRUE)))
    m <- as.matrix(DTM.tfidf)
    tf <- sort(colSums(m), decreasing = TRUE)
    terms <- names(tf)
    cloud <- data.frame(word = terms, freq = tf)
    
    p1 <- wordcloud(cloud$word,
                   cloud$freq,
                   main = input$hash1,
                   scale=c(6,1),
                   min.freq = 0.5,
                   max.words = 30,
                   random.order= FALSE,
                   random.color= TRUE,
                   colors= brewer.pal(8, 'Dark2')
    )
    print(p1)
  })
output$plot2 <- renderPlot({
    tweets <- searchTwitter(input$hash2, n=input$total,since = as.character(input$dates[1]), until = as.character(input$dates[2]), lang="en")
    
    tweets_corpus <- twListToDF(tweets)
    
    
    #clean the corpus
    
    
    data <- str_replace_all(tweets_corpus$text,"[^[:graph:]]", " ")
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Building a R corpus
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    corpus <- VCorpus(VectorSource(data))
    
    
    #corpus[[2]][1]
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #use tm package to clean the corpus
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    
    #remve the stop words
    #the function stopwords reports a list of about 175 words
    stopwords()[1:10]
    
    #get the first document in the corpus
    
    
    #remove the white space again
    corpus <- tm_map(corpus, stripWhitespace)
    
    writeLines(as.character(corpus[[2]]))
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Bag of Words
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #split a string on the space: bag.of.words <- strsplit(text, " ")
    
    DTM <- DocumentTermMatrix(corpus)
    bag.of.words <- as.vector(DTM$dimnames$Terms)
    DTM <- weightTfIdf(DTM, normalize = TRUE)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Stemming using Porter's algorithm
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    corpus.stem <- tm_map(corpus, stemDocument)
    DTM.stem <- DocumentTermMatrix(corpus.stem)
    bag.of.words.stem <- as.vector(DTM.stem$dimnames$Terms)
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Create a word cloud using weighted tf-idf
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    DTM.tfidf <- DocumentTermMatrix(corpus,
                                    control = list(tokenize=MC_tokenizer, weighting=function(x)
                                      weightTfIdf(x, normalize = TRUE)))
    m <- as.matrix(DTM.tfidf)
    tf <- sort(colSums(m), decreasing = TRUE)
    terms <- names(tf)
    cloud <- data.frame(word = terms, freq = tf)
    
    p2 <- wordcloud(cloud$word,
                   cloud$freq,
                   main = input$hash2,
                   scale=c(6,1),
                   min.freq = 0.5,
                   max.words = 30,
                   random.order= FALSE,
                   random.color= TRUE,
                   colors= brewer.pal(8, 'Dark2')
    )
    print(p2)
})
output$plot3 <-  renderPlot({

    tweets <- searchTwitter(input$hash3, n=input$total,since = as.character(input$dates[1]), until = as.character(input$dates[2]), lang="en")
    
    tweets_corpus <- twListToDF(tweets)
    
    
    
    
    data <- str_replace_all(tweets_corpus$text,"[^[:graph:]]", " ")
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Building a R corpus
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    corpus <- VCorpus(VectorSource(data))
    
    
    #corpus[[2]][1]
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #use tm package to clean the corpus
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    
    #remve the stop words
    #the function stopwords reports a list of about 175 words
    stopwords()[1:10]
    
    #get the first document in the corpus
    
    
    #remove the white space again
    corpus <- tm_map(corpus, stripWhitespace)
    
    writeLines(as.character(corpus[[2]]))
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Bag of Words
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #split a string on the space: bag.of.words <- strsplit(text, " ")
    
    DTM <- DocumentTermMatrix(corpus)
    bag.of.words <- as.vector(DTM$dimnames$Terms)
    DTM <- weightTfIdf(DTM, normalize = TRUE)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Stemming using Porter's algorithm
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    corpus.stem <- tm_map(corpus, stemDocument)
    DTM.stem <- DocumentTermMatrix(corpus.stem)
    bag.of.words.stem <- as.vector(DTM.stem$dimnames$Terms)
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Create a word cloud using weighted tf-idf
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    DTM.tfidf <- DocumentTermMatrix(corpus,
                                    control = list(tokenize=MC_tokenizer, weighting=function(x)
                                      weightTfIdf(x, normalize = TRUE)))
    m <- as.matrix(DTM.tfidf)
    tf <- sort(colSums(m), decreasing = TRUE)
    terms <- names(tf)
    cloud <- data.frame(word = terms, freq = tf)
    
    p3 <- wordcloud(cloud$word,
                   cloud$freq,
                   main = input$hash3,
                   scale=c(6,1),
                   min.freq = 0.5,
                   max.words = 30,
                   random.order= FALSE,
                   random.color= TRUE,
                   colors= brewer.pal(8, 'Dark2')
    )

    
    
    print(p3)
  })
  #close the action button
  })
  
})