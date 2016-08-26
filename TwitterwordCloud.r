



twitterWordcloud <- function(hash, total, dates){
  
  #Send out the crawler and create corpus 
tweets <- searchTwitter(hash, n=total,since = as.character(dates[1]), until = as.character(dates[2]), lang="en")

tweets_corpus <- twListToDF(tweets)


#clean the corpus
library(tm)
library(NLP)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)

data <- tweets_corpus$text


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Building a R corpus
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

corpus <- VCorpus(VectorSource(data))
print(corpus)

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
corpus[[1]][1]

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

w <- wordcloud(cloud$word,
          cloud$freq,
          main = hash,
          scale=c(6,1),
          min.freq = 0.5,
          max.words = 30,
          random.order= FALSE,
          random.color= TRUE,
          colors= brewer.pal(8, 'Dark2')
)
return(w)
}

