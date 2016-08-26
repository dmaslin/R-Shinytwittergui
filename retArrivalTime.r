

retArrivalTime <- function (hashtag, total, dates){

  
# Pull 500 tweets with hashtag climate
tweets <- searchTwitter(hashtag, n=total, since = as.character(dates[1]), until = as.character(dates[2]))
tweets.df <- twListToDF(tweets)


#############################################
# Step 2: calculate next tweet arrival time 
############################################

# Convert timestamps to an integer vector and sort  
Created.sort <- sort(as.integer(tweets.df[,'created']))

# find the difference in seconds between each pair
Created.diff <- diff(Created.sort)


#####################################
# Step 3: histogram the distribution
####################################


source("CFD.R")
list <- CFD(Created.diff, 1, 1, 'Seconds')


x <- c(min(Created.diff) : max(Created.diff)) #vector
y <- as.data.frame(list[4])$cumProb #vector
tag <- c(hashtag)
legend <- as.factor(rep(tag, times=length(y))) #factor vector of same length of y

dat <- data.frame(x,y,legend) 

return(list(Created.diff,dat))

}