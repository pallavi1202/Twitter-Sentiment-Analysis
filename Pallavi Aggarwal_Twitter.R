#install.packages("twitteR")
#install.packages("ROAuth")
library(twitteR)
library(ROAuth)
#install.packages("bitops")
library("RCurl"); library("bitops"); library("rjson")
library(tm)
library(SnowballC)
#install.packages("tidytext")
library(tidytext)
#install.packages("wordcloud")
library(wordcloud)


APIkey <- "GyKdqisjN99oPFHPxiIJ5Hci6"
APISecretKey  <- "sTPPcHsuLkARbmwNNZYd6VBH1mmsJZtQTz8hEXiQdtwZ5qPQML"
AccessToken <- "802043328644530176-7Om9D46p68zSYqEr5r24ItzrvNA5Ypn"
AccessTokenSecret <- "rh1vqPXMZ0s9Wn1dnD5R7vEcKw4gwx9Xv1CQc0baaW04Z"

setup_twitter_oauth(APIkey, APISecretKey, AccessToken, AccessTokenSecret)
save(list=(c("APIkey","APISecretKey")), file="twitter_credentials.RData")


tweetsRedSox <- searchTwitter('#RedSox', n = 500)
tweetsYankees <- searchTwitter('#Yankees', n = 500)

#Function to display tweets
display.tweet <- function (tweet) {
  cat("Screen name:", tweet$getScreenName(), 
      "\nText:", tweet$getText(), "\n\n")
}

#sentiment Analysis
# Lexicons
sentiments

pos.words = scan('positive-words.txt',
                 what='character',
                 comment.char=';')

neg.words = scan('negative-words.txt',  
                 what='character', 
                 comment.char=';')


sentiment <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  text <- tolower(text)
  # split the text into a vector of words
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  # find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  # find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  # calculate the sentiment score
  p <- sum(pos.matches)
  n <- sum(neg.matches)
  if (p==0 & n==0)
    return(NA)
  else 
    return(p-n)
}


# Analysis for Red Sox
for (t in tweetsRedSox) {
  display.tweet(t)
}


tweets.text.RedSox <- lapply(tweetsRedSox, function(t){t$getText()})

data.source.RedSox <- VectorSource(tweets.text.RedSox)
data.corpus.RedSox <- Corpus(data.source.RedSox)
tdm.RedSox <- TermDocumentMatrix(data.corpus.RedSox)
inspect(data.corpus.RedSox[1:2])

m.RedSox <- as.matrix(tdm.RedSox)

# calculate the frequency of words 
wordFreq.RedSox <- rowSums(m.RedSox)

# Examine part of the frequencies
cbind(wordFreq.RedSox[150:160])

# Sort the words by descending order of frequency
wordFreq.RedSox <- sort(wordFreq.RedSox, decreasing=TRUE)

# Examine the top ten words
cbind(wordFreq.RedSox[1:10])

# frequent terms
findFreqTerms(tdm.RedSox, lowfreq=15)

# word cloud

palette <- brewer.pal(8,"Dark2")
palette

set.seed(137)
wordcloud(words=names(wordFreq.RedSox), 
          freq=wordFreq.RedSox, 
          min.freq=3, 
          random.order=F,
          colors=palette)


#Sentimental Analysis for Red Sox
sentiment(tweets.text.RedSox, pos.words, neg.words)


scores.RedSox <- sapply(tweets.text.RedSox, 
                 sentiment, 
                 pos.words, neg.words)

table(scores.RedSox)
par(mar=c(1,1,1,1))

barplot(table(scores.RedSox), 
        xlab="Score", ylab="Count",
        col="cyan")


# Data frame of scores and tweets

vector <- sapply(tweets.text.RedSox,
                      function (t) {(t)})
x <- data.frame(Score=scores.RedSox, Text=vector)
View(x)



#Yankees
for (i in tweetsYankees) {
  display.tweet(i)
}


tweets.text.Yankees <- lapply(tweetsYankees, function(i){i$getText()})

data.source.Yankees <- VectorSource(tweets.text.Yankees)
data.corpus.Yankees <- Corpus(data.source.Yankees)
tdm.Yankees <- TermDocumentMatrix(data.corpus.Yankees)

inspect(data.corpus.Yankees[1:2])

m.Yankees <- as.matrix(tdm.Yankees)

# calculate the frequency of words 
wordFreq.Yankees <- rowSums(m.Yankees)

# Examine part of the frequencies
cbind(wordFreq.Yankees[150:160])

# Sort the words by descending order of frequency
wordFreq.Yankees <- sort(wordFreq.Yankees, decreasing=TRUE)

# Examine the top ten words
cbind(wordFreq.Yankees[1:10])

# frequent terms
findFreqTerms(tdm.Yankees, lowfreq=15)

# word cloud

palette <- brewer.pal(8,"Dark2")
palette

set.seed(137)
wordcloud(words=names(wordFreq.Yankees), 
          freq=wordFreq.Yankees, 
          min.freq=3, 
          random.order=F,
          colors=palette)


#Sentimental Analysis for Yankees
sentiment(tweets.text.Yankees, pos.words, neg.words)


scores.Yankees <- sapply(tweets.text.Yankees, 
                        sentiment, 
                        pos.words, neg.words)

table(scores.Yankees)
par(mar=c(1,1,1,1))

barplot(table(scores.Yankees), 
        xlab="Score", ylab="Count",
        col="blue")


# Data frame of scores and tweets

vector <- sapply(tweets.text.Yankees,
                 function (i) {(i)})
y <- data.frame(Score=scores.Yankees, Text=vector)
View(y)

