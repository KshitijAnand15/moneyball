library(twitteR)
library(ROAuth)
library(RCurl)
library(httr)

library(stringr)
library(plyr)
library(dplyr)
library(tm)

#library(ggmap)
#library(wordcloud)

### Establishing A Connection - Direct Method

key="BqNr4WZw1f21ImTw5W5CQxJwH"
secret="Eu4oeWlnNDhX8RJEJmTVtr2f19Z9lzhV6MRyNRsi9jURX1QPov"
atoken="826788876131840003-PKNZvxHUq92gaNociKbaWHluU9JTBI2"
asecret="WUqbraBPJpqV4SpZQT9jpOL8tnTMwQtywK30iSJEz3cIx"

setup_twitter_oauth(key, secret, atoken, asecret)

searchTwitter("Supply chain")

### Sentiment Score Function - approach after J. Breen

library("stringr")

library("plyr")

# Function is called sentimentfun
sentimentfun = function(tweettext, pos, neg, .progress='non')
{
  
  # tweettext: vector of text to score
  # pos: vector of words of postive sentiment
  # neg: vector of words of negative sentiment
 
  pos=readLines("positive-words.txt")
  neg=readLines("negative-words.txt")
  
  # create simple array of scores with laply
  scores = laply(tweettext,
                 function(singletweet, pos, neg)
                 {
                   # remove punctuation - using global substitute
                   singletweet = gsub("[[:punct:]]", "", singletweet)
                   # remove control characters
                   singletweet = gsub("[[:cntrl:]]", "", singletweet)
                   # remove digits
                   singletweet = gsub("\\d+", "", singletweet)
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   singletweet = sapply(singletweet, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(singletweet, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos)
                   neg.matches = match(words, neg)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos, neg, .progress=.progress )
  
  # data frame with scores for each sentence
  sentiment.df = data.frame(text=tweettext, score=scores)
  return(sentiment.df)
}

### Using searchTwitter


tweets = searchTwitter("supply chain", n=2000, 
                       lang="en")


# extracting the text
tweettext = sapply(tweets, function(x) x$getText())

## first cleaning stage

tweettext=lapply(tweettext, function(x) iconv(x, "latin1", 
                                              "ASCII", sub=""))
tweettext=lapply(tweettext, function(x) gsub("htt.*",' ',x))
tweettext=lapply(tweettext, function(x) gsub("#",'',x))
tweettext=unlist(tweettext)

# getting the opinion lexicons from working directory
pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")


## apply function score.sentiment
scores = sentimentfun(tweettext, pos, neg, .progress='text')

## extracting further elements (besides text) for the export csv
tweetdate=lapply(tweets, function(x) x$getCreated())
tweetdate=sapply(tweetdate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

isretweet=sapply(tweets, function(x) x$getIsRetweet())

retweetcount=sapply(tweets, function(x) x$getRetweetCount())

favoritecount=sapply(tweets, function(x) x$getFavoriteCount())

## Creating the Data Frame
data=as.data.frame(cbind(ttext=tweettext,
                         date=tweetdate,
                         isretweet=isretweet, 
                         retweetcount=retweetcount,
                         favoritecount=favoritecount,
                         score = scores$score,
                         product = "Supply Chain"))
                         
View(scores)
## remove duplicates
data2 = duplicated(data[,1])

data$duplicate = data2

write.csv(data,file="supplychain.csv")

## create file to wd
write.csv(data, file= "data1.csv")
