#library(ROAuth)
#requestURL <- "https://api.twitter.com/oauth/request_token"
#accessURL <- "https://api.twitter.com/oauth/access_token"
#authURL <- "https://api.twitter.com/oauth/authorize"
#consumerKey <- "bDp95I7S9arvEqPb8SccZ20Nn"
#consumerSecret <- "piocWoAvWB4bcNbOW9VIaF0YyT2TUpi6TOakHg3voV4iiXXlQM"
#my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                             requestURL = requestURL, accessURL = accessURL, authURL = authURL)

### Now you need a pin from dev.twitter.com
#my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

#save(my_oauth, file = "my_oauth.Rdata")




install.packages("RCurl")
install.packages("bitops")
install.packages("rjson")

library(streamR)

load("my_oauth.Rdata")

library(twitteR)
install.packages("qdap")
library(qdap)
install.packages("ggplot2")
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

### United States
filterStream("tweetUS.json",language ='en',
             locations=c(-125,25,-75,49),tweets=7000,
             
             oauth=my_oauth) 
tweets1.df <- parseTweets("tweetUS.json",verbose=FALSE)




geocode("Los Angeles")


library(RgoogleMaps)
library(ggmap)
library(ggplot2)
#library(maptools)
library(sp)
library(grid)

devtools::install_github("dkahle/ggmap")
devtools::install_github("hadley/ggplot2")
points <- data.frame(x=as.numeric(tweets1.df$place_lon),
                     y=as.numeric(tweets1.df$place_lat))



install.packages("maps")
library(maps)
map.data <- map_data("state") 
ggplot(map.data)+
  geom_map(aes(map_id=region),
           map=map.data,
           fill="white",
           color="grey20",size=0.25)+
  expand_limits(x=map.data$long,y=map.data$lat)+
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        plot.background=element_blank(),
        plot.margin=unit(0*c(-1.5,-1.5,-1.5,-1.5),"lines"))+
  geom_point(data=points,
             aes(x=x,y=y),size=1,
             alpha=1/5,color="darkblue")


#### Los Angeles 
filterStream("tweetL.json",language ='en',
             locations=c(-120,32,-117,35),tweets=5000,
             
             oauth=my_oauth) 

tweets.df <- parseTweets("tweetL.json",verbose=FALSE)

length(grep("Trump", tweets.df$text, ignore.case=TRUE))

points <- data.frame(x=as.numeric(tweets.df$place_lon),
                     y=as.numeric(tweets.df$place_lat))


map.data <- get_map("Los Angeles",zoom=8) 
ggmap(map.data)+ geom_point(data=points,
                            aes(x=x,y=y),size=1,
                            color="darkblue")
##### 
pos <- scan('positive.txt',what='character',comment.char=';')
neg <- scan('negative.txt',what='character',comment.char=';')
t1 <- tweets.df$text

p <- function(t,positive){
  lat = vector(mode = "numeric",length=0)
  lon = vector(mode = "numeric",length=0)
  
  for (i in (1:length(positive))){
    for (j in (1:length(t$text))){
         l = length(grep(positive[i], t$text[j], ignore.case=TRUE))
         if(l!=0){
           lat = c(lat,t$place_lat[j])
           lon = c(lon,t$place_lon[j])
         }
      
       }
  }
  c = cbind(lat,lon)
    
  return(c)
}


d <- tweets.df[grep("Trump", tweets.df$text, ignore.case=TRUE),]




p1<- p(d,pos)
p1 <- as.data.frame(p1)
View(p1)
n1 <- n(tweets.df,neg)
n1 <- as.data.frame(n1)


newpoint <- data.frame(x=as.numeric(d$place_lon),
                     y=as.numeric(d$place_lat))

map.data <- get_map("Los Angeles",zoom=8) 
ggmap(map.data)+ geom_point(data=newpoint,aes(x=x,y=y),size=1,
                                            color="darkblue") +
  geom_point(data=p1,
             aes(x=as.numeric(p1$lon),y=as.numeric(p1$lat)),size=1,
             color="red") 
                           
  
  


#####Distribution of words per tweet
words_list = strsplit(tweets.df$text, " ")
# words per tweet
words_per_tweet = sapply(words_list, length)
# barplot
barplot(table(words_per_tweet), border=NA,
        main="Distribution of words per tweet", cex.main=1)



##########
library(devtools)
library(twitteR)
api_key <- 	"bDp95I7S9arvEqPb8SccZ20Nn"
api_secret <- "piocWoAvWB4bcNbOW9VIaF0YyT2TUpi6TOakHg3voV4iiXXlQM"
access_token <- "793892699619426304-j8D2ivAINuq7fk7O2m9pg1RE7OQCHnP"
access_token_secret <- "Aa4fUH303hHdtxdWwhHpCabs9G6jwBcZTr8Jya4ubwL0c"


setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

####Los Angeles
geocode("Los Angeles")

tweets1<- searchTwitter("Trump OR  #Trump", n=7000,  geocode="37.77493,-122.4194,50mi")
df1 <- twListToDF(tweets1) 




#### numbers of tweets in each hour
count <- function(x){
  b <- vector(mode = "numeric",length=0)
  for (i in (1:length(x))){
    a = strsplit(as.character(x[i]),' ')
    a = a[[1]][2]
    a = strsplit(a,":")
    b = c(b,a[[1]][1])
    
  }
  return(b)
  
  }
hour <-count(df1$created)
df1$hour <- hour
ggplot(df1,aes(x=hour))+geom_bar(aes(y=(..count..)))
######Sentiment
require(devtools)
install_url("http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
install.packages("sentiment")
install.packages("Rstem")
install.packages("tm")
install.packages("NLP")
require(sentiment)
ls("package:sentiment")

txt1 = sapply(tweets1, function(x) x$getText())

# remove retweet entities
txt1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", txt1)
# remove at people
txt1 = gsub("@\\w+", "", txt1)
# remove punctuation
txt1 = gsub("[[:punct:]]", "", txt1)
# remove numbers
txt1 = gsub("[[:digit:]]", "", txt1)
# remove html links
txt1 = gsub("http\\w+", "", txt1)
# remove unnecessary spaces
txt1 = gsub("[ \t]{2,}", "", txt1)
txt1 = gsub("^\\s+|\\s+$", "", txt1)

# define "tolower error handling" function 
try.error = function(x)
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
# lower case using try.error with sapply 
txt1 = sapply(txt1, try.error)

# remove NAs in some_txt
txt1 = txt1[!is.na(txt1)]
names(txt1) = NULL

# classify emotion
class_emo = classify_emotion(txt1, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"
#
class_pol = classify_polarity(txt1, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]
# data frame with results
sent_df = data.frame(text=txt1, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))



# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets")

# Word clouds!!!

txt1 <- gsub(" (the|to|is|its|on|and|of|in|for|a|after|from|but|that|about|was|you|it|i|with|if|so|can|this|where) "," ",txt1)

require(tm)
require(wordcloud)
require(RColorBrewer)
ap.corpus <- Corpus(DataframeSource((data.frame(as.character(txt1)))))
ap.corpus <- tm_map(ap.corpus, removePunctuation)
ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, stopwords("english")))
ap.tdm <- TermDocumentMatrix(ap.corpus) 
ap.m <- as.matrix(ap.tdm) 
ap.v <- sort(rowSums(ap.m),decreasing=TRUE) 
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,max.words=200, random.order=FALSE, rot.per=.15, colors=brewer.pal(8,"Dark2"))




###### Chicago


filterStream("tweetC.json",language ='en',
             locations=c(-88,41,-87,42),tweets=5000,
             
             oauth=my_oauth) 
tweets5.df <- parseTweets("tweetC.json",verbose=FALSE)

points5 <- data.frame(x=as.numeric(tweets5.df$place_lon),
                     y=as.numeric(tweets5.df$place_lat))

map.data2 <- get_map("Chicago",zoom=9) 
ggmap(map.data2)+ geom_point(data=points5,
                            aes(x=x,y=y),size=1,
                            color="darkblue")

#########
pos <- scan('positive.txt',what='character',comment.char=';')
neg <- scan('negative.txt',what='character',comment.char=';')
t5 <- tweets5.df$text

p <- function(t,positive){
  lat = vector(mode = "numeric",length=0)
  lon = vector(mode = "numeric",length=0)
  #count = 0
  for (i in (1:length(positive))){
    for (j in (1:length(t$text))){
      l = length(grep(positive[i], t$text[j], ignore.case=TRUE))
      if(l!=0){
        lat = c(lat,t$place_lat[j])
        lon = c(lon,t$place_lon[j])
      }
      
    }
  }
  c = cbind(lat,lon)
  
  return(c)
}



p1<- p(tweets5.df,pos)
p1 <- as.data.frame(p1)






map.data <- get_map("Chicago",zoom=9) 
ggmap(map.data)+ geom_point(data=points,aes(x=x,y=y),size=1,
                            color="darkblue")+
  geom_point(data=p1,
             aes(x=as.numeric(p1$lon),y=as.numeric(p1$lat)),size=1,
             color="red") 

#######Distribution of words per tweet
words_list = strsplit(tweets5.df$text, " ")

# words per tweet
words_per_tweet = sapply(words_list, length)
# barplot
barplot(table(words_per_tweet), border=NA,
        main="Distribution of words per tweet", cex.main=1)


tweets5<- searchTwitter("Trump OR  #Trump", n=7000,  geocode="41.87811,-87.6298,20mi")
df5 <- twListToDF(tweets5)

##### numbers of tweets in each hour
hour <-count(df5$created)
df5$hour <- hour
ggplot(df5,aes(x=hour))+geom_bar(aes(y=(..count..)))
lines(df5,dt(df5,df=1),type="l",lwd=2,col="yellow")

########
txt5 = sapply(tweets5, function(x) x$getText())
txt5 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", txt5)
txt5 = gsub("@\\w+", "", txt5)
txt5 = gsub("[[:punct:]]", "", txt5)
txt5 = gsub("[[:digit:]]", "", txt5)
txt5 = gsub("http\\w+", "", txt5)
txt5 = gsub("[ \t]{2,}", "", txt5)
txt5 = gsub("^\\s+|\\s+$", "", txt5)


txt5 = sapply(txt5, try.error)
txt5 = txt5[!is.na(txt5)]
names(txt5) = NULL

# classify emotion
class_emo5 = classify_emotion(txt5, algorithm="bayes", prior=1.0)
emotion5 = class_emo5[,7]
emotion5[is.na(emotion5)] = "unknown"
class_pol5 = classify_polarity(txt5, algorithm="bayes")
# get polarity best fit
polarity5 = class_pol5[,4]
# data frame with results
sent_df5 = data.frame(text=txt5, emotion=emotion5,
                      polarity=polarity5, stringsAsFactors=FALSE)

# sort data frame
sent_df5 = within(sent_df5,
                  emotion5 <- factor(emotion5, levels=names(sort(table(emotion5), decreasing=TRUE))))


# plot distribution of polarity
ggplot(sent_df5, aes(x=polarity5)) +
  geom_bar(aes(y=..count.., fill=polarity5)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets")




library(wordcloud)

txt5 <- gsub(" (the|to|is|its|on|and|of|in|for|a|after|from|but|that|about|was|you|it|i|with|if|so|can|this|where) "," ",txt1)
ap.corpus <- Corpus(DataframeSource((data.frame(as.character(txt5)))))
ap.corpus <- tm_map(ap.corpus, removePunctuation)
ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, stopwords("english")))

ap.tdm <- TermDocumentMatrix(ap.corpus) 
ap.m <- as.matrix(ap.tdm) 
ap.v <- sort(rowSums(ap.m),decreasing=TRUE) 
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,max.words=100, random.order=FALSE, rot.per=.15, colors=brewer.pal(8,"Dark2"))



##### Boston

filterStream("tweetB.json",language ='en',
             locations=c(-72,41,-70,43),tweets=2000,
             
             oauth=my_oauth) 
tweets3.df <- parseTweets("tweetB.json",verbose=FALSE)

points3 <- data.frame(x=as.numeric(tweets3.df$place_lon),
                      y=as.numeric(tweets3.df$place_lat))

map.data3 <- get_map("Boston",zoom=9) 
ggmap(map.data3)+ geom_point(data=points3,
                             aes(x=x,y=y),size=1,
                             color="darkblue")

#######Distribution of words per tweet
words_list = strsplit(tweets3.df$text, " ")

# words per tweet
words_per_tweet = sapply(words_list, length)
# barplot
barplot(table(words_per_tweet), border=NA,
        main="Distribution of words per tweet", cex.main=1)

######### tweets refered to Trump
tweets3<- searchTwitter("Trump OR  #Trump", n=7000,  geocode="42.36008,-71.05888,20mi")
df3 <- twListToDF(tweets3)

##### numbers of tweets in each hour
hour <-count(df3$created)
df3$hour <- hour
ggplot(df3,aes(x=hour))+geom_bar(aes(y=(..count..)))


########
txt3 = sapply(tweets3, function(x) x$getText())
txt3 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", txt3)
txt3 = gsub("@\\w+", "", txt3)
txt3 = gsub("[[:punct:]]", "", txt3)
txt3 = gsub("[[:digit:]]", "", txt3)
txt3 = gsub("http\\w+", "", txt3)
txt3 = gsub("[ \t]{2,}", "", txt3)
txt3 = gsub("^\\s+|\\s+$", "", txt3)


txt3 = sapply(txt3, try.error)
txt3 = txt3[!is.na(txt3)]
names(txt3) = NULL

# classify emotion
class_emo3 = classify_emotion(txt3, algorithm="bayes", prior=1.0)
emotion3 = class_emo3[,7]
emotion3[is.na(emotion3)] = "unknown"
class_pol3 = classify_polarity(txt3, algorithm="bayes")
# get polarity best fit
polarity3 = class_pol3[,4]
# data frame with results
sent_df3 = data.frame(text=txt3, emotion=emotion3,
                      polarity=polarity3, stringsAsFactors=FALSE)

# sort data frame
sent_df3 = within(sent_df3,
                  emotion3 <- factor(emotion3, levels=names(sort(table(emotion3), decreasing=TRUE))))


# plot distribution of polarity
ggplot(sent_df3, aes(x=polarity3)) +
  geom_bar(aes(y=..count.., fill=polarity3)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets")




library(wordcloud)

txt3 <- gsub(" (the|to|is|its|on|and|of|in|for|a|after|from|but|that|about|was|you|it|i|with|if|so|can|this|where) "," ",txt1)
ap.corpus <- Corpus(DataframeSource((data.frame(as.character(txt3)))))
ap.corpus <- tm_map(ap.corpus, removePunctuation)
ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, stopwords("english")))

ap.tdm <- TermDocumentMatrix(ap.corpus) 
ap.m <- as.matrix(ap.tdm) 
ap.v <- sort(rowSums(ap.m),decreasing=TRUE) 
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,max.words=300, random.order=FALSE, rot.per=.15, colors=brewer.pal(8,"Dark2"))



#### Comparison.cloud
la = paste(txt1, collapse=" ")
ch = paste(txt5, collapse=" ")
bo = paste(txt3, collapse=" ")

all = c(la,ch,bo)
all = removeWords(all, c(stopwords("english"),"trump","the","a","he","an","that","to"))

# create corpus
corpus = Corpus(VectorSource(all))

# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)

# add column names
colnames(tdm) = c("Los Angeles", "Chicago", "Boston")
# comparison cloud
comparison.cloud(tdm, random.order=FALSE, 
                 colors = c("#00B2FF", "red", "#FF0099"),
                 title.size=1.5, max.words=400)



