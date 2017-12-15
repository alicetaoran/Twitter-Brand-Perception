library(devtools)
library(twitteR)
library(tidytext)
library(tidyverse)
library(dplyr)
library(scales)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(plotly)




#####################
#### Data Import ####
#####################


### 1 Twitter Setup

api_key <- 	"RrPTCIryCswqP35AFeanh73cV"
api_secret <- "w4JEhIMHsEArgaandqAtTqhc610kRvenP8e8fdXfSRCnuLVsn9"
access_token <- "927638841925742593-3NjhdkxNIJRbGQ8PPDIMIpFskIMFpPd"
access_token_secret <- "Qfd1mKmPbi0E6O6cmblVG4Fmi6CQceG1CtbyQKR28nAAY"

setup_twitter_oauth(api_key, 
                    api_secret, 
                    access_token, 
                    access_token_secret)



### 2 Get the tweets

Alexatweet <- searchTwitter("Amazon Alexa", 
                        n=3000, lang="en", since="2014-01-01")
Alexatweetdf <- twListToDF(Alexatweet)


GoogleHometweet <- searchTwitter("Google Home", 
                             n=3000, lang="en", since="2014-01-01")
GoogleHometweetdf <- twListToDF(GoogleHometweet)


AppleHometweet <- searchTwitter("Apple HomePod", 
                                  n=3000, lang="en", since="2014-01-01")
AppleHometweetdf <- twListToDF(AppleHometweet)


### 3 we use write.csv to store our data into csv file
### this enables us to process the same data,
### as each time we run searchTwitter, it will return
### different tweets


#####################
## Text Preprocess ##
#####################

### 0 import the stored data

Alexadf<-read.csv("Alexa.csv")
GHdf<-read.csv("GoogleHome.csv")
AHdf<-read.csv("AppleHome.csv")

### 1 tokenize the tweet text
### we use unnest tokens function to help us
### break the sentences into words

Alexatext<-select(Alexadf,text)
Alexatext$text<-as.character(Alexatext$text)
Alexatext<- Alexatext %>% 
  unnest_tokens(word, text)


GHtext<-select(GHdf,text)
GHtext$text<-as.character(GHtext$text)
GHtext<- GHtext %>% 
  unnest_tokens(word, text)


AHtext<-select(AHdf,text)
AHtext$text<-as.character(AHtext$text)
AHtext<- AHtext %>% 
  unnest_tokens(word, text)


### 2 remove stop words
### The stop words package enables us to remove
### no-meaning or conjunction words
### besides the stop words
### I also created my own stop words

mystopwords<-c("https","rt","htt","votedougjones")

mystopwords<-as.data.frame(mystopwords)
colnames(mystopwords)<-"word"
mystopwords$word<-as.character(mystopwords$word)


Alexa<-Alexatext %>%
  anti_join(stop_words) %>% 
  anti_join(mystopwords) %>%
  count(word, sort = TRUE) %>%
  filter(str_detect(word,"^[a-z']+$"))

### as we can see the top words
### amp: AppDynamics
### Jones and Alex: Jones grilled an Amazon Echo 
### on his show on Friday, asking Alexa — 
### the device’s personal assistant — about its connections to the CIA.
### Michael Schaub (author in NYT): Amazon will open in a Manhattan mall in May 2017


GH<-GHtext %>%
  anti_join(stop_words) %>% 
  anti_join(mystopwords) %>%
  count(word, sort = TRUE) %>%
  filter(str_detect(word,"^[a-z']+$"))

### mini: mini smart speaker
### MKBHD: Marques Brownlee (Web Video Producer)
### cnet: a news website dedicated to technology
### votedougjones: an American attorney, prosecutor, and politician


AH<-AHtext %>%
  anti_join(stop_words) %>% 
  anti_join(mystopwords) %>%
  count(word, sort = TRUE) %>%
  filter(str_detect(word,"^[a-z']+$"))

### gleamapp: Gleam's Apple HomePod Giveaway campaign




#####################
######## EDA ########
#####################


Alexa %>%
  filter(n > 210) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill="cornflowerblue") +
  xlab(NULL) +
  coord_flip()+
  ggtitle("Alexa Words Frequency")+
  ylab("Count")


GH %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill="brown3") +
  xlab(NULL) +
  coord_flip()+
  ggtitle("Google Home Words Frequency")+
  ylab("Count")


AH %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill="darkgray") +
  xlab(NULL) +
  coord_flip()+
  ggtitle("Apple Home Words Frequency")+
  ylab("Count")


Alexa %>%
  with(wordcloud(word, n, max.words = 50, colors = brewer.pal(8, "Dark2")))

GH %>%
  with(wordcloud(word, n, max.words = 50, colors = brewer.pal(8, "Dark2")))

AH %>%
  with(wordcloud(word, n, max.words = 50, colors = brewer.pal(8, "Dark2")))



#####################
### Test Analysis ###
#####################


### 1 sentiment analysis

get_sentiments("bing")
get_sentiments("nrc")


### Alexa

AlexaST <- Alexa %>%
  inner_join(get_sentiments("bing")) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

A=ggplot(data = AlexaST,aes(word,sentiment,fill=sentiment))+
  geom_col()+theme(axis.text.x = element_blank())+
  ggtitle("Alexa Sentiment Analysis")

ggplotly(A)


### Google Home

GHst <- GH %>%
  inner_join(get_sentiments("bing")) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

B=ggplot(data = GHst,aes(word,sentiment,fill=sentiment))+
  geom_col()+theme(axis.text.x = element_blank())+
  ggtitle("GoogleHome Sentiment Analysis")

ggplotly(B)


### Apple Home

AHst <- AH %>%
  inner_join(get_sentiments("bing")) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

C=ggplot(data = AHst,aes(word,sentiment,fill=sentiment))+
  geom_col()+theme(axis.text.x = element_blank())+
  ggtitle("AppleHome Sentiment Analysis")

ggplotly(C)




### 2 correlation analysis

allproduct<- bind_rows(mutate(GH, product = "Google Home"),
                       mutate(AH, product = "Apple Home"), 
                       mutate(Alexa, product = "Alexa")) 


allpro <- allproduct %>% 
  mutate(proportion = round(n / sum(n),5)) %>% 
  select(-n) %>% 
  spread(product, proportion) %>% 
  gather(product, proportion, "Google Home":"Apple Home") %>%
  na.omit(proportion)


cor.test(data = allpro[allpro$product == "Google Home",],
         ~ proportion + Alexa)

cor.test(data = allpro[allpro$product == "Apple Home",], 
         ~ proportion + Alexa)



