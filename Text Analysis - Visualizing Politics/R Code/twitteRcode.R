
####TRUMP TWITTER EXAMPLE####

library(stm)
library(SnowballC)
library(tm)
require(plyr)  
require(stringr)
require(igraph)
library(twitteR)
library(wordcloud)
library(syuzhet)

api_key <- "OJl7tCe1OItxVUCIuuctmKhL2"

api_secret <- "Mt1GpylBSnoJwCsqxuzm32XaagGAzLKojwr0qejBjMLiOY3g1Y"

access_token <- "4129864713-6DznWrZSxVAsrYeZn96karmQ8QGz3fPHEfjScf1"

access_token_secret <- "KteMxvHAFBtPw4DKOh8bhttWHQmH0HvU0b0201M1Y3yCN"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

Trump_tweets <-searchTwitter('Trump', n=3000, lang="en")
df <- do.call(rbind, lapply(Trump_tweets, as.data.frame))
write.csv(df, file = "TrumpTweets.csv")


#Load csv file with 3000 tweets that contain the word "Trump." 
#the individual tweets are in a column called "text"
TrumpTweets <- textfile("TrumpTweets.csv", textField = "text")

#Create a corpus of the tweets
twitterCorpus <- corpus(TrumpTweets) 
#Summarize the corpus
summary(twitterCorpus, n=5)

#Clean up the tweets for processing  
review_text <- str_replace_all(twitterCorpus$documents$texts,"[^[:graph:]]", " ")
#Create a new document-level variable for sentiment score
docvars(twitterCorpus, "Sentiment") <- get_sentiment(review_text, method="afinn")

#Create a data frame called "tokenTweets"
tokenTweets <- summary(twitterCorpus, n=3000)

#Examples of Very Negative Tweets
twitterCorpus[1506]
twitterCorpus[506]
twitterCorpus[2912]

#Examples of Very Positive Tweets
twitterCorpus[2301]
twitterCorpus[1511]

#Plot the distribution of the "tokenTweets" varaible 
ggplot(tokenTweets, aes(x=Sentiment)) + geom_histogram(binwidth=1, fill="#56B4E9", colour="black") + theme_classic() +  theme(panel.background = element_rect(colour = 'black', size = 1, linetype='solid'))  +  labs(x="Sentiment Score of Tweets",y="Frequency") 




