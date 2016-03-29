install.packages("quanteda")
install.packages("ggplot2")
install.packages("syuzhet")
install.packages("stringr")
install.packages("foreign")

####Load in Necessary Libraries 
library(quanteda)
library(stringr)
library(syuzhet)
library(ggplot2)
library(foreign)

###############CREATE A CORPUS####################

##Upload files
## "dvsep" is seprating the name of the document into three variables 
#You will need to specify your own file path!

speeches <- textfile("~/Dropbox/Text Analysis Workshop/Announcements/*.txt",  docvarsfrom="filenames", dvsep="_", docvarnames=c("Candidate", "Party", "Date"))

##Create corpus called "speechCorpus"
speechCorpus <- corpus(speeches) 

#Get Summary of Documents 
#This will look a normal structured text document
summary(speechCorpus, n=5) 
#Corpus consisting of 21 documents, showing 5 documents

#View a single document within the corpus:
texts(speechCorpus)[2] #Ben Carson's announcement speech
texts(speechCorpus)[5] #Hillary Clinton's announcement speech

###############DESCRIPTIVE STATISTICS####################

#Create a dataframe with the basic descriptive statistics from the 21 speeches
tokenInfo <- summary(speechCorpus)
View(tokenInfo)
#Tokens = words
#Types = number of unique words
#Sentences = number of sentences

#Which candidate gave the longest speech? 
#Tokens are the number of words. This is asking which speech has the most words?
tokenInfo[which.max(tokenInfo$Tokens),] 

#Which candidate gave the shortest speech?
tokenInfo[which.min(tokenInfo$Tokens),] 

#Exploring a text
##Key Words in Context (KWIC)
options(width = 80)
kwic(speechCorpus, "terrorism")

#What are stopwords?
head(stopwords("english"), 20)

##Create a document-frame matrix
#the command is "dfm()"
#remove the stopworks
#Rows are the individual documents (speeches)
#Columns are the number of times a word was within a document
speechMatrix <- dfm(speechCorpus, ignoredFeatures = stopwords("english"))

# What are the 20 top words found within the corpus?
topfeatures(speechMatrix, 20)  

#Create a wordcloud 
plot(speechMatrix)

#Let's make a prettier one using colors
if (require(RColorBrewer)) # Loading required package: RColorBrewer
  plot(speechMatrix, max.words = 500, colors = brewer.pal(6, "Dark2"), scale = c(8, .5)) 


###############DOCUMENT READABILITY####################

#Create measures of the readability scores for each document:
docvars(speechCorpus, "readability")  <- readability(speechCorpus, "Flesch")
docvars(speechCorpus, "readabilityGrade")  <- readability(speechCorpus, "Flesch.Kincaid")
#The first variable is titled "readiability" 
#The second variable is titled "readabilityGrade" 
#The command itself is "readability()"

#Preview to ensure that the variables have been created
summary(speechCorpus, n=5) 

#Update dataset to include the new readability grade variables
tokenInfo <- summary(speechCorpus)

#Plot the results using ggplot2
ggplot(data=tokenInfo, aes(x=reorder(Candidate, -readabilityGrade), y=readabilityGrade)) + geom_bar(position="dodge", stat="identity", fill="navy", colour="black") + theme(axis.title.x = element_text(face="bold", size=12), axis.text.x  = element_text(angle=45, vjust=0.5, size=12, face="bold")) +  labs(x="",y="Flesch-Kincaid Score") + theme(axis.title.y = element_text(size=12), axis.text.y  = element_text(size=12)) 

       

###############DOCUMENT SIMILARITY####################

#Let's adjust the document labels 
docnames(speechCorpus) <- paste(docvars(speechCorpus, "Candidate"))

#Create a new document frame matrix
speechMatrix <- dfm(speechCorpus, ignoredFeatures = stopwords("english"), stem = TRUE)

#Measuring Document Similiarity
TrumpSimilarity <- similarity(speechMatrix, c("Trump"), n = NULL,  margin = "documents", method = "cosine", normalize = FALSE)

#Plot these results
dotchart(TrumpSimilarity$Trump, xlab = "Cosine similarity")


###############SENTIMENT ANALYSIS####################

#Create a "Sentiment Score" for each candidate's speech
docvars(speechCorpus, "Sentiment") <- get_sentiment(speechCorpus$documents$texts, method="afinn")
#The command is "get_sentiment()". It comes from the syuzhet package

#Preview to ensure that the variables have been created
summary(speechCorpus, n=5) 

#Update dataset to include the new readability grade variables
tokenInfo <- summary(speechCorpus)
#Plot the sentiment scores of the speeches
ggplot(data=tokenInfo, aes(x=reorder(Candidate, -Sentiment), y=Sentiment)) + geom_bar(position="dodge", stat="identity", fill="purple", colour="black") + theme(axis.title.x = element_text(face="bold", size=12), axis.text.x  = element_text(angle=45, vjust=0.5, size=12, face="bold")) +  labs(x="",y="Sentiment Score of Speech") + theme(axis.title.y = element_text(size=12), axis.text.y  = element_text(size=12)) 


###Generate emotional content of speeches

nrc_data <- get_nrc_sentiment(speechCorpus$documents$texts)
names(nrc_data)
View(nrc_data)

docvars(speechCorpus, "anger") <- nrc_data$anger
docvars(speechCorpus, "trust") <- nrc_data$trust
docvars(speechCorpus, "disgust") <- nrc_data$disgust
docvars(speechCorpus, "joy") <- nrc_data$joy
docvars(speechCorpus, "fear") <- nrc_data$fear
docvars(speechCorpus, "sadness") <- nrc_data$sadness
docvars(speechCorpus, "anticipation") <- nrc_data$anticipation


barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Candidates' Speeches", xlab="Percentage"
)

#Update dataset to include the new readability grade variables
tokenInfo <- summary(speechCorpus)

#Create ratios of content
tokenInfo$fear_ratio = tokenInfo$fear/tokenInfo$Type
tokenInfo$anger_ratio = tokenInfo$anger/tokenInfo$Type

#Plot the fear scores of the speeches
ggplot(data=tokenInfo, aes(x=reorder(Candidate, -fear), y=fear)) + geom_bar(position="dodge", stat="identity", fill="red", colour="black") + theme(axis.title.x = element_text(face="bold", size=12), axis.text.x  = element_text(angle=45, vjust=0.5, size=12, face="bold")) +  labs(x="",y="Sentiment Score of Fear") + theme(axis.title.y = element_text(size=12), axis.text.y  = element_text(size=12)) 


###############TOPIC MODELING####################

#Load csv file
CABills <- textfile("CABills.csv", textField = "billsdescription")
#Create a corpus of the legislation
billsCorpus <- corpus(CABills) 
#Summarize the corpus
summary(billsCorpus, n=5)

# make a dfm, removing stopwords and applying stemming

myStemMat <- dfm(billsCorpus, ignoredFeatures=c("bill", "California", stopwords("english")), stem = TRUE)

#Estimate a LDA Topic Model 
if (require(topicmodels)) {
  myLDAfit15 <- LDA(convert(myStemMat, to = "topicmodels"), k = 15)
  get_terms(myLDAfit15, 5)
  topics(myLDAfit15, 3)
}

terms(myLDAfit15, 12)









