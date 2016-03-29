#Code to replicate figures in blog post on Supreme Court Oral Arguments
#be sure to intall the qdap package

library(qdap)
library(ggplot2)
library(gridExtra)
library(ggplot2) 
library(scales) 
library(RColorBrewer)
library(grid)
library(data.frame)
library(reshape2)

#Set docx files into working directory
#Note that transcripts must be in .docx for the qdap package
#Load in transcript for Fisher I 
Fisher1 <- read.transcript("Fisher1.docx", col.names=c("Justice","Statement"), bracket2quote=TRUE)
Fisher1$Statement <- qprep(Fisher1$Statement)  
Fisher1$Justice <- factor(Trim(Fisher1$Justice))

#Load in transcript for Fisher 2
Fisher2 <- read.transcript("Fisher2.docx", col.names=c("Justice","Statement"), bracket2quote=TRUE)
Fisher2$Statement <- qprep(Fisher2$Statement)  
Fisher2$Justice <- factor(Trim(Fisher2$Justice))

#View the data to ensure proper loading
View(Fisher1)
View(Fisher2)

#Get dimensions of Fisher1
dim(Fisher1)
Fisher1$counter <- seq(1:420)

#Create variable for different attorneys: Petitioner =1; Respondent=2; SG=3; Petitioner Rebuttal=4
attach(Fisher1)
Fisher1$stage1[counter > 382] <- 4
Fisher1$stage1[counter > 324 & counter <= 382] <- 3
Fisher1$stage1[counter > 152 & counter <= 324] <- 2
Fisher1$stage1[counter <= 152] <- 1
detach(Fisher1)
Fisher1$stage1 <- factor(Trim(Fisher1$stage1))

#Get dimensions of Fisher2
dim(Fisher2)
Fisher2$counter2 <- seq(1:423)

#Create variable for different attorneys: Petitioner =1; Respondent=2; SG=3; Petitioner Rebuttal=4
attach(Fisher2)
Fisher2$stage2[counter2 > 392] <- 4
Fisher2$stage2[counter2 > 317 & counter2 <= 392] <- 3
Fisher2$stage2[counter2 > 186 & counter2 <= 317] <- 2
Fisher2$stage2[counter2 < 187] <- 1
detach(Fisher2)
Fisher2$stage2 <- factor(Trim(Fisher2$stage2))


#####Generate Gantt Plots of Discourse

Fisher1Graph <- with(Fisher1, gantt_plot(Statement, Justice,  xlab = "Course of Argument", ylab="", x.tick=TRUE, minor.line.freq = NULL, major.line.freq = NULL, rm.horiz.lines = FALSE, size=6, title="Oral Argument from Fisher I", transform=TRUE, space="free")) 

Fisher2Graph <- with(Fisher2, gantt_plot(Statement, Justice,  xlab = "Course of Argument", ylab="", x.tick=TRUE, minor.line.freq = NULL, major.line.freq = NULL, rm.horiz.lines = FALSE, size=6, title="Oral Argument from Fisher II",  transform=TRUE, , space="free"))

grid.arrange(Fisher1Graph, Fisher2Graph)

####Generate the Lexical Dispersion Plots

Words1 <- with(Fisher1 , dispersion_plot(Statement, c("race", "racial", "black", "affrican~~american", "affirmative~~action", "quota", "minority", "diversity", "discrimination", "hispanic", "asian", "bakke", "grutter", "parents~~involved", "overrule", "strict~~scrutiny", "rational~~basis", "compelling~~interest" ,"rational~~basis", "twenty~~five", "race~~neutral", "sat", "ten~~percent", "rotc", "military", "university"), title = "Lexical Dispersion Plot for Fisher I", ylab="",  rm.vars = stage1, size=9))
Words1 <- Words1 + theme(axis.ticks = element_blank(), axis.text.x = element_blank())

Words2 <- with(Fisher2 , dispersion_plot(Statement, c("race", "racial", "black", "affrican~~american", "affirmative~~action", "quota", "minority", "diversity", "discrimination", "hispanic", "asian", "bakke", "grutter", "parents~~involved", "overrule", "strict~~scrutiny", "rational~~basis", "compelling~~interest" , "rational~~basis", "twenty~~five", "raceneutral", "sat", "ten~~percent", "rotc", "military", "university"), rm.vars = stage2, title = "Lexical Dispersion Plot for Fisher II",  size=9, ylab="" ))
Words2 <- Words2 + theme(axis.ticks = element_blank(), axis.text.y = element_blank(), axis.title = element_blank())

#Set plots next to each other
grid.arrange(Words1, Words2, ncol=2)

###Generate bar graphs comparing the number of words spoken

#Get number of questions of Petitioners asked by Justices in Fisher I and Fisher II

(Fisher1_pet <- Fisher1[Fisher1$stage1 == 1 | Fisher1$stage1 == 4, ])
(Fisher2_pet <- Fisher2[Fisher2$stage2 == 1 | Fisher2$stage2 == 4, ])
(pet_wrds1 <- with(Fisher1_pet, word_stats(Statement, Justice)))
(pet_wrds2 <- with(Fisher2_pet, word_stats(Statement, Justice)))

Fisher1Scores_pet<- scores(pet_wrds1)
Fisher2Scores_pet<- scores(pet_wrds2)

View(Fisher1Scores_pet)
View(Fisher2Scores_pet)

(Fisher1_resp <- Fisher1[Fisher1$stage1 == 2, ])
(Fisher2_resp <- Fisher2[Fisher2$stage2 == 2, ])
(resp_wrds1 <- with(Fisher1_resp, word_stats(Statement, Justice)))
(resp_wrds2 <- with(Fisher2_resp, word_stats(Statement, Justice)))

Fisher1Scores_resp<- scores(resp_wrds1)
Fisher2Scores_resp<- scores(resp_wrds2)

View(Fisher1Scores_resp)
View(Fisher2Scores_resp)

#Plot for Respondent Questions

words <- data.frame(
  case = factor(c("Fisher I", "Fisher II", "Fisher I", "Fisher II", "Fisher I","Fisher II", "Fisher I","Fisher II","Fisher I","Fisher II", "Fisher I","Fisher II", "Fisher I","Fisher II")),
  justice = factor(c("Kennedy","Kennedy","Roberts","Roberts", "Ginsburg", "Ginsburg", "Alito", "Alito", "Breyer", "Breyer", "Sotomayor", "Sotomayor", "Scalia", "Scalia"), levels=c("Kennedy","Roberts", "Ginsburg", "Alito", "Breyer", "Sotomayor", "Scalia")),
  resp_words = c(154, 187, 558, 304, 154, 284, 494, 820, 123, 66, 131, 187, 440, 279 ),
  pet_words = c(202, 316, 76, 71, 370, 428, 21, 265, 681, 612, 1130, 907, 329, 330),
  attorney = factor(c("Petitioner", "Respondent", "Petitioner", "Respondent", "Petitioner", "Respondent", "Petitioner", "Respondent", "Petitioner", "Respondent", "Petitioner", "Respondent", "Petitioner", "Respondent")))

words.m <- melt(words)
head(words.m)
table(words.m$variable)

levels(factor(words.m$variable)) 
words.m$variable.new <- relevel(factor(words$variable), ref="resp_words")
levels(words.m$variable.new) 

total_words = rev(variable.new), width=.5)) +
  geom_bar(aes(y=value), stat="identity", position= "stack")+
  facet_wrap(~justice, scales = 'free_x') +
  scale_fill_discrete("Attorney", labels=c("Respondent", "Petitioner")) +
  labs(y = "Total Words Spoken", x ="")

ggsave("total_words.pdf", total_words)



