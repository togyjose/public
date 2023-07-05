# Sequence of Processing

# 1) Convert Voyager data into an R dataframe
# 2) Extract word-level data from this data frame
# 3) Remove stock stop-words from the data frame
# 4) Use tf-idf analysis to identify contextual stop-words. Remove 
# these contextual stop-words as well.
# 5) Generate wordcloud to manually identify words that don't add value.
# 6) Generate sentiment (i.e. Polarity) score for each word and 
# aggregate back into message level data.
# 7) Generate subjectivity score at a message level.
# 8) Normalize the Polarity Score.
# 9) Generate an xls file with the output.

# Search Key Words
# mulan disneysmulan mulandisney mulan2020 liuyifei mulanliuyifei boycottmulan

#install.packages("stringr")
library(stringr)
#install.packages("dplyr")
#install.packages("tidytext")
#install.packages("readxl")
#install.packages("lda")
#install.packages("igraph")
library(lda)
library(plyr)
library(dplyr)
library(tidytext)
library(readxl)
library(tidytext)
library(tidyr)
library(reshape2)
library(writexl)
library(tidyverse)
library(igraph)
library(wordcloud)
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")
#install.packages("plyr")
#install.packages("edgar")
library(edgar)
#install.packages("clusterSim")
#install.packages("caret")
library(caret)
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
#install.packages("pKSEA")
library(pKSEA)
#install.packages("data.table")
library(data.table)
#install.packages("textdata")
library(textdata)
#install.packages("MASS")
library(MASS)
#install.packages("cld2")
library(cld2)
#install.packages("cld3")
library(cld3)
#install.packages("googleLanguageR")
library(googleLanguageR)
#install.packages("qdapTools")
library(qdapTools)
#install.packages("rlist")
library(rlist)
#install.packages("rjson")
library(rjson)
#install.packages("purrr")
library(purrr)

library(rtweet)
library(rtweetXtras)
library(streamr)
library(tweetrmd)
gl_auth("C:/Program Files/RStudio/<deleted JSON key>") # Authentical JSON file to access Google NLP API


# Required parameters - Post and Post_ID

sentmt <- get_sentiments("afinn")
str(sentmt)
head (sentmt)

sentmt_labels <- get_sentiments("bing")
str(sentmt_labels)
head(sentmt_labels)

# ############################
# #Translate Key Words and output file - Start
# ############################
# 
# # IS0 639 codes - https://cloud.google.com/translate/docs/languages
# # Get the text
setwd("G:/PEX_and_Data_Science_02Aug2020/R_Working_Dir")
getwd()
# 
# 

# #WeekWise nlp generation
# nlp_F22sepIN_u_wk1 <- gl_nlp(F22sepIN_u_wk1$'Post content') # records

#### Transition - start #####

F22sepIN15df <- F22sepIN[F22sepIN$Year=="2015",]
F22sepIN16df <- F22sepIN[F22sepIN$Year=="2016",]
F22sepIN17df <- F22sepIN[F22sepIN$Year=="2017",]


nlp_F22sepIN_u_wk1 <- F22sepIN15
# nlp_F22sepIN_u_wk1 <- F22sepIN16
# nlp_F22sepIN_u_wk1 <- F22sepIN17

F22sepIN_u_wk1 <- F22sepIN15df
# F22sepIN_u_wk1 <- F22sepIN16df
# F22sepIN_u_wk1 <- F22sepIN17df


#### Transition - end #####


#Create weekly copies to avoid regenrating the original nlp file
nlp_F22sepIN_u_wk1_wip <- nlp_F22sepIN_u_wk1

# Insert Post_ID into nlp File

i=1 # Reset i
repeat {
  nlp_F22sepIN_u_wk1_wip$sentences[[i]]$Post_ID <- F22sepIN_u_wk1$Post_ID[[i]]
  nlp_F22sepIN_u_wk1_wip$tokens[[i]]$Post_ID <- F22sepIN_u_wk1$Post_ID[[i]]
  nlp_F22sepIN_u_wk1_wip$classifyText[[i]]$Post_ID <- F22sepIN_u_wk1$Post_ID[[i]]
  nlp_F22sepIN_u_wk1_wip$entities[[i]]$Post_ID <- F22sepIN_u_wk1$Post_ID[[i]]
  i=i+1
}

#Creating a copy of weekly files for cleanup
nlp_F22sepIN_u_wk1_wip_clean <- nlp_F22sepIN_u_wk1_wip

##############################################################################################
###### Remove Weekly error entries and insert the voyager message# into this list to enable merges later
##############################################################################################

# Week 1
#####
i=1 # Reset i
repeat{
  if(nlp_F22sepIN_u_wk1_wip_clean$sentences[[i]][[1]]=="#error -  API returned: Invalid text content: too few tokens (words) to process." ||
     nlp_F22sepIN_u_wk1_wip_clean$sentences[[i]][[1]]=="#error - zero length string" ||
     length(nlp_F22sepIN_u_wk1_wip_clean$sentences[[i]])==1 ||
     nlp_F22sepIN_u_wk1_wip_clean$sentences[[i]]=="#error -  Request failed before finding status code: Could not resolve host: language.googleapis.com"){
    nlp_F22sepIN_u_wk1_wip_clean$sentences[[i]] <- NULL
}
i = i+1
}## Repeat till number stabilizes
i=1 # Reset i
repeat{
  if(nlp_F22sepIN_u_wk1_wip_clean$tokens[[i]][[1]]=="#error -  API returned: Invalid text content: too few tokens (words) to process." ||
     nlp_F22sepIN_u_wk1_wip_clean$tokens[[i]][[1]]=="#error - zero length string" ||
     length(nlp_F22sepIN_u_wk1_wip_clean$tokens[[i]])==1 ||
     nlp_F22sepIN_u_wk1_wip_clean$tokens[[i]]=="#error -  Request failed before finding status code: Could not resolve host: language.googleapis.com"){
    nlp_F22sepIN_u_wk1_wip_clean$tokens[[i]] <- NULL
}
i = i+1
}## Repeat till number stabilizes
i=1 # Reset i
repeat{
  if(is.na(nlp_F22sepIN_u_wk1_wip_clean$classifyText[[i]][[1]]) ||
     nlp_F22sepIN_u_wk1_wip_clean$classifyText[[i]][[1]]=="#error -  API returned: Invalid text content: too few tokens (words) to process." ||
     nlp_F22sepIN_u_wk1_wip_clean$classifyText[[i]][[1]]=="#error - zero length string" ||
     length(nlp_F22sepIN_u_wk1_wip_clean$classifyText[[i]])==1 ||
     nlp_F22sepIN_u_wk1_wip_clean$classifyText[[i]]=="#error -  Request failed before finding status code: Could not resolve host: language.googleapis.com"){
     nlp_F22sepIN_u_wk1_wip_clean$classifyText[[i]] <- NULL
}
i = i+1
}## Repeat till number stabilizes
i=1 # Reset i
repeat{
  if(is.null(nlp_F22sepIN_u_wk1_wip_clean$entities[[i]][[1]]) ||
     is.na(nlp_F22sepIN_u_wk1_wip_clean$entities[[i]][[1]]) ||
     nlp_F22sepIN_u_wk1_wip_clean$entities[[i]][[1]]=="#error -  API returned: Invalid text content: too few tokens (words) to process." ||
     nlp_F22sepIN_u_wk1_wip_clean$entities[[i]][[1]]=="#error - zero length string" ||
     length(nlp_F22sepIN_u_wk1_wip_clean$entities[[i]])==1 ||
     nlp_F22sepIN_u_wk1_wip_clean$entities[[i]]=="#error -  Request failed before finding status code: Could not resolve host: language.googleapis.com"){
    nlp_F22sepIN_u_wk1_wip_clean$entities[[i]] <- NULL
  }
  i = i+1
}## Repeat till number stabilizes




###############################
# Sentence Processing Weekly Files
###############################

# Sentence Processing - Wk33 - Convert formats one pair at a time and rbind i.e. converst list to a dataframe.
# Cleanup outstanding error cases before doing sentence overall aggregation

#Wk1
# Processing sentences content
i=1 # Reset i
sentence_overall_wk1 <- nlp_F22sepIN_u_wk1_wip_clean$sentences[[1]]
sentence_overall_wk1$Post_ID_Temp <- 1
repeat {# Check While loop effectivness
  sentence_i <- nlp_F22sepIN_u_wk1_wip_clean$sentences[[i]]
  sentence_i$Post_ID_Temp <- i
  j=i+1
  sentence_j <- nlp_F22sepIN_u_wk1_wip_clean$sentences[[j]]
  sentence_j$Post_ID_Temp <- j
  sentence_ij <- rbind(sentence_i, sentence_j)
  sentence_overall_wk1 <- rbind(sentence_ij, sentence_overall_wk1)
  i=j+1
}
# Processign the classifyText content
i=1 # Reset i
classifyText_overall_wk1 <- nlp_F22sepIN_u_wk1_wip_clean$classifyText[[1]]
classifyText_overall_wk1$Post_ID_Temp <- 1
repeat {# Check While loop effectivness
  classifyText_i <- nlp_F22sepIN_u_wk1_wip_clean$classifyText[[i]]
  classifyText_i$Post_ID_Temp <- i
  j=i+1
  classifyText_j <- nlp_F22sepIN_u_wk1_wip_clean$classifyText[[j]]
  classifyText_j$Post_ID_Temp <- j
  classifyText_ij <- rbind(classifyText_i, classifyText_j)
  classifyText_overall_wk1 <- rbind(classifyText_ij, classifyText_overall_wk1)
  i=j+1
}
# Processign the tokens content
i=1
tokens_overall_wk1 <- nlp_F22sepIN_u_wk1_wip_clean$tokens[[1]]
tokens_overall_wk1$Post_ID_Temp <- 1
repeat {# Check While loop effectivness
  tokens_i <- nlp_F22sepIN_u_wk1_wip_clean$tokens[[i]]
  tokens_i$Post_ID_Temp <- i
  j=i+1
  tokens_j <- nlp_F22sepIN_u_wk1_wip_clean$tokens[[j]]
  tokens_j$Post_ID_Temp <- j
  tokens_ij <- rbind(tokens_i, tokens_j)
  tokens_overall_wk1 <- rbind(tokens_ij, tokens_overall_wk1)
  i=j+1
}
# Processign the entities content
i=1
name <- nlp_F22sepIN_u_wk1_wip_clean$entities[[i]]$name
type <- nlp_F22sepIN_u_wk1_wip_clean$entities[[i]]$type
salience <- nlp_F22sepIN_u_wk1_wip_clean$entities[[i]]$salience
score <- nlp_F22sepIN_u_wk1_wip_clean$entities[[i]]$score
Post_ID <- nlp_F22sepIN_u_wk1_wip_clean$entities[[i]]$Post_ID
entities_overall_wk1 <- cbind(name, type, salience, score, Post_ID)
i=1
repeat{
  name <- nlp_F22sepIN_u_wk1_wip_clean$entities[[i]]$name
  type <- nlp_F22sepIN_u_wk1_wip_clean$entities[[i]]$type
  salience <- nlp_F22sepIN_u_wk1_wip_clean$entities[[i]]$salience
  score <- nlp_F22sepIN_u_wk1_wip_clean$entities[[i]]$score
  Post_ID <- nlp_F22sepIN_u_wk1_wip_clean$entities[[i]]$Post_ID
  entities_i <- cbind(name, type, salience, score, Post_ID)
  j=i+1
  name <- nlp_F22sepIN_u_wk1_wip_clean$entities[[j]]$name
  type <- nlp_F22sepIN_u_wk1_wip_clean$entities[[j]]$type
  salience <- nlp_F22sepIN_u_wk1_wip_clean$entities[[j]]$salience
  score <- nlp_F22sepIN_u_wk1_wip_clean$entities[[j]]$score
  Post_ID <- nlp_F22sepIN_u_wk1_wip_clean$entities[[j]]$Post_ID
  entities_j <- cbind(name, type, salience, score, Post_ID)
  entities_ij <- rbind(entities_i,entities_j)
  entities_overall_wk1 <- rbind(entities_ij, entities_overall_wk1)
  i=j+1
}
entities_overall_wk1 <- as.data.frame(entities_overall_wk1)
###################################################################################

#Insert Week number in relevant files
sentence_overall_wk1$WeekInt<-1

#Insert Week number in Sentencse files
tokens_overall_wk1$WeekInt<-1

#Insert Week number in Sentencse files
classifyText_overall_wk1$WeekInt <- 1

#Insert Week number in Sentencse files
entities_overall_wk1$WeekInt <- 1

###########################################################################
### Generate message level scores 
###########################################################################


# Weekly Files - Replace score with value
sentence_overall_wk1$value <- sentence_overall_wk1$score #rename 'score' to 'value'
sentence_overall_wk1 = subset(sentence_overall_wk1, select = -c(4))

# Generate sentence message level scores for each week
#Wk1
message_overall_wk1 <- sentence_overall_wk1 %>% dplyr::group_by(Post_ID) %>% dplyr::summarise(
  Post_ID_Temp = mean(Post_ID_Temp),
  Polarity = sum(value),
  Count_Overall = length(value),
  Count_Sentmt = length(which(value!=0)),
  Positive_Polarity = sum(value[value>0]),
  Sentmt_Pos = length(which(value>0)),
  Negative_Polarity = sum(value[value<0]),
  Sentmt_Neg = length(which(value<0)),
  Subjectivity = round(Count_Sentmt/Count_Overall,4),
  Message_Type = ifelse(sum(value)!=0,"Polarity Present",
                        ifelse(sum(value)==0 & length(which(value!=0))!=0,"Polarity Zero but Present",
                               "Polarity Absent"))
)

# Generate sentences message level scores for each week
message_final_Wk1 <- merge(x=message_overall_wk1, y=F22sepIN_u_wk1, by="Post_ID", all.x = TRUE)

# Generate classifyText message level scores for each week
message_overall_wk1_sent <- classifyText_overall_wk1 %>% dplyr::group_by(Post_ID) %>% top_n(1,confidence)
message_overall_wk1_sent_u <- message_overall_wk1_sent[!duplicated(message_overall_wk1_sent$Post_ID),]
# Merge with larger weekly file
message_final_Wk1 <- merge(x=message_final_Wk1, y= message_overall_wk1_sent_u, by="Post_ID", all.x = TRUE)

# Generate tokens message level scores for each week
## No aggregation required - as of now

# Generate entities message level scores for each week
## No aggregation required - as of now


week_overall_wk1 <- message_final_Wk1 %>% dplyr::group_by(WeekInt) %>% dplyr::summarise(
  #Post_ID_Temp = mean(Post_ID_Temp),
  Count_Wk = length(Count_Overall),
  Positive_Messages = length(which(Polarity>0)),
  Percentage_Positive_Messages = length(which(Polarity>0)) / length(Count_Overall),
  Percentage_Positive_Messages = round(Percentage_Positive_Messages,2),
  Negative_Messages = length(which(Polarity<0)),
  Percentage_Negative_Messages = length(which(Polarity<0)) / length(Count_Overall),
  Percentage_Negative_Messages = round(Percentage_Negative_Messages,2),
  Neutral_Messages = length(which(Polarity==0)),
  Percentage_Neutral_Messages = length(which(Polarity==0)) / length(Count_Overall),
  Percentage_Neutral_Messages = round(Percentage_Neutral_Messages,2),
  Polarity_Present = length(which(Message_Type == "Polarity Present")),
  Polarity_Zero_But_Present = length(which(Message_Type == "Polarity Zero but Present")),
  Polarity_Absent = length(which(Message_Type == "Polarity Absent")),
  Polarity = sum(Polarity),
  Polarity = round(Polarity,2),
  Positive_Polarity = sum(Positive_Polarity),
  Negative_Polarity = sum(Negative_Polarity),
  Polarity_Wk_Adj= sum(Polarity) /
    sum(Message_Type == "Polarity Present" |
          Message_Type == "Polarity Zero but Present"),
  Polarity_Wk_Adj = round(Polarity_Wk_Adj,2)
)
write_xlsx(week_overall_wk1,"G:/PEX_and_Data_Science_02Aug2020/R_Working_Dir/week_overall_wk1.xlsx")




































