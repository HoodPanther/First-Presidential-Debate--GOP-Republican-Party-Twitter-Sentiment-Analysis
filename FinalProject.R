install.packages('RColorBrewer')
library('readr')
library('dplyr')
library('igraph')
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer") 
readcsv<- read_csv('Sentiment_GOP.csv')
#Normalizing data by removing NA values
readcsv_normalized<- readcsv[!(is.na(readcsv$tweet_location) | readcsv$tweet_location=="" |  readcsv$subject_matter=="None of the above" | is.na(readcsv$candidate)) , ]
readcsv_normalized <- readcsv_normalized[!(is.na(readcsv_normalized$candidate)),]

#Filtering data based on subject matter such as Abortion, Racial Issues
readcsv_normalized_abortion_sm<-readcsv_normalized[readcsv_normalized$subject_matter=="Abortion" & !readcsv_normalized$candidate=="No candidate mentioned",]
readcsv_normalized_Racialissues_sm<- readcsv_normalized[readcsv_normalized$subject_matter=="Racial issues" & !readcsv_normalized$candidate=="No candidate mentioned",]
readcsv_normalized_GunControl_sm<- readcsv_normalized[readcsv_normalized$subject_matter=="Gun Control" & !readcsv_normalized$candidate=="No candidate mentioned",]

# Function for finding the adjacency list between candidates for different subject matter 
create_adj_list = function(df){
  # Input: a dataframe with a column "tokens"
  # Output: all possible 2-combinations (sorted) of the unique tokens
  unique_tokens = unique(df$candidate)
  adj_list = data.frame()
  if(length(unique_tokens) >= 2){
    all_combins = t(combn(unique_tokens, 2))
    all_combins = t(apply(all_combins, 1, sort))
    adj_list = data.frame(all_combins, stringsAsFactors = TRUE)
  }
  return(adj_list)
}

#Creating adjacency List for Adoption subject Matter
adj_list_abortion_sm = readcsv_normalized_abortion_sm %>% group_by(subject_matter) %>% do(create_adj_list(.))
state_graph_abortion = graph.data.frame(adj_list_abortion_sm[, c("X1", "X2")], directed = FALSE)


plot(state_graph_abortion, layout=layout.fruchterman.reingold)
plot(simplify(state_graph_abortion))

#Creating adjacency List for Racial Issues subject 
adj_list_RacialIssues_sm = readcsv_normalized_Racialissues_sm %>% group_by(subject_matter) %>% do(create_adj_list(.))
state_graph_racialissues = graph.data.frame(adj_list_RacialIssues_sm[, c("X1", "X2")], directed = FALSE)
plot(state_graph_racialissues, layout=layout.fruchterman.reingold)
plot(simplify(state_graph_racialissues))


#Creating adjacency List for Gun Control subject Matter
adj_list_GunControl_sm = readcsv_normalized_GunControl_sm %>% group_by(subject_matter) %>% do(create_adj_list(.))
state_graph_GunControl = graph.data.frame(adj_list_GunControl_sm[, c("X1", "X2")], directed = FALSE)
degree(state_graph_GunControl)
plot(state_graph_GunControl, layout=layout.fruchterman.reingold)
plot(simplify(state_graph_GunControl))
# From the above finding we can see that not all the candidates spoke about the Gun Control issues


# Group candidates based on Sentiments( Positive , Negative or Neautral) from general public tweets.
#Creating adjacency List for Adoption subject Matter
adj_list_abortion_sentiment_sm = readcsv_normalized_abortion_sm %>% group_by(sentiment) %>% do(create_adj_list(.))
state_graph_abortion_sentiments = graph.data.frame(adj_list_abortion_sentiment_sm[, c("X1", "X2")], directed = FALSE)

# Assigning edge colors based on sentiments ( Green for Positive, Red for Negative and Yellow for Neutral)

E(state_graph_abortion_sentiments)$color=
ifelse(adj_list_abortion_sentiment_sm$sentiment=="Positive","Green",
                                                 ifelse(adj_list_abortion_sentiment_sm$sentiment=="Negative","Red",
                                                       "Yellow"))
#Finding Degree of graph and depending upon degree we can change vertex size
degree(state_graph_abortion_sentiments)
V(state_graph_abortion_sentiments)$vertex_degree <-  degree(state_graph_abortion_sentiments)
plot(state_graph_abortion_sentiments, layout=layout.fruchterman.reingold,vertex.size=V(state_graph_abortion_sentiments)$vertex_degree)
plot(simplify(state_graph_abortion_sentiments))


#Creating adjacency List for Racial Issues subject Matter
adj_list_racialissues_sentiment_sm = readcsv_normalized_Racialissues_sm %>% group_by(sentiment) %>% do(create_adj_list(.))
state_graph_racialissues_sentiments = graph.data.frame(adj_list_racialissues_sentiment_sm[, c("X1", "X2")], directed = FALSE)

# Assigning edge colors based on sentiments ( Green for Positive, Red for Negative and Yellow for Neutral)

E(state_graph_racialissues_sentiments)$color=
  ifelse(adj_list_racialissues_sentiment_sm$sentiment=="Positive","Green",
         ifelse(adj_list_racialissues_sentiment_sm$sentiment=="Negative","Red",
                "Yellow"))
#Finding Degree of graph and depending upon degree we can change vertex size
degree(state_graph_racialissues_sentiments)
V(state_graph_racialissues_sentiments)$vertex_degree <-  degree(state_graph_racialissues_sentiments)
degree(state_graph_racialissues_sentiments)
plot(state_graph_racialissues_sentiments, layout=layout.fruchterman.reingold,vertex.size=V(state_graph_racialissues_sentiments)$vertex_degree)


degree(state_graph_racialissues_sentiments)
degree.distribution(state_graph_racialissues_sentiments)
#Histogram of candidates for a specific issue having positive tweets 



#load the twitter data
#format the tweets 
readcsv$text <- gsub("#GOPDebate", "", readcsv$text) 
readcsv$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", readcsv$text) 
readcsv$text <- gsub("#GOPdebate", "", readcsv$text)  
readcsv$text = gsub("[[:punct:]]", "", readcsv$text)
readcsv$text = gsub("[[:digit:]]", "",readcsv$text)
readcsv$text = gsub("http\\w+", "", readcsv$text)
readcsv$text = gsub("[ \t]{2,}", "", readcsv$text)
readcsv$text = gsub("^\\s+|\\s+$", "", readcsv$text)
readcsv$text = gsub("amp", "", readcsv$text)
readcsv$text = gsub("[^\x20-\x7E]", "", readcsv$text)



#create data frame containing text, sentiment and retweet count
tweet_data <- subset(readcsv,selec=c("text","sentiment","retweet_count")) 

sents = levels(factor(tweet_data$sentiment))  
cand=levels(factor(tweet_data$candidate))


labels <- lapply(sents, 
                 function(x,y) paste(x,format(
                round((length((tweet_data[tweet_data$sentiment ==x ,])$text)/
                         length(tweet_data$sentiment)*100),2),nsmall=2),"%"))

nemo = length(sents)
emo.docs = rep("", nemo) 

for (i in 1:nemo)
{
  tmp = tweet_data[tweet_data$sentiment == sents[i],]$text
  
  emo.docs[i] = paste(tmp,collapse=" ")
}  

emo.docs = removeWords(emo.docs, stopwords("german"))

emo.docs = removeWords(emo.docs, stopwords("english"))

corpus = Corpus(VectorSource(emo.docs)) 
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = labels

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)


# world Cloud Sentiments of people on Donald Trump tweets.

readcsv_normalized_DT<-readcsv_normalized[readcsv_normalized$candidate=="Donald Trump",]

readcsv_normalized_DT$text <- gsub("#GOPDebate", "", readcsv_normalized_DT$text) 
readcsv_normalized_DT$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", readcsv_normalized_DT$text) 
readcsv_normalized_DT$text <- gsub("#GOPdebate", "", readcsv_normalized_DT$text)  
readcsv_normalized_DT$text = gsub("[[:punct:]]", "", readcsv_normalized_DT$text)
readcsv_normalized_DT$text = gsub("[[:digit:]]", "",readcsv_normalized_DT$text)
readcsv_normalized_DT$text = gsub("http\\w+", "", readcsv_normalized_DT$text)
readcsv_normalized_DT$text = gsub("[ \t]{2,}", "", readcsv_normalized_DT$text)
readcsv_normalized_DT$text = gsub("^\\s+|\\s+$", "", readcsv_normalized_DT$text)
readcsv_normalized_DT$text = gsub("amp", "", readcsv_normalized_DT$text)
readcsv_normalized_DT$text = gsub("[^\x20-\x7E]", "", readcsv_normalized_DT$text)



#create data frame containing text, sentiment and retweet count
tweet_data <- subset(readcsv_normalized_DT,selec=c("text","sentiment")) 

sents = levels(factor(tweet_data$sentiment))  
cand=levels(factor(tweet_data$candidate))


labels <- lapply(sents, 
                 function(x) paste(x,format(
                   round((length((tweet_data[tweet_data$sentiment ==x ,])$text)/
                            length(tweet_data$sentiment)*100),2),nsmall=2),"%"))

nemo = length(sents)
emo.docs = rep("", nemo) 

for (i in 1:nemo)
{
  tmp = tweet_data[tweet_data$sentiment == sents[i],]$text
  
  emo.docs[i] = paste(tmp,collapse=" ")
}  

emo.docs = removeWords(emo.docs, stopwords("german"))

emo.docs = removeWords(emo.docs, stopwords("english"))

corpus = Corpus(VectorSource(emo.docs)) 
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = labels

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Accent"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)

