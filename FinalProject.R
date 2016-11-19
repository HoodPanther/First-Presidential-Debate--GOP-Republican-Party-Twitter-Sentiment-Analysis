library('readr')
library('dplyr')
library('igraph')
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

plot(state_graph_abortion_sentiments, layout=layout.fruchterman.reingold)
plot(simplify(state_graph_abortion_sentiments))


#Creating adjacency List for Racial Issues subject Matter
adj_list_racialissues_sentiment_sm = readcsv_normalized_Racialissues_sm %>% group_by(sentiment) %>% do(create_adj_list(.))
state_graph_racialissues_sentiments = graph.data.frame(adj_list_racialissues_sentiment_sm[, c("X1", "X2")], directed = FALSE)

# Assigning edge colors based on sentiments ( Green for Positive, Red for Negative and Yellow for Neutral)

E(state_graph_racialissues_sentiments)$color=
  ifelse(adj_list_racialissues_sentiment_sm$sentiment=="Positive","Green",
         ifelse(adj_list_racialissues_sentiment_sm$sentiment=="Negative","Red",
                "Yellow"))

plot(state_graph_racialissues_sentiments, layout=layout.fruchterman.reingold)

