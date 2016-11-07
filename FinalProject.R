library('readr')
readcsv<- read_csv('Sentiment_GOP.csv')
readcsv_normalized<- readcsv[!(is.na(readcsv$tweet_location) | readcsv$tweet_location=="") , ]

candidate1<- readcsv_normalized[readcsv_normalized$candidate=="No candidate mentioned" ,]
candidate2<- readcsv_normalized[readcsv_normalized$candidate=="Donald Trump" ,]
candidate3 <-readcsv_normalized[readcsv_normalized$candidate=="Ted Cruz" ,] 
candidate4 <-readcsv_normalized[readcsv_normalized$candidate=="Ben Carson" ,] 
candidate5 <-readcsv_normalized[readcsv_normalized$candidate=="Chris Christie" ,] 
candidate6 <-readcsv_normalized[readcsv_normalized$candidate=="John Kasich" ,] 
candidate7 <-readcsv_normalized[readcsv_normalized$candidate=="Marco Rubio" ,] 
candidate8 <-readcsv_normalized[readcsv_normalized$candidate=="Mike Huckabee" ,] 
candidate9 <-readcsv_normalized[readcsv_normalized$candidate=="Rand Paul" ,] 
candidate10 <-readcsv_normalized[readcsv_normalized$candidate=="Scott Walker" ,] 







