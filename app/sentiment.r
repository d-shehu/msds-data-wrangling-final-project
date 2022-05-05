library(dplyr)
library(ggplot2)

######################
# Sentiment Analysis
######################

fnGetSentimentsByDay <- function(dfInTerms, sentimentType){
  inner_join(dfInTerms, get_sentiments(sentimentType)) %>% 
    group_by(published, sentiment) %>% 
    summarise(n=n())  %>% 
    group_by(published) %>% 
    mutate(prop=n/sum(n))
}

fnGetNetSentimentsByDay <- function(dfInTerms, sentimentType){
  if(sentimentType == "afinn"){
    inner_join(dfInTerms, get_sentiments(sentimentType)) %>% 
      group_by(published) %>% 
      summarise(net=sum(value)) %>%
      select(published, net)
  }
  else if(sentimentType == "loughran" | sentimentType == "bing"){
    fnGetSentimentsByDay(dfInTerms, sentimentType) %>%
      select(-n) %>% 
      pivot_wider(names_from = sentiment, values_from = prop) %>% 
      mutate(net=positive-negative) %>%
      select(published, net)
  }
  else{
    stop("Unknown sentiment type")
  }
}

fnPlotNetSentimentsByDay <- function(dfSentimentsByDay){
  dfSentimentsByDay %>%
    ggplot(aes(x=published, y=net)) + 
    geom_bar(stat="identity") + 
    geom_smooth() +
    xlab("Date Published") +
    ylab("Net Sentiment")
}

fnPlotSentimentsByDay <- function(dfSentimentsByDay){
  dfSentimentsByDay %>% 
    group_by(published) %>% 
    mutate(prop=n/sum(n)) %>% 
    ggplot(aes(x=published,y=prop,fill=sentiment)) + 
    geom_bar(stat="identity")  +
    xlab("Date Published") +
    ylab("Proportion of Words")
}


fnPlotWordsBySentiment <- function(dfInTerms){
  inner_join(dfInTerms, get_sentiments("nrc")) %>% 
    group_by(sentiment, word) %>% 
    summarise(n=n()) %>% 
    group_by(sentiment) %>%
    slice_max(order_by=n, n=10) %>%
    ggplot(aes(x=word, y=n)) +
    geom_bar(stat="identity") +
    coord_flip() +
    facet_wrap(~sentiment, scales="free")
}