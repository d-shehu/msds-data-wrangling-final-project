################################################################################
# Topic Modeling
#
# Use LDA to derive topics automatically and visualize the top terms for those
# topics.
#
################################################################################

library(tm)
library(topicmodels)

fnGetTidyTerms <- function(dfInMetaData,dfInTerms){
  print("Get Tidy Terms...")
  print(dfInMetaData)
  print(dfInTerms)
  
  dfRes <- merge(dfInMetaData, dfInTerms, by="id") %>%
    group_by(id) %>% 
    mutate(serial = cur_group_id()) %>% 
    group_by(serial,word) %>% 
    summarise(count=sum(n)) %>% 
    rename(document=serial,term=word)
  
  print("Output")
  print(dfRes)
  return (dfRes)
}

fnGetNewsTopics <- function(dfInTidyTerms, numTopics, inMethod){
  tdNews <- dfInTidyTerms %>% 
    cast_dtm(document, term, count)
  
  tidy(LDA(tdNews, k = numTopics,method=inMethod))
}

fnGetTopicTerms <- function(newsTopics, topicNum){
  newsTopics %>% filter(topic==topicNum) %>%
    mutate(beta_rank = min_rank(desc(beta))) %>% 
    filter(beta_rank <= 10) %>% 
    arrange(beta_rank)
}

fnPlotNewsTopic <- function(newsTopics, topicNum){
  newsTopics %>% filter(topic==topicNum) %>% 
    mutate(beta_rank = min_rank(desc(beta))) %>% 
    filter(beta_rank <= 10) %>% 
    arrange(beta_rank) %>%
    mutate(term = reorder(term, beta)) %>% 
    ggplot(aes(beta, term)) + 
    geom_col(show.legend = FALSE)
}