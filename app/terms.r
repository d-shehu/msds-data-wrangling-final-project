##############################################################################
# Terms & Trigrams
#
# Functionality to tokenize tidy text into words and trigrams to identify
# the most relavenet terms by frequency.
##############################################################################

library(dplyr)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(wordcloud)
library(tidyr)

fnGetTopWords <- function(dfInMetaData, dfInTerms, topN){
  merge(dfInTerms, dfInMetaData, by="id") %>%
    group_by(word,source) %>% summarise(n=n()) %>%
    group_by(word) %>% summarize(total=sum(n),sources=n()) %>%
    arrange(desc(total), desc(sources)) %>%
    head(topN)
}

fnGetTopTrigrams <- function(dfInMetaData, dfInTrigrams, topN){
  merge(dfInTrigrams, dfInMetaData, by="id") %>%
    group_by(trigram,source) %>% summarise(n=n()) %>%
    group_by(trigram) %>% summarize(total=sum(n),sources=n()) %>%
    arrange(desc(total), desc(sources)) %>%
    head(topN)
}

fnGetTopTrigramsByDay <- function(dfInTrigrams, topN){
  dfInTrigrams %>% 
    mutate(published=as.character(published)) %>%
    group_by(published, trigram) %>%
    summarize(n=n()) %>% 
    arrange(desc(published), desc(n), trigram) %>% 
    mutate(rank=row_number()) %>% 
    select(-n) %>% 
    pivot_wider(names_from=rank, values_from=trigram) %>% 
    select(published, c(1:topN+1))
}

fnPlotTopWords <- function(dfInMetaData, dfInTerms, topN){
  fnGetTopWords(dfInMetaData, dfInTerms, topN) %>%
    mutate(word = reorder(word, total)) %>%
    ggplot(aes(word, total)) +
    geom_bar(stat="identity") +
    xlab(NULL) + coord_flip()
}

fnPlotTopTrigrams <- function(dfInMetaData, dfInTrigrams, topN){
  fnGetTopTrigrams(dfInMetaData, dfInTrigrams, topN) %>%
    mutate(trigram = reorder(trigram, total)) %>%
    ggplot(aes(trigram, total)) +
    geom_bar(stat="identity") +
    xlab(NULL) + coord_flip()
}

fnCreateWordCloud <- function(dfInTerms, minFreq, maxNumWords){
  
  # Get consistent arrangement
  set.seed(4321)
  
  dfWordCloud <- dfInTerms %>% group_by(word) %>% summarise(n=n())
  
  wordcloud(words = dfWordCloud$word, freq=dfWordCloud$n, min.freq = minFreq, 
            max.words=maxNumWords, random.order = FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}

fnFilterTrigrams <- function(dfInTrigrams){
  dfInTrigrams %>% filter(!grepl("NA NA NA", trigram))
}