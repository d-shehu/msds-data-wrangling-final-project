##############################################################################
# Summary of Sources
#
# Functions needed to generate summary and plots of source statistics including
# number of articles, words, etc.
##############################################################################

library(dplyr)
library(ggplot2)

fnGetTopSources <- function(dfInMetaData, topN){
  dfInMetaData %>% 
    group_by(source) %>% 
    summarize(n=n()) %>% 
    mutate(prop = 100.0 * (n / sum(n))) %>% 
    arrange(desc(n)) %>% 
    head(topN)
}

fnPlotSources <- function(dfInMetaData){
  fnGetTopSources(dfInMetaData, 20) %>% 
    ggplot(aes(x="", y=prop, fill=source)) +
    geom_bar(width = 1, stat = "identity") + 
    xlab("") +
    ylab("% of Articles") +
    ggtitle("Article Source") +
    guides(fill=guide_legend(ncol=1)) +
    theme(legend.title=element_blank())
}

fnPlotSourceContribution <- function(dfInMetaData, dfInStats){
  dfUnique <- merge(dfInStats, dfInMetaData, by="id") %>% 
    select(source, num_unique_words) %>% 
    group_by(source) %>% 
    summarize(total_unique=sum(num_unique_words)) %>% 
    arrange(desc(total_unique)) %>%
    head(20)
  
  ggplot(data=dfUnique, aes(x=source, y=total_unique, fill=source)) +
    geom_bar(width = 1, stat = "identity") + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    ylab("# Unique Words") +
    ggtitle("Contribution to Corpus") +
    guides(fill=guide_legend(ncol=1)) +
    theme(legend.title=element_blank())
}

fnGetSourceSummaryStats <- function(dfInMetaData, dfInStats){
  merge(dfInMetaData, dfInStats, by="id") %>%
    group_by(source) %>% 
    summarize(total_articles=n(),total_words=sum(num_words), 
              paragraphs=mean(num_paragraphs),words=mean(num_words), 
              unique=mean(num_unique_words)) %>%
    arrange(desc(total_articles), desc(total_words))
}

fnGetTopNByDay <- function(dfInMetaData, dfInStats, topN){
  merge(dfInMetaData, dfInStats, by="id") %>% 
    filter(num_unique_words > 300) %>%
    group_by(source, published) %>% 
    summarize(unique_words_day=sum(num_unique_words)) %>%
    slice_max(order_by = unique_words_day, n = topN)
}

fnPlotDailyCoverage <- function(dfInMetaData, dfInStats, topN){
  fnGetTopNByDay(dfInMetaData, dfInStats, 5) %>% 
    ggplot(aes(x=published, y=unique_words_day, fill=source, group=source)) +
    geom_bar(width = 1, stat = "identity") +
    guides(fill=guide_legend(ncol=3))
}