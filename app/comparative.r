library(dplyr)
library(ggplot2)

library(tidyr)
library(tidytext)
library(textdata)

################################################################################
# Comparative Analysis
#
# Classic text mining functionality that extracts common words and word pairs
# with the goal of trying to surface key concepts and facts from the articles.
#
################################################################################

fnGetWordFrequency <- function(dfMetaData, dfAllTerms, inSource, topN){
  merge(dfMetaData, dfAllTerms, by="id") %>% 
    filter(nchar(word) > 4) %>%
    filter(source=={{inSource}}) %>%
    select(word) %>%
    count(word, sort=TRUE) %>%
    head(topN)
}

fnCompareSources <- function(dfMetaData, dfAllTerms, sourceA, sourceB, topN){
  # Limit to 100 most common
  dfSourceA <- fnGetWordFrequency(dfMetaData, dfAllTerms, sourceA, topN)
  dfSourceB <- fnGetWordFrequency(dfMetaData, dfAllTerms, sourceB, topN)
  
  dfCombined <- bind_rows(mutate(dfSourceA, source = sourceA),
                          mutate(dfSourceB, source = sourceB)) %>%
    mutate(word = str_extract(word, "[a-z]+")) %>%
    count(source, word) %>%
    group_by(source) %>%
    mutate(prop = n / sum(n)) %>%
    arrange(desc(n)) %>%
    select(-n) %>% 
    pivot_wider(names_from = "source", values_from = "prop")
  
  dfCombined
}

fnGetInOneSource <- function(dfCombined, source, nTop){
  dfCombined %>% 
    filter(is.na(.[[2]]) != is.na(.[[3]])) %>% 
    filter(!is.na(!!as.symbol(source))) %>% 
    arrange(desc(word)) %>% 
    select(word, !!as.symbol(source)) %>% 
    head(nTop)
}

fnPlotCompareWordUsage <- function(dfCombined){
  ggplot(dfCombined, aes_string(x=names(dfCombined)[2], 
                                y=names(dfCombined)[3])) +
    geom_abline(color="red", lty=3, lwd=3) +
    geom_point(color="black") +
    geom_text(aes(label=word), position=position_jitter(width=1,height=1)) + 
    scale_x_log10() + 
    scale_y_log10()
}