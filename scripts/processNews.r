library(dplyr)
library(tokenizers)
library(tidytext)

# Basic unit of text is going paragraphs instead of lines.
# Paragraphs is how we extract text from HTML.
# Because of the number and variety of sources it should
# result in a more consistent way of handling articles  
# However we will also annotate with sentences since that
# should correspond losely to statements (and ideally facts).
fnGetDataFrameFromText <- function(inText) {
    tibble(text=inText) %>%
        mutate(paragraph_num = row_number()) %>%
        mutate(sentence = tokenize_sentences(text)) %>%
        select(-text) %>%
        unnest(sentence) %>%
        mutate(sentence_num = row_number())
}

# Remove stop words and other cleaning
fnGetSanitizedDataFrame <- function(inTextDf){
    # Unnest and remove stop words and then recombine into sentences
    inTextDf %>% 
        unnest_tokens(word, sentence, to_lower=TRUE) %>%
        anti_join(stop_words) %>% 
        group_by(paragraph_num, sentence_num) %>%
        summarize(sentence=str_c(word, collapse=" "), .groups="keep") %>%
        ungroup()
}

# Get terms by just tokenizing the text
fnGetTermsDataFrame <- function(dfInSanitizedText){
    dfInSanitizedText %>% 
        unnest_tokens(word, sentence) %>%
        select(word) %>%
        group_by(word) %>%
        summarize(n=n()) %>%
        arrange(desc(n))
}

# Takes in unsanitized text and remove tri-grams with a stop word
# Trigram seemed to produce the more meaningful results
fnGetTrigramDataFrame <- function(dfInText){
    dfInText %>% 
        unnest_tokens(trigram, sentence, token = "ngrams", n = 3) %>%
        separate(trigram, c("w1", "w2", "w3"), sep=" ") %>%
        filter(!w1 %in% stop_words$word) %>%
        filter(!w2 %in% stop_words$word) %>%
        filter(!w3 %in% stop_words$word) %>%
        unite(trigram, w1, w2, w3, sep=" ") %>%
        count(trigram, sort = TRUE)
}

fnGetBasicStats <- function(dfInText){
    
    numParagraphs <- nrow(dfInText %>% select(paragraph_num) %>% distinct())
    numSentences <- nrow(dfInText)

    dfWords <- dfInText %>% unnest_tokens(word, sentence)
    dfWordsPerSentence <- dfWords %>% count(sentence_num)

    numWords <- sum(dfWordsPerSentence$n)
    numUniqueWords <- length(unique(dfWords$word))
    maxWordPerSentence <- max(dfWordsPerSentence$n)
    avgWordPerSentence <- mean(dfWordsPerSentence$n)
    sdWordPerSentence <- sd(dfWordsPerSentence$n)

    maxWordLen <- max(str_length(dfWords$word))
    avgWordLen <- mean(str_length(dfWords$word))
    sdWordLen <- sd(str_length(dfWords$word))

    tibble(num_paragraphs = numParagraphs, num_sentences=numSentences, num_words=numWords,
        num_unique_words=numUniqueWords,
        max_word_sentence=maxWordPerSentence, avg_word_sentence=avgWordPerSentence, 
        sd_word_sentence=sdWordPerSentence,
        max_word_len=maxWordLen, avg_word_len=avgWordLen, sd_word_len=sdWordLen)
}
