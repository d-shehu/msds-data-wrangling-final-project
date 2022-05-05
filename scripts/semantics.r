library(spacyr)

fnInit <- function(){
    spacy_initialize(model = "en_core_web_sm")
}

fnProcessSemantics <- function(dfInSentence){
    tryCatch(spacy_parse(dfInSentence, dependency = TRUE, lemma = TRUE,
                        nounphrase = TRUE),
                        error = function(e){
                            print(paste("Error while processing semantics:", e))
                            print(paste("Could not get semantics from sentence:", dfInSentence))
                            tibble()
                        })
}

fnExtractRootWord <- function(dfSpacyText){
    dfSpacyText %>%
        select(lemma) %>% 
        rename(word=lemma) %>%
        anti_join(stop_words) %>%
        filter(grepl("[a-z]+", word)) %>%
        group_by(word) %>%
        summarize(n=n()) %>% 
        arrange(desc(n)) %>%
        filter(grepl("^\\w+", word))
}

fnExtractEntities <- function(dfSpacyText){
    entity_consolidate(dfSpacyText) %>%
        filter(entity_type!="") %>%
        select(token, entity_type) %>%
        rename(entity=token) %>%
        group_by(entity, entity_type) %>%
        summarize(n=n()) %>%
        arrange(desc(n))
}

fnExtractNounPhrases <- function(dfSpacyText){
    nounphrase_consolidate(dfSpacyText) %>% 
        filter(pos=="nounphrase") %>% 
        select(token) %>%
        rename(phrase=token) %>%
        group_by(phrase) %>%
        summarize(n=n()) %>%
        arrange(desc(n))
}

fnProcessArticleSemantics <- function(dfInArticleText){
    # Doing this for every sentence to keep the results comparable with
    # other representation (words and paragraphs)
    lSentenceSemantics <- lapply(dfInArticleText$sentence, fnProcessSemantics)

    lSentenceSemantics %>% bind_rows()
    #do.call("rbind", lSentenceSemantics) #%>% 
        #select(token, lemma, pos, dep_rel, entity)

    # lSentenceRootWords <- lapply(lSentenceSemantics, fnExtractRootWord)
    # lSentenceEntities <- lapply(lSentenceSemantics, fnExtractEntities)
    # lSentenceNounPhrases <- lapply(lSentenceSemantics, fnExtractNounPhrases)

    # list(lsRoots <- lSentenceRootWords, lsEntities <- lSentenceEntities, 
    #         lsNounPhrases <- lSentenceNounPhrases)
}