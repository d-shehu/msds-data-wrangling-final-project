args = commandArgs(trailingOnly=TRUE)

srcDir = Sys.getenv("APP_SRC_DIR")
dataDir = Sys.getenv("APP_DATA_DIR")

source(paste(srcDir, "scripts", "data.r", sep="/"))
source(paste(srcDir, "scripts", "parseNews.r", sep="/"))
source(paste(srcDir, "scripts", "processNews.r", sep="/"))
source(paste(srcDir, "scripts", "utils.r", sep="/"))


fnProcessAllArticles <- function(dfInCorpus){

    # Aggregate dataframe for all articles
    dfTextSanitizedAll <- tibble()
    dfTermsAll <- tibble()
    dfTrigramsAll <- tibble()
    dfArticleStatsAll <- tibble()

    # Semantics
    dfRootsAll <- tibble()
    dfEntitiesAll <- tibble()
    dfPhrasesAll <- tibble()

    lsNotProcessed <- list()

    # This is a bit more readable as a for loop
    for (i in 1:nrow(dfInCorpus))
    {
        dfArticle <- dfInCorpus[i,]

        # Get Article path 
        sArticlePath <- fnGetArticlePath(dfArticle)

        #print(paste("Processing: ", sArticlePath))
        if((i %% 10) == 1){
            print(paste("Processed:", i))
        }

        # Read from disk
        articleText <- fnParseArticle(sArticlePath)
        if(length(articleText) > 0){
            # Convert to tidy format
            dfText <- fnGetDataFrameFromText(articleText)

            # Sanitized version with no stop words
            dfTextSanitized <- fnGetSanitizedDataFrame(dfText)

            # Get common terms from the sanitized text
            dfTerms <- fnGetTermsDataFrame(dfTextSanitized)

            # Process trigrams
            dfTrigrams <- fnGetTrigramDataFrame(dfText)

            # Get stats (useful for filtering articles)
            dfArticleStats <- fnGetBasicStats(dfTextSanitized)

            # Append the article ID so we can combine with other articles
            # Since we will do some "temporal" analysis let's also add the
            # date to simplify things a bit (reduce joins)

            # Serial id is just the i (article index) and can be used
            # for those routines like dtm which need a numeric ID
            dfTextSanitized$serial <- i
            dfTextSanitized$id <- dfArticle$id
            dfTextSanitized$published <- dfArticle$published

            dfTerms$id <- dfArticle$id
            dfTerms$published <- dfArticle$published
            dfTrigrams$id <- dfArticle$id
            dfTrigrams$published <- dfArticle$published

            dfArticleStats$id <- dfArticle$id

            # Combine into a single set
            dfTextSanitizedAll <- rbind(dfTextSanitizedAll, dfTextSanitized)
            dfTermsAll <- rbind(dfTermsAll, dfTerms)
            dfTrigramsAll <- rbind(dfTrigramsAll, dfTrigrams)
            dfArticleStatsAll <- rbind(dfArticleStatsAll, dfArticleStats)

            # Now the semantics
            # Get roots, entities and noun phrases
            dfSemantics <- fnProcessArticleSemantics(dfText)

            # Get root words
            dfRoots <- tryCatch({
                dfRoots <- fnExtractRootWord(dfSemantics)
                dfRoots$id <- dfArticle$id
                dfRoots$published <- dfArticle$published
                dfRoots
            },
            error = function(e){
                print(paste("Could not extract root:", e))
                tibble()
            })
            
            dfEntities <- tryCatch({
                dfEntities <- fnExtractEntities(dfSemantics)
                dfEntities$id <- dfArticle$id
                dfEntities$published <- dfArticle$published
                dfEntities
            },
            error = function(e){
                print(paste("Could not extract entities:", e))
                tibble()
            })
            
            dfPhrases <- tryCatch({
                dfPhrases <- fnExtractNounPhrases(dfSemantics)
                dfPhrases$id <- dfArticle$id
                dfPhrases$published <- dfArticle$published
                dfPhrases
            },
            error = function(e){
                print(paste("Could not extract nounphrases:", e))
                tibble()
            })

            # Combine into a single set
            if(nrow(dfRoots) > 0){
                dfRootsAll <- rbind(dfRootsAll, dfRoots)
            }
            
            if(nrow(dfEntities) > 0){
                dfEntitiesAll <- rbind(dfEntitiesAll, dfEntities)
            }
            
            if(nrow(dfPhrases) > 0){
                print("Phrases now:")
                dfPhrasesAll <- rbind(dfPhrasesAll, dfPhrases)
            }
            
        }
        else{
            append(lsNotProcessed, sArticlePath)
        }
    }

    # Return as collection of lists
    list(   
            dfAlldfTextSanitized=dfTextSanitizedAll,
            dfAllTerms=dfTermsAll, 
            dfAllTrigrams=dfTrigramsAll, 
            dfAllArticleStats=dfArticleStatsAll,
            lsNotProcessed=lsNotProcessed, 
            dfRootsAll=dfRootsAll, 
            dfEntitiesAll=dfEntitiesAll,
            dfPhrasesAll=dfPhrasesAll
    )
}

# Get corpus of all articles
dfNewsCorpus <- fnLoadCorpus()

# Process Article text
lsOut <- fnProcessAllArticles(dfNewsCorpus)

# Cleanup Author
dfNewsCorpus <- fnCleanupAuthors(dfNewsCorpus)

write_parquet(lsOut$dfAlldfTextSanitized, paste(fnGetParsedDir(), "allTextSanitized.parquet", sep="/"))
write_parquet(lsOut$dfAllTerms, paste(fnGetProcessedDir(), "allTerms.parquet", sep="/"))
write_parquet(lsOut$dfAllTrigrams, paste(fnGetProcessedDir(), "allTrigrams.parquet", sep = "/"))
write_parquet(lsOut$dfAllArticleStats, paste(fnGetStatsDir(), "allStats.parquet", sep="/"))

# Semantics
write_parquet(lsOut$dfRootsAll, paste(fnGetProcessedDir(), 
                                            "allRoots.parquet", sep="/"))
write_parquet(lsOut$dfEntitiesAll, paste(fnGetProcessedDir(), 
                                               "allEntities.parquet", sep = "/"))
write_parquet(lsOut$dfPhrasesAll, paste(fnGetProcessedDir(), 
                                               "allPhrases.parquet", sep = "/"))

# Add parquet files for semantics

print(paste("Number of articles not processed:", length(lsOut$lsNotProcessed)))
print("Writing list of articles which were not processed")
if(length(lsOut$lsNotProcessed)){
    fOut <- file("unprocessArticles.txt")
    writeLines(unlist(lsOut$lsNotProcessed), fOut)
    close(fOut)
}



