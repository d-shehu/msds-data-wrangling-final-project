################################################################################
# Data
#
# Code used to read from S3 and data util functions
#
################################################################################

##############################################################################
# Data routines for loading parquet files from S3 and utility functions
# common to the whole application.
##############################################################################

fetchFile <- function(url){
  fTemp <- tempfile()
  download.file(url, fTemp, mode="wb")
  fTemp
}

fnGetDataDir <- function(dataset){
  paste("https://s3.amazonaws.com/s3.msds.rutgers.org/data_wrangling",
        dataset, sep="/")
}

fnReadMetaData <- function(dataset){
  urlS3 <- fnGetDataDir(dataset)
  
  dfGoogleNewsMeta <- read_parquet(fetchFile(paste(urlS3,"google_news_meta.parquet",
                                                   sep="/")))
  dfNewsAPIMeta <- read_parquet(fetchFile(paste(urlS3,"news_api_meta.parquet",
                                                sep="/")))
  
  # Combine into a single frame. Google news doesn't have author which needs to be parsed.
  # Also clean up some inconsistencies such as date being formatted differently.
  dfGoogleNewsEssential <- dfGoogleNewsMeta %>% select(id, title, published,
                                                       source, sourceURL)
  dfNewsAPIEssential <- dfNewsAPIMeta %>% select(id, title, published, authors,
                                                 source, sourceURL)
  dfNewsAPIEssential$published <- as.Date(dfNewsAPIEssential$published)
  
  bind_rows(dfGoogleNewsEssential, dfNewsAPIEssential)
}

fnSafeRead <- function(urlS3, filename){
  df <- tryCatch({
    read_parquet(fetchFile(paste(urlS3,filename, sep="/")))
  },
  error = function(e){
    print(paste("Error could not read file:", filename, "due to:", e))
    tibble()
  })
  
  return (df)
}

fnReadData <- function(dataset){
  urlS3 <- fnGetDataDir(dataset)
  
  # Read sanitized text
  dfAllTextSanitized <- fnSafeRead(urlS3,"allTextSanitized.parquet")
  dfAllTerms <- fnSafeRead(urlS3,"allTerms.parquet")
  dfAllTrigrams <- fnSafeRead(urlS3,"allTrigrams.parquet")
  dfAllStats <- fnSafeRead(urlS3,"allStats.parquet")
  
  # Read semantic data if available
  dfAllRoots <- fnSafeRead(urlS3,"allRoots.parquet")
  dfAllEntities <- fnSafeRead(urlS3,"allEntities.parquet")
  # Noun phrases
  dfAllPhrases <- fnSafeRead(urlS3,"allPhrases.parquet")
  
  # Return data set as a list of data frames
  list(dfAllTextSanitized=dfAllTextSanitized, dfAllTerms=dfAllTerms,
       dfAllTrigrams=dfAllTrigrams,dfAllStats=dfAllStats, dfAllRoots=dfAllRoots,
       dfAllEntities =  dfAllEntities, dfAllPhrases = dfAllPhrases)
}

fnGetFilteredMetaData <- function(dfInMetaData, startDate, endDate, dfSources){
  # Filtered on time and by source
  dfFilteredMetaData <- dfInMetaData %>% 
    filter(published >= startDate & published <= endDate)
  
  # Filter by 1 or more sources
  if(nrow(dfSources) > 0){
    # Semi join as we only care about the filtered meta data
    dfFilteredMetaData <- semi_join(dfFilteredMetaData, dfSources, 
                                    by="source")
  }
  
  return (dfFilteredMetaData)
}


fnGetFilteredDataSet <- function(dfInMetaData, lsInData){
  # Filter each data set using the filtered meta
  dfFilteredTextSanitized <- semi_join(lsInData$dfAllTextSanitized,
                                       dfInMetaData, by="id")
  
  dfFilteredTerms <- semi_join(lsInData$dfAllTerms,
                               dfInMetaData, by="id")
  
  dfFilteredTrigrams <- semi_join(lsInData$dfAllTrigrams,
                                  dfInMetaData, by="id")
  
  dfFilteredStats <- semi_join(lsInData$dfAllStats,
                               dfInMetaData, by="id")
  
  # Semantics if available
  dfFilteredRoots <- semi_join(lsInData$dfAllRoots,
                               dfInMetaData, by="id")
  
  dfFilteredEntities <- semi_join(lsInData$dfAllEntities,
                                  dfInMetaData, by="id")
  
  dfFilteredPhrases <- semi_join(lsInData$dfAllPhrases,
                               dfInMetaData, by="id")
  
  list(dfAllTextSanitized=dfFilteredTextSanitized, dfAllTerms=dfFilteredTerms, 
       dfAllTrigrams=dfFilteredTrigrams, dfAllStats=dfFilteredStats,
       dfAllRoots=dfFilteredRoots, dfAllEntities=dfFilteredEntities, 
       dfAllPhrases=dfFilteredPhrases)
}

