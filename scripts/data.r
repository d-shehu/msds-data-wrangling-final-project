library(arrow)
library(dplyr)
library(stringr)
library(tidyr)
library(tools)
library(utils) # Sys utils
library(xml2)

srcDir = Sys.getenv("APP_SRC_DIR")

source(paste(srcDir, "scripts", "utils.r", sep="/"))

# Some useful routines for reading and writing articles to disks. 
# Also for managing the data set (corpus)

fnReadMetaData <- function(metaSourceName){

  dfArticles = NA

  articlesMetaPath <- paste(fnGetDataDir(), paste0(metaSourceName, "_meta.parquet"), sep = "/")

  if (file.exists(articlesMetaPath)){
      dfArticles <- read_parquet(articlesMetaPath)
  }

  return (dfArticles)
}

fnWriteMetaData <- function(dfArticles, metaSourceName){
  
  articlesMetaPath <- paste(fnGetDataDir(), paste0(metaSourceName, "_meta.parquet"), sep = "/")

  write_parquet(dfArticles, articlesMetaPath)
}

fnMergeMetadata <- function(metaSourceName){
  # Match all the individual files for the batches
  pattern <- paste0(metaSourceName, "*")
  # Get the files
  lsFiles <- list.files(fnGetDataDir(), pattern)
  lsFilesPrefix <- str_replace(lsFiles, "_meta.parquet", "")

  sapply(lsFilesPrefix, function(filename, metaSourceName) {
    dfArticlesBatch <- fnReadMetaData(filename)
    if(!is.na(dfArticlesBatch)){
      print(paste("Meta source", metaSourceName))
      fnUpdateMetadata(metaSourceName, dfArticlesBatch)
    }
    else{
      print(paste("Could not read from:", filename))
    }
  }, metaSourceName=metaSourceName)
}

fnUpdateMetadata <- function(metaSourceName, dfNewArticles) {
    
  print(paste("Writing metadata and articles for:", metaSourceName))
  
  # Read the existing meta data from the corresponding parquet
  dfArticles <- fnReadMetaData(metaSourceName)
  if (is.na(dfArticles) || nrow(dfArticles) == 0){
    dfAllArticles <- dfNewArticles
  }
  else{
    # Combine old with new.
    dfAllArticles <- bind_rows(dfArticles, dfNewArticles)
  }

  head(dfAllArticles)
  
  # Combine and remove duplicates assuming source, title and published date
  # are sufficient. Assumes the 1st (original is kept)
  dfAllArticles <- dfAllArticles %>% distinct(across(c(source, title, published)), 
                            .keep_all = TRUE)
  
  # Write back out to parquet file
  fnWriteMetaData(dfAllArticles, metaSourceName)
}

fnGetArticlePath <- function(dfArticle){
  filename = paste0(dfArticle$id, ".html")
  articlePath = paste(fnGetOriginalDir(), filename, sep = "/") #TODO: fix
  
  return(articlePath)
}

fnGetArticleSig <- function(filepath){
  ret <- tryCatch({
    if(file.exists(filepath)){
      # Make sure the file is OK ... i.e. can be parsed.
      htmlFile <- read_html(filepath)
      cHtml <- as.character(htmlFile)
      sizeFromDisk <- as.double(object.size(cHtml))
      hashFromDisk <- digest::digest(cHtml, algo = "md5")
      print("File stats")
      print(sizeFromDisk)
      print(hashFromDisk)
      print("End")

      list("size" = sizeFromDisk, "hash" = hashFromDisk)
    }
    else{
      NULL
    }
  },
  error=function(e){
    print(paste("Error", e, "while calculating chksum and size of", filepath))
    NULL
  })

  return (ret)
}

fnGatherArticle <- function(dfArticle, selClient){
  
  # Now let's write out an individual articles in raw form (HTML). 
  tryCatch({
    articlePath <- fnGetArticlePath(dfArticle)
    print(paste("Reading:", dfArticle$sourceURL))
    if (!dfArticle$onDisk){
      # Use Selenium to grab the article
      selClient$navigate(dfArticle$sourceURL)
      pageSource <- selClient$getPageSource()

      print(paste("Saving to:", articlePath))
      write(pageSource[[1]], articlePath)

      lsSig <- fnGetArticleSig(articlePath)
      if(!is.null(lsSig)){
        dfArticle$fileLen <- lsSig$size
        dfArticle$fileHash <- lsSig$hash
        dfArticle$onDisk <- TRUE
        print(dfArticle)
      }
      else{
        print("Unable to get signature from article file")
      }
    }
  },
  error = function(e){
    print(paste("Error while saving articles:", e))
  })

  # Nothing may have been updated if there was an error
  dfArticle
}

# Let's get articles from all sources (Google and News API are it for now)
fnLoadCorpus <- function(){
    # Some slight differences in meta due to how data was collected
    dfGoogleNewsMeta <- fnReadMetaData("google_news")
    dfNewsAPIMeta <- fnReadMetaData("news_api")

    # Combine into a single frame. Google news doesn't have author which needs to be parsed. 
    # Also clean up some inconsistencies such as date being formatted differently.
    dfGoogleNewsEssential <- dfGoogleNewsMeta %>% select(id, title, published, source, sourceURL)
    dfNewsAPIEssential <- dfNewsAPIMeta %>% select(id, title, published, authors, source, sourceURL)
    dfNewsAPIEssential$published <- as.Date(dfNewsAPIEssential$published) 
    
    bind_rows(dfGoogleNewsEssential, dfNewsAPIEssential)
}

# Not comprehensive
fnGetPhoneNumPattern <- function(){
  "\\d+-\\d+-\\d+"
}

fnGetEmailPattern <- function(){
  "[a-zA-Z0-9+_.-]+@[a-zA-Z0-9.-]+"
}

fnPatternFromTokens <- function(tokens){
  paste0(sapply(tokens, function(phrase) { paste0("((^|\\W)", phrase, "(\\W|$))")
          }), 
          collapse="|")
}

fnSafeStrReplace <- function(str, pattern, replace){
  ifelse(!is.na(str) & !is.na(pattern), str_replace_all(str, pattern, replace), str)
}

fnCleanupAuthors <- function(dfNewsCorpus){

    dfNewsCorpusClean <- dfNewsCorpus

    # See if article is attribute to another source (among the ones in the corpus)
    # Throwing out the 2 character abbreviations (too many bogus matches) and
    # sorting pattern in reverse length for a more precise first match.
    srcMatchPattern <- fnPatternFromTokens(dfNewsCorpusClean %>% 
                          filter(nchar(source) > 2) %>% 
                          arrange(desc(nchar(source))) %>%
                          distinct(tolower(source)))

    # Even the news API has some messy data
    # Remove any URLs,emails from the authors and remove the source itself if it's same as the author
    # Some articles attribute a different source. Extracting only the first one since that's the 
    # vast majority of cases (simpler).
    dfNewsCorpusClean <- dfNewsCorpusClean %>% 
                          mutate(authorsNoURL=str_replace_all(authors, "^http*", "")) %>%
                          mutate(authorsNoEmail=str_replace_all(authorsNoURL, fnGetEmailPattern(), "")) %>%
                          mutate(authorsNoPhone=str_replace_all(authorsNoEmail, fnGetPhoneNumPattern(), "")) %>%
                          mutate(authorsNoSrc=str_replace_all(tolower(authorsNoPhone), tolower(source), "")) %>%
                          mutate(authorsNoExtras=str_replace_all(authorsNoSrc, "(/)|[()]|(and)|(via)|( -)", ",")) %>%
                          mutate(attribute=str_extract(authorsNoExtras, srcMatchPattern)) %>%
                          select(-authors, -authorsNoURL, -authorsNoEmail, -authorsNoPhone, -authorsNoSrc) %>% 
                          rename(authors=authorsNoExtras)
    
    cleanMatchPattern <- "(www.)|(the)|(our reports)|(no author)|(opinion)|(by)|(^ *, *)|( *, *$)|(,\\d+,)|(,\\w,)"

    # Replace NA with "" to facilitate follow up queries
    dfNewsCorpusClean <-  dfNewsCorpusClean %>%
                          mutate(authorsNoAttribute=fnSafeStrReplace(authors, attribute, "")) %>%
                          mutate(authorsClean=fnSafeStrReplace(authorsNoAttribute, 
                                  cleanMatchPattern, "")) %>%
                          mutate(attributeClean=fnSafeStrReplace(attribute, 
                                  cleanMatchPattern, "")) %>%
                          select(-authors, -authorsNoAttribute, -attribute) %>%
                          rename(authors=authorsClean, attribute=attributeClean)

    # Sources are in a relatively decent state. Minor cleanup to standardize and get consistent
    # To lower
    dfNewsCorpusClean <- dfNewsCorpusClean %>% mutate(sourceNoCaps=tolower(source)) %>%
                          select(-source) %>% rename(source=sourceNoCaps)
}

# Retrieve article from the data store and load it in selenium
# to data store so we can process it later
fnReadArticle <- function(dfArticle, selClient, articlesOriginalPath){
  print(dfArticle)
  tryCatch({
    filename = paste0(dfArticle$id, ".html")
    articlePath = paste(articlesOriginalPath, filename, sep = "/") #TODO: fix
    selClient$navigate(articlePath)
  },
  error = function(e){
    print(paste("Error while saving articles:"), e)
  })
}