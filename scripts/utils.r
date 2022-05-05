library(uuid)

# Wrapper that takes a dataframe and returns one UUID for
# every row of the dataframe.
fnGenIDs <- function(df){
  apply(df, MARGIN=1, FUN=function(x){UUIDgenerate()})
}

# Data repo for this project
fnGetDataDir <- function() {
  Sys.getenv("APP_DATA_DIR")
}

# Articles in their original form (HTML)
fnGetOriginalDir <- function() {
  paste(fnGetDataDir(), "original", sep = "/")
}

# Articles in their parsed form (i.e tokenized)
fnGetParsedDir <- function() {
  paste(fnGetDataDir(), "parsed", sep = "/")
}

# Processed data for each article: tokenized, n-grams and lemmatization
fnGetProcessedDir <- function() {
  paste(fnGetDataDir(), "processed", sep = "/")
}

# Statistics per article
fnGetStatsDir <- function() {
  paste(fnGetDataDir(), "stats", sep = "/")
}