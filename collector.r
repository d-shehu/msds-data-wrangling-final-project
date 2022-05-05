args = commandArgs(trailingOnly=TRUE)

srcDir = Sys.getenv("APP_SRC_DIR")

source(paste(srcDir, "scripts", "data.r", sep="/"))
source(paste(srcDir, "scripts", "selenium.r", sep="/"))
source(paste(srcDir, "scripts", "utils.r", sep="/"))

# Read meta data. A bit inefficient but should be safer
metaSource <- args[1]
print(paste("Reading from meta data:", metaSource))
dfArticles <- fnReadMetaData(metaSource)

selDriver = NULL

for(i in 1:nrow(dfArticles)) {

  print(paste("Processing article", i))
  bFlush = ((i %% 100) == 1)
  if (bFlush){
    print("Reseting Selenium and flushing to disk")
    # Selenium/browser are flaky and seems to error out after a while
    # if left running. So reset with each batch 
    if(!is.null(selDriver)){
      fnCleanupSelenium(selDriver, selClient)

      # Also write updates to meta data
      fnWriteMetaData(dfArticles, metaSource)
    }

    lRet <- fnGetSelenium(6831L, T)

    selDriver <- lRet[[1]]
    selClient <- lRet[[2]]
  }

  # Download each article and then update meta data record
  # But only if we haven't done so before. Some articles (paywall)
  # error out so we need a mechanism for skipping those.
  if(!dfArticles$onDisk[i]){
    dfUpdatedArticle <- fnGatherArticle(dfArticles[i,], selClient)
    dfArticles[i,] <- dfUpdatedArticle
  }
  else{
    print(paste("Already downloaded article", i))
  }
}

# Also write updates to meta data
fnWriteMetaData(dfArticles, metaSource)

# Clean up
fnCleanupSelenium(selDriver, selClient) 

