args = commandArgs(trailingOnly=TRUE)

srcDir = Sys.getenv("APP_SRC_DIR")

source(paste(srcDir, "scripts", "data.r", sep="/"))
source(paste(srcDir, "scripts", "googleNews.r", sep="/"))
source(paste(srcDir, "scripts", "selenium.r", sep="/"))
source(paste(srcDir, "scripts", "utils.r", sep="/"))

# Search params
keyWords <- c("Ukraine", "War")
startDate <- ymd(args[1])
endDate <- ymd(args[2])

currentDate <- startDate

while(currentDate < endDate){

    # Selenium seems to error out beyond a certain number of requests.
    # Unclear if this is because it was blocked, etc.
    # So grab articles in batches (5 days) and restart Selenium with every batch.
    lRet <- fnGetSelenium(6832L, T)
    selDriver <- lRet[[1]]
    selClient <- lRet[[2]]

    nextDate <- min(currentDate + 5, endDate)
    
    print(paste("Scraping Google news via search from", startDate, "to", nextDate))

    dfGoogleNewsArticles <- fnGetGoogleNewsFromSearch(selClient, keyWords,
                                           currentDate, 
                                           nextDate)

    sMetadataName <- paste0("google_news_", startDate, "_", endDate)
    fnUpdateMetadata(sMetadataName, dfGoogleNewsArticles)

    # Clean connection safely
    fnCleanupSelenium(selDriver, selClient)

    # Move to the next batch
    currentDate <- nextDate
}