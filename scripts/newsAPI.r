# Code to work with News.API augmenting Google News from Google search

fnGetNewsFromNewsAPIByDay <- function(date, sSearchTerms, pageSize){
  
  # Don't hard code API key in code
  newsAPIKey <- ""

  # Assume no articles were generated
  dfNewsAPIArticles <- NA
  
  # Get exactly 1 day's worth of news assuming UTC timezone
  startDateTime <- paste0(date, "T00:00:00")
  endDateTime <- paste0(date, "T23:59:59")
  
  print(paste("Get news from", startDateTime, endDateTime))
  tryCatch({
    # Call API. Notes there are some limitations on how far back in time we
    # can go since this is a free account.
    newsAPIJSON <- getForm(uri = "https://newsapi.org/v2/everything",
                        "q" = sSearchTerms,
                        "from" = startDateTime,
                        "to" = endDateTime,
                        "pageSize" = 100,
                        "language" = "en",
                        "sortBy" = "publishedAt",
                        "apiKey" = newsAPIKey)
    
    newsAPIInfo <- newsAPIJSON %>% fromJSON()
    
    dfNewsAPIArticles <- as_tibble(newsAPIInfo$articles) %>% 
    unnest(source, names_sep = ".") %>%
    select(title=title, published=publishedAt, sourceURL=url, 
        source=source.name, authors=author)
    
    dfNewsAPIArticles$id <- fnGenIDs(dfNewsAPIArticles)

    # Additional meta data for the file/article data
    dfNewsAPIArticles$fileHash <- ""
    dfNewsAPIArticles$fileLen <- 0
    dfNewsAPIArticles$onDisk <- FALSE
    
    # Sleep 1 second per API request to avoid throttling
    Sys.sleep(1)
  },
  error = function(e){
    print(paste("Error while getting or parsing News API", e))
    e # TODO: parse
  })
  
  return (dfNewsAPIArticles)
}

fnGetNewsFromNewsAPI <- function(cSearchTerms, startDate, endDate){
  
  print(paste("Getting news sources from News API", 
               startDate, "to", endDate))
  
  #Combine search terms into a single string of form <Term1>+<Term2>... 
  sSearchTerms <- paste(cSearchTerms, collapse="+")
  
  # Get all the days in the sequence from start to end date. 
  # End date is excluded.
  dates <- as.list(seq(startDate, endDate-1, "days"))
  
  # Call for each date and get one date frame of article links,URLs
  lsArticleDFs <- lapply(dates, fnGetNewsFromNewsAPIByDay, 
                         sSearchTerms=sSearchTerms,
                         pageSize=100)
  
  # Merge into tibble for all days and remove any duplicates
  dfAllArticles <- bind_rows(lsArticleDFs) %>% distinct()
  
  return (dfAllArticles)
}