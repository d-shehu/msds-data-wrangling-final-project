library(dplyr)
library(lubridate)
library(rvest)
library(RCurl)
library(stringr)
library(urltools)
library(tidyr)

srcDir = Sys.getenv("APP_SRC_DIR")

source(paste(srcDir, "scripts", "./selenium.r", sep="/"))
source(paste(srcDir, "scripts", "./utils.r", sep="/"))

# depends on selenium.r

# This block of code uses Google search to fetch as many of the news articles
# as possible, up to approximately 10 pages per search. It searches one day
# at a time in order to get as many articles as are available over a period of
# time and provide some "historical" data.

fnGetRedirectFromURL <- function(selClient, url){
  
  # Return undefined if can't parsed
  redirectURL <- NA
  
  tryCatch(expr = {
      # Get header from URL
      cHeader <- curlGetHeaders(url)
      # Extract the redirect location from the header of the URL and return it
      redirectURL <- as_tibble(cHeader) %>% 
        filter(!is.na(str_match(value, "location"))) %>%
        mutate(redirect=str_extract(value, "[^(location: )]+[(http)].*")) %>% 
        select(link=redirect) %>% head(1)
      
      # Sometimes the location header is missing even though it's a 200
      # So will use selenium to navigate to the url. It's a bit more expensive
      # and definitely not thread safe.
      if( nrow(redirectURL) == 0 ){
        selClient$navigate(url)
        redirectURL <- as_tibble(selClient$getCurrentUrl()[[1]]) %>% 
                                 rename(link=value)
      }
    },
    # On error
    error = function(e){
      print(paste("Can't get redirect URL:", url))
      print(paste("Error:", e))
    }
  )
  
  return (redirectURL)
} 

fnGetSourceFromURL <- function(url) {
    
    # Returned undefined if can't parse
    domain <- NA
    
    # Decompose URL into sub-parts
    tryCatch(expr = {
        # Let's assume URL is of form http(s)://*<domain>.<suffix>/*
        # Source is going to be the domain (NPR, CNN, etc.)
        urlParts <- suffix_extract(domain(url))
        domain <- urlParts$domain
        },
        error = function(e){
          # If can't parse let's return undefined
          print(parse("Can't parse url:", url))
          print(paste("Error:", e))
        }
    )
    
    return (domain)
}

# Sometimes we can't find the node we're looking for. This function is a 
# bit safer in that if an exception is caught, it breaks and returns NULL.
fnGetGoogleNewsFromSearchByDay <- function(date, selClient, sSearchTerms){
    startDate = date
    endDate = date + 1
    
    dfAllArticles = NA

    for(iPage in 0:9){
        
        dfArticles <- tryCatch({
            # We need to paginate so will construct the URL ourselves. But we also
            # want to get as many articles as possible. So we iterate day by day
            # over the specified period (start, end) date.
            # Attributes such as the site to search are encoded with ":".
            url <- paste0("https://www.google.com/search?", "q=", sSearchTerms,
                            "+site%3Anews.google.com",
                            "+before%3A", endDate,
                            "+after%3A", startDate,
                            "&start=", iPage*10,
                            "&lr=lang_en&hl=en-US&gl=US&ceid=US:en")
            
            print(paste0("Fetching URL: ", url))
            # Fetch the page at the current offset
            html <- fnGetHTMLFromSelClient(selClient, url)
            
            dfArticles <- bind_cols(
                                link=html %>%
                                html_nodes(".yuRUbf") %>% 
                                html_node("a") %>% 
                                html_attr("href"),
                                # Article Title
                                title=html %>% 
                                html_nodes(".yuRUbf") %>%
                                html_nodes("h3") %>%
                                html_text(),
                                # Let's assume Google's date is correct (spot checked some)
                                published=startDate
                                ) %>% filter(grepl("https://news.google.com", link))
            
            # Use a UUID for each article to make it easier to lookup
            dfArticles$id <- fnGenIDs(dfArticles)

            # Additional meta data for the file/article data
            dfArticles$fileHash <- ""
            dfArticles$fileLen <- 0
            dfArticles$onDisk <- FALSE
            
            # Return articles
            dfArticles
        },
        error = function(err){
            print("Unexpected error while parsing page")
            print(err)
        })
        
        if (!is.data.frame(dfAllArticles)){
            dfAllArticles <- dfArticles
        }
        else if (is.data.frame(dfArticles)){
            # Combine into dataframe
            dfAllArticles <- bind_rows(dfAllArticles, dfArticles)
        }
        
        # Check if we have more pages. Results are returned 10 at a time in Google.
        # Using this approach as "clicking" on next link didn't seem to work.
        # Using a try catch as Selenium throws an exception if elem not found.
        selStatus <- tryCatch({
            selClient$findElement("css", "#pnnext")
            TRUE
        }, 
        error = function(e){
            print("Assume no more pages ...")
            # Let's assume for now this is 1:1 with no more pages
            FALSE
        })

        if(!selStatus){
            print(paste("Stop iterating through the pages for this day's results", startDate))
            break
        }
    }
    
    return (dfAllArticles)
}

fnGetGoogleNewsFromSearch <- function(selClient, cSearchTerms, startDate, endDate){

    print(paste("Getting news sources from searching Google news from", 
                startDate, "to", endDate))

    #Combine search terms into a single string of form <Term1>+<Term2>... 
    sSearchTerms <- paste(cSearchTerms, collapse="+")

    # Get all the days in the sequence from start to end date. 
    # End date is excluded.
    dates <- as.list(seq(startDate, endDate-1, "days"))

    # Call for each date and get one date frame of article links,URLs
    lsArticleDFs <- lapply(dates, fnGetGoogleNewsFromSearchByDay, 
                            selClient=selClient,
                            sSearchTerms=sSearchTerms)

    # Merge into tibble for all days and remove any duplicates
    dfAllArticles <- bind_rows(lsArticleDFs) %>% distinct()

    print("Get source URL from redirect URL (Google news)...")

    # Get the source URLs from the Google redirect links
    dfAllArticles$sourceURL <- sapply(dfAllArticles$link, fnGetRedirectFromURL, 
                                    selClient=selClient)

    # Extract the source from the source URL
    dfAllArticles$source <- sapply(dfAllArticles$sourceURL, fnGetSourceFromURL)

    # Source URL is stored as list. Flatten column since there is only 1 URL per article.
    dfAllArticles <- unnest(dfAllArticles, sourceURL)

    return (dfAllArticles)
}

# This snippet holds functions used to parse Google News. The 1st pass was
# an attempt to fetch and parse articles directly from Google News. However,
# Google news has a limitations of approximately 100 articles.

# Deprecated!
# Get articles from Google news directly
fnGetGoogleNewsArticles <- function(selClient, cSearchTerms){
  sSearchTerms <- paste(cSearchTerms, collapse=" ")
    
  url <- str_replace_all(paste0("https://news.google.com/search?q=", 
                                sSearchTerms, 
                                "&lr=lang_en&hl=en-US&gl=US&ceid=US:en"),
                         " ", "%20")
  
  # Replace rvest call with call to selenium to grab as
  # much of the data as possible.
  html <- fnGetHTML(selClient, url)
    
  # Data frame with each article obtained from parsing Google news
  dfArticles <- bind_cols(link=html %>%
                    html_nodes(".DY5T1d") %>% 
                    html_attr("href") %>% 
                        {gsub("./articles/", 
                              "https://news.google.com/articles/", .)},
                    # Article Title
                    title=html %>% 
                      html_nodes(".DY5T1d") %>%
                      html_text(),
                    # Source (CNN, etc.)
                    source=html %>% 
                      html_nodes(".wEwyrc") %>% 
                      html_text(),
                    # Date and Time in UTC
                    dateTime=as_datetime(ymd_hms(html %>% 
                                       html_nodes(".WW6dff") %>% 
                                       html_attr("datetime"))))
  return (dfArticles)
}