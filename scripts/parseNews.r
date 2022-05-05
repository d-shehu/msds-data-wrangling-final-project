# Some functions for parsing individual news sources such as CNN, etc. in order
# to extract more data such as the author. Feasibility of writing a custom
# parser for each source will depend on the # of sources and complexity of meta
# data needed.

fnParseCNNArticle <- function(url){
  
  html <- rvest::read_html(url)
  
  # Get the headline
  headline <- html %>% rvest::html_nodes("h1") %>% rvest::html_text()
  
  # Get the author from the byline
  byline <- html %>% rvest::html_nodes("p[data-type='byline-area']") %>% 
    rvest::html_text()
  
  # Get list of authors
  authors <- unlist(str_split(str_replace_all(byline, "(By )|(and)", ""), ","))

  # Get the date and time it was published
  published <- as_date(html %>% html_nodes("meta[property='article:published_time']") 
  %>% html_attr("content"))

  # Get the paragraphs and then convert to text
  articleText <- html %>% rvest::html_nodes("p") %>% rvest::html_text()
  
  # Collect article info as class
  lFields <- list(title=headline, authors=authors, published=published,
                  text=articleText, url=url, source="CNN")
  
  class(lFields) <- "ArticleInfo"
}

# uri can be URL or path to local file
fnParseArticle <- function(uri) {

  paragraphs <- tryCatch({
      # Read from local or remote file
      html <- rvest::read_html(uri)
      
      # Read the paragraphs where the text is presumably
      html %>% rvest::html_nodes("p") %>% rvest::html_text()
  }, 
  error = function(e){
      print(paste("Error while trying to parse file:", uri))
      # rvest return character(0) when it's not able to extract paragraphs
      character(0)
  })

  paragraphs
}