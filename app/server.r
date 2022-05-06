################################################################################
# Server
#
# Brings together all the individual functionality of this app and connect
# to UI.
#
################################################################################

library(arrow)
library(dplyr)

source("data.r")
source("summary.r")
source("terms.r")
source("comparative.r")
source("sentiment.r")
source("topic_modeling.r")
source("semantics.r")

#######################
# Functions to init UI
#######################

fnGetSourcesByArticleCount <- function(dfInMetaData, dfInAllStats){
  # Sort the list of sources by total words contributed
  # to the Corpus
  merge(dfInMetaData, dfInAllStats, by="id") %>% 
    group_by(source) %>% 
    summarize(sum_words=sum(num_words)) %>% 
    arrange(desc(sum_words)) %>% 
    select(source)
}

#######################
# Utility
#######################

fnCreateLink <- function(url, text) {
  sprintf('<a href="%s" target="_blank" class="btn btn-primary">%s</a>', url,
          text)
}

fnGetCmpSelection <- function(idx, dfInSource){
  if(idx < nrow(dfInSource)){
    print("Selection of source:")
    print(dfInSource[idx,])
    return (dfInSource[idx,])
  }
  else{
    return (NULL)
  }
}

server <- function(input, output, session) {
  
  #######################
  # Event Handlers
  #######################
  
  # Handle change to dataset by reloading from S3
  observeEvent(input$dataset,{
    if(input$dataset != ""){
      # Update the date filter from the earliest,latest date from corpus
      minDate=min(MetaData()$published)
      maxDate=max(MetaData()$published)
      print(paste("Articles Range:", minDate, maxDate))

      # Load the whole range by default
      updateSliderInput(inputId = "dateRange", min = minDate)
      updateSliderInput(inputId = "dateRange", max = maxDate)
      updateSliderInput(inputId = "dateRange", value=c(minDate, maxDate))

      # Load all sources
      updateSelectInput(inputId = "sources" , choices = AvailableSources())
      updateSelectInput(inputId = "cmpSourceA" , choices = AvailableSources(),
                        selected = fnGetCmpSelection(1, AvailableSources()))
      updateSelectInput(inputId = "cmpSourceB" , choices = AvailableSources(),
                        selected = fnGetCmpSelection(2, AvailableSources()))
    }
  })
  
  #######################
  # Reactive Variables
  #######################
  
  MetaData <- reactive({
    if(input$dataset != ""){
      return(fnReadMetaData(input$dataset))
    }
    else{
      return(tibble())
    }
  })
  
  DataSet <- reactive({
    if(input$dataset != ""){
      return(fnReadData(input$dataset))
    }
    else{
      return(list())
    }
  })
  
  AvailableSources <- reactive({
    if(nrow(MetaData()) > 0){
      return(fnGetSourcesByArticleCount(MetaData(),
                                 DataSet()$dfAllStats))
    }
    else{
      return(tibble())
    }
  })
  
  FilteredMetaData <- reactive({
    
    if(nrow(MetaData()) > 0){
      startDate <- input$dateRange[1]
      endDate <- input$dateRange[2]
    
      selectedSources <- tibble(source=input$sources)
      if(nrow(selectedSources) == 0){
        selectedSources <- fnGetSourcesByArticleCount(MetaData(), 
                                                      DataSet()$dfAllStats)
      }
      return (fnGetFilteredMetaData(MetaData(), startDate, endDate,
                                                   selectedSources))
    }
    else{
      return (tibble())
    }
  })
  
  FilteredDataSet <- reactive({
    if(nrow(FilteredMetaData()) > 0){
      return (fnGetFilteredDataSet(FilteredMetaData(), DataSet()))
    }
    else{
      return (list())
    }
  })
  
  FilteredSources <- reactive({
    if(nrow(FilteredMetaData()) > 0){
      return (semi_join(AvailableSources(), FilteredMetaData(), by="source"))
    }
    else{
      return (tibble())
    }
  })
  
  NumWordCloud <- reactive({
    input$numWordsInCloud
  })

  NumTop <- reactive({
    input$numTop
  })
  
  WordsPerWeek <- reactive({
    input$wordsPerWeek
  })
  
  CmpSourceA <- reactive({
    input$cmpSourceA
  })
  
  CmpSourceB <- reactive({
    input$cmpSourceB
  })
  
  CmpAnalyzeMaxWords <- reactive({
    input$cmpAnalyzeMaxWords
  })
  
  CmpShowNumWords <- reactive({
    input$cmpShowNumWords
  })
  
  SAMethod <- reactive({
    input$saMethod
  })
  
  TMMethod <- reactive({
    input$tmMethod
  })
  
  NumTopics <- reactive({
    input$tmNumTopics
  })
  
  NewsTopics <- reactive({
    if(nrow(FilteredMetaData()) > 0){
      dfFilteredTerms <- FilteredDataSet()$dfAllTerms
      print(dfFilteredTerms)
      dfTidyTerms <- fnGetTidyTerms(FilteredMetaData(), dfFilteredTerms)
      
      fnGetNewsTopics(dfTidyTerms, NumTopics(), TMMethod())
    }
    else{
      return (tibble())
    }
  })
  
  TopNumSemantics <- reactive({
    input$semNumTopWords
  })
  
  #######################
  # Summary Stats
  #######################
  
  #req checks to make sure data has been loaded
  output$sourceCoverage <- renderPlot({
    req(nrow(FilteredMetaData()) > 0)
    fnPlotSources(FilteredMetaData())
  })

  output$sourceContribution <- renderPlot({
    req(nrow(FilteredMetaData()) > 0 & length(FilteredDataSet()) > 0)
    fnPlotSourceContribution(FilteredMetaData(), FilteredDataSet()$dfAllStats, 20)
  })

  # Calculate some basic (summary statistic on the source)
  output$sourceSummaryStats <- renderTable({
    req(nrow(FilteredMetaData()) > 0 & length(FilteredDataSet()) > 0)
    fnGetSourceSummaryStats(FilteredMetaData(), FilteredDataSet()$dfAllStats) %>%
      head(20)
  })

  # News Coverage by Day
  output$sourceDailyCoverage <- renderPlot({
    req(nrow(FilteredMetaData()) > 0 & length(FilteredDataSet()) > 0)
    fnPlotDailyCoverage(FilteredMetaData(), FilteredDataSet()$dfAllStats, 10, 3)
  })
  
  #######################
  # Terms & Concepts
  #######################
  
  # Word cloud of key terms
  output$topWordsCloud <- renderPlot({
    req(length(FilteredDataSet()) > 0)
    fnCreateWordCloud(FilteredDataSet()$dfAllTerms, 10, NumWordCloud())
  })

  # Top words
  output$topWords <- renderPlot({
    req(nrow(FilteredMetaData()) > 0 & length(FilteredDataSet()) > 0)
    fnPlotTopWords(FilteredMetaData(), FilteredDataSet()$dfAllTerms, NumTop())
  })

  # Top trigrams
  output$topTrigrams <- renderPlot({
    req(nrow(FilteredMetaData()) > 0 & length(FilteredDataSet()) > 0)
    fnPlotTopTrigrams(FilteredMetaData(), 
                      fnFilterTrigrams(FilteredDataSet()$dfAllTrigrams), NumTop())
  })

  # Daily breakdown
  output$topDailyTrigrams <- renderTable({
    req(length(FilteredDataSet()) > 0)
    fnGetTopTrigramsByDay(fnFilterTrigrams(FilteredDataSet()$dfAllTrigrams), 
                          WordsPerWeek())
  })
  
  #######################
  # Comparative Analysis
  #######################
  
  output$plotComparative <- renderPlot({
    req(nrow(FilteredMetaData()) > 1 & length(FilteredDataSet()) > 0)
    fnPlotCompareWordUsage(fnCompareSources(FilteredMetaData(),
                                            FilteredDataSet()$dfAllTerms,
                                            CmpSourceA(), CmpSourceB(), 
                                            CmpAnalyzeMaxWords()))
  })
  
  output$commonToBothSource <- renderTable({
    req(nrow(FilteredMetaData()) > 1 & length(FilteredDataSet()) > 0)
    fnCompareSources(FilteredMetaData(), FilteredDataSet()$dfAllTerms, 
                     CmpSourceA(), CmpSourceB(), CmpAnalyzeMaxWords()) %>%
      filter(!is.na(!!as.symbol(CmpSourceA()))
                    & !is.na(!!as.symbol(CmpSourceB()))) %>%
      head(CmpShowNumWords())
  }, digits=3)

  output$sourceAOnly <- renderTable({
    req(nrow(FilteredMetaData()) > 1 & length(FilteredDataSet()) > 0)
    dfCombined <- fnCompareSources(FilteredMetaData(), FilteredDataSet()$dfAllTerms,
                     CmpSourceA(), CmpSourceB(), CmpAnalyzeMaxWords())
    
    fnGetInOneSource(dfCombined, CmpSourceA(), CmpShowNumWords()) %>%
      head(CmpShowNumWords())
  }, digits=3)

  output$sourceBOnly <- renderTable({
    req(nrow(FilteredMetaData()) > 1 & length(FilteredDataSet()) > 0)
    dfCombined <- fnCompareSources(FilteredMetaData(), FilteredDataSet()$dfAllTerms,
                                   CmpSourceA(), CmpSourceB(), CmpAnalyzeMaxWords())
    
    fnGetInOneSource(dfCombined, CmpSourceB(), CmpShowNumWords()) %>%
      head(CmpShowNumWords())
  }, digits=3)

  #######################
  # Sentiment Analysis
  #######################
  
  output$plotNetSentiments <- renderPlot({
    req(length(FilteredDataSet()) > 0)
    print(min(FilteredDataSet()$dfAllTerms$published))
    fnPlotNetSentimentsByDay(fnGetNetSentimentsByDay(FilteredDataSet()$dfAllTerms, 
                                             SAMethod()))
  })

  # Plot sentiment categories
  output$plotSentiments <- renderPlot({
    req(length(FilteredDataSet()) > 0)
    # Removed positive and negative since that's already covered above
    # Should help accentuate the other emotions
    dfSentiments <- fnGetSentimentsByDay(FilteredDataSet()$dfAllTerms, "nrc") %>% 
      filter(sentiment != "negative" & sentiment != "positive")
    fnPlotSentimentsByDay(dfSentiments)
  })

  output$plotTopSentimentTerms <- renderPlot({
    req(length(FilteredDataSet()) > 0)
    fnPlotWordsBySentiment(FilteredDataSet()$dfAllTerms)
  })
  
  #######################
  # Topic Modeling
  #######################
  
  output$tmTopic1 <- renderPlot({
    req(length(FilteredDataSet()) > 0 & nrow(NewsTopics() > 0))
    fnPlotNewsTopic(NewsTopics(), 1)
  })
  
  output$tmTopic2 <- renderPlot({
    req(length(FilteredDataSet()) > 0 & nrow(NewsTopics() > 1))
    fnPlotNewsTopic(NewsTopics(), 2)
  })
  
  output$tmTopic3 <- renderPlot({
    req(length(FilteredDataSet()) > 0 & nrow(NewsTopics() > 2))
    fnPlotNewsTopic(NewsTopics(), 3)
  })
  
  output$tmTopic4 <- renderPlot({
    req(length(FilteredDataSet()) > 0 & nrow(NewsTopics() > 3))
    fnPlotNewsTopic(NewsTopics(), 4)
  })

  output$tmTopic5 <- renderPlot({
    req(length(FilteredDataSet()) > 0 & nrow(NewsTopics() > 4))
    fnPlotNewsTopic(NewsTopics(), 5)
  })
  
  output$tmTopic6 <- renderPlot({
    req(length(FilteredDataSet()) > 0 & nrow(NewsTopics() > 5))
    fnPlotNewsTopic(NewsTopics(), 6)
  })
  
  output$tmTopic7 <- renderPlot({
    req(length(FilteredDataSet()) > 0 & nrow(NewsTopics() > 6))
    fnPlotNewsTopic(NewsTopics(), 7)
  })
  
  output$tmTopic8 <- renderPlot({
    req(length(FilteredDataSet()) > 0 & nrow(NewsTopics() > 7))
    fnPlotNewsTopic(NewsTopics(), 8)
  })
  
  #######################
  # Semantics
  #######################
  
  output$semTopRoots <- renderPlot(({
    req(nrow(FilteredDataSet()$dfAllRoots) > 0)
    fnPlotTopRoots(FilteredDataSet()$dfAllRoots, TopNumSemantics())
  }))
  
  output$semTopEntities <- renderPlot(({
    req(nrow(FilteredDataSet()$dfAllEntities) > 0)
    fnPlotTopEntities(FilteredDataSet()$dfAllEntities, TopNumSemantics())
  }))
  
  output$semTopPhrases <- renderPlot(({
    req(nrow(FilteredDataSet()$dfAllPhrases) > 0)
    fnPlotTopPhrases(FilteredDataSet()$dfAllPhrases, TopNumSemantics())
  }))
  
  output$semEntityTypeDesc <- renderTable({
    fnGetEntityTypeDescription()
  })
  
  output$semEntitiesByType <- renderPlot(({
    req(nrow(FilteredDataSet()$dfAllEntities) > 0)
    fnPlotEntitiesByType(FilteredDataSet()$dfAllEntities, TopNumSemantics())
  }))
  
  #######################
  # Sources
  #######################
  
  # List all sources (meta data), links and other information
  output$sourceListing <- DT::renderDataTable({
    req(length(FilteredMetaData()) > 0)
    dfSources <- FilteredMetaData()
    dfSources$link <- fnCreateLink(dfSources$sourceURL, dfSources$title)
    DT::datatable(dfSources %>% select(-title, -sourceURL),
                  options = list(pageLength = 25), escape = FALSE)
  }) # Don't "escape" HTML formatting
}