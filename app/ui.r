################################################################################
# UI Logic
#
# All user interface controls for the app are in this file.
#
################################################################################

library(DT)
library(lubridate)
library(shiny)

ui <- fluidPage(
  # App Title
  titlePanel(
    "R News Digest"
  ),
  sidebarLayout(
    sidebarPanel(width=4,
                 
                 #########################
                 # Common Controls & Filters
                 #########################
                 
                 wellPanel(
                   h3("Common Filters"),
                   selectInput("dataset", label = "Dataset",
                               choices = list("", "small", "large")),
  
                   sliderInput("dateRange", "Time-Frame", value = c(today()-30,
                                                         today()),
                               min = today()-30, max = today()),
  
                   h4("Filter on Sources(s)?"),
                   selectInput("sources", label = "Sources", 
                                choices = list(""), multiple = TRUE)
                 ),
                 
                 #########################
                 # Terms & Concepts
                 #########################
                 
                 wellPanel(
                   h4("Terms & Concepts"),
                   numericInput("numWordsInCloud", "Max in Word Cloud", 20),
                   numericInput("numTop", "Num Top Word & Trigrams:", 20),
                   numericInput("wordsPerWeek", "Top Weekly Terms:", 5, min=2, 
                                max=10)
                 ),
                 
                 #########################
                 # Comparative Analysis
                 #########################
                 
                 wellPanel(
                   h4("Comparative Analysis"),
                   selectInput("cmpSourceA", label = "Source A", 
                               choices = list(""), multiple = FALSE),
                   selectInput("cmpSourceB", label = "Source B", 
                               choices = list(""), multiple = FALSE),
                   
                   numericInput("cmpAnalyzeMaxWords", "Analyze Max Words:", 
                                300),
                   
                   numericInput("cmpShowNumWords", "Show Top:", 30, 
                                min=2, max=100)
                 ),
                
                 #########################
                 # Sentiment Analysis
                 ######################### 
                 wellPanel(
                   h4("Sentiment Analysis"),
                   selectInput("saMethod", label = "Net Sentiment Method",
                               choices = list("afinn", "bing", "loughran"))
                 ),
                 
                 #########################
                 # Topic Modeling
                 #########################
                 
                 wellPanel(
                   h4("Topic Modeling"),
                   selectInput("tmMethod", label = "Method",
                               choices = list("VEM", "Gibbs")),
                   
                   # Number of Topics
                   numericInput("tmNumTopics", "Number of Topics:", 2, min=2,
                                max=8)
                 ),
                 
                 #########################
                 # Semantic Analysis
                 ######################### 
                 wellPanel(
                   h4("Semantic Analysis"),
                   
                   # Number of to words
                  numericInput("semNumTopWords", "NumTopWords", 20, min=3,
                                 max=50)
                 )
                 
    ),
    mainPanel(width=8,
              # Output: Tabset w/ plot, summary, and table ----
              tabsetPanel(type = "tabs",
                          tabPanel("Summary", 
                                   fluidRow(
                                     column(4, align="left", 
                                            h3("Coverage"),
                                            plotOutput("sourceCoverage"),
                                            plotOutput("sourceContribution")
                                     ),
                                     column(8, align="center",
                                            h3("Summary Stats"),
                                            tableOutput("sourceSummaryStats")
                                     )
                                   ),
                                   fluidRow(
                                     column(12, align="center",
                                            h3("Over Time"),
                                            plotOutput("sourceDailyCoverage")
                                     )
                                   )
                          ),
                          tabPanel("Terms & Concepts", 
                                   fluidRow(
                                     column(4, align="center",
                                            h3("Word Cloud"),
                                            plotOutput("topWordsCloud")
                                     ),
                                     column(4, align="center",
                                            h3("Top Words"),
                                            plotOutput("topWords")
                                     ),
                                     column(4, align="center",
                                            h3("Top Trigrams"),
                                            plotOutput("topTrigrams")
                                     )
                                   ),
                                   fluidRow(
                                     column(12, align="center",
                                            h3("Over Time"),
                                            tableOutput("topDailyTrigrams")
                                     )
                                   ) 
                          ),
                          tabPanel("Comparative Analysis", 
                                   fluidRow(
                                     column(12, align="center",
                                            h3("Comparative Word Usage Plot"),
                                            plotOutput("plotComparative")
                                     )
                                   ),
                                   fluidRow(
                                     column(4, align="center",
                                            h3("Common to Both"),
                                            tableOutput("commonToBothSource")
                                     ),
                                     column(4, align="center",
                                            h3("Source A Only"),
                                            tableOutput("sourceAOnly")
                                     ),
                                     column(4, align="center",
                                            h3("Source B Only"),
                                            tableOutput("sourceBOnly")
                                     )
                                   )
                          ),
                          tabPanel("Sentiment Analysis",
                                   fluidRow(
                                     column(6, align="center",
                                            h3("Positive vs Negative Sentiment"),
                                            plotOutput("plotNetSentiments")
                                     ),
                                     column(6, align="center",
                                            h3("Categories of Sentiments"),
                                            plotOutput("plotSentiments")
                                     )
                                   ),
                                   fluidRow(
                                     column(12, align="center",
                                            h3("Top Terms by Sentiment"),
                                            plotOutput("plotTopSentimentTerms")
                                     )
                                   )
                          ),
                          tabPanel("Topic Modeling", 
                                   fluidRow(
                                     column(3, align="center",
                                            h4("Topic 1"),
                                            plotOutput("tmTopic1")
                                     ),
                                     column(3, align="center",
                                            h4("Topic 2"),
                                            plotOutput("tmTopic2")
                                     ),
                                     column(3, align="center",
                                            h4("Topic 3"),
                                            plotOutput("tmTopic3")
                                     ),
                                     column(3, align="center",
                                            h4("Topic 4"),
                                            plotOutput("tmTopic4")
                                     )
                                   ),
                                   fluidRow(
                                     column(3, align="center",
                                            h4("Topic 5"),
                                            plotOutput("tmTopic5")
                                     ),
                                     column(3, align="center",
                                            h4("Topic 6"),
                                            plotOutput("tmTopic6")
                                     ),
                                     column(3, align="center",
                                            h4("Topic 7"),
                                            plotOutput("tmTopic7")
                                     ),
                                     column(3, align="center",
                                            h4("Topic 8"),
                                            plotOutput("tmTopic8")
                                     )
                                   )
                          ),
                          tabPanel("Semantics (SpacyR)", 
                               fluidRow(
                                 column(4, align="center",
                                        h4("Top Roots"),
                                        plotOutput("semTopRoots")
                                 ),
                                 column(4, align="center",
                                        h4("Top Entities"),
                                        plotOutput("semTopEntities")
                                 ),
                                 column(4, align="center",
                                        h4("Top Phrases"),
                                        plotOutput("semTopPhrases")
                                 )
                               ),
                               fluidRow(
                                 column(12, align="center",
                                        h4("Entities by Type"),
                                        plotOutput("semEntitiesByType")
                                 )
                               ),
                               fluidRow(
                                 column(6, align="center",
                                        h4("Entity Type & Description"),
                                        tableOutput("semEntityTypeDesc")
                                 ),
                               )
                          ),
                          tabPanel("Source", 
                              h3("Articles in Dataset"),
                              dataTableOutput("sourceListing")  
                          )
              ) # Tabset        
    ) # Main Panel
  ) # App layout
) # UI widget