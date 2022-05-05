################################################################################
# R Shiny News Digest App
#
# The purpose of this application is to allow users to explore a pre-compiled
# Corpus of news articles, analyze and visualize characteristics such as
# common terms, trigrams, and see trends over time. The app also demonstrates
# topic modeling, semantics extraction and provides a way to access the sources.
#
################################################################################
library(shiny)

source("ui.r", local = TRUE)
source("server.R", local = TRUE)

shinyApp(ui, server)
