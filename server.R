#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

suppressPackageStartupMessages(library(shiny))
library(stringr)
library(dplyr)

source("predictWordGit.R")
bigramFile<-readRDS("data/bigramFile.RDS")
trigramFile<-readRDS("data/trigramFile.RDS")
quadgramFile<-readRDS("data/quadgramFile.RDS")
pentagramFile<-readRDS("data/pentagramFile.RDS")

shinyServer(function(input, output) {
  langSpecify <- reactive({
    
    if_else(input$lang=='en',"You chose English","Chosen language is not supported")
  })
  output$language <- renderPrint(langSpecify())
  
  predicted <- reactive({
    
    predictNextWord(input$text)
    
  })
  output$predictedWord <- renderText({ as.character(predicted()) })
  
  
  
})
