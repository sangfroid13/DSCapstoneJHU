#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

suppressPackageStartupMessages(library(shiny))

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Word Prediction App"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("lang", "Language:",
                  c("English" = "en",
                    "German" = "de",
                    "Finnish" = "fi",
                    "Russian" = "ru")),
      textOutput("language")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Type :"),
      textInput("text", "Text",value = ""),
      em("The predicted next word is:"),
      textOutput("predictedWord")
    )
  )
))
