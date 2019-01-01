#setwd("C:\\Users\\ram.p\\Documents\\R\\capstone\\version2")

library(shiny)
library(plotly)

shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Prediction", 
             h2("Ram's Predictive Keys"),
             h4("The language input sofware that predicts your next word"), 
             sidebarLayout(
               sidebarPanel(
                 textInput("sentenceInput", "Enter your phrase: ")
               ),
               mainPanel(
                 h3("Your next word might be..."),
                 plotlyOutput("sentenceOutput")
               )
             )
    ),
    tabPanel("About", 
             h3("Author"),
             p("Ram Kishore Pasupathi"),
             p(a("https://linkedin.com/in/ramkishore97/",href="https://linkedin.com/in/ramkishore97/")),
             h3("Application"),
             p("Data Exploration: ", a("RPubs", href = "http://rpubs.com/prkishore97/cdss10", target="_blank")),
             p("Brief Pitch", a("Slide deck", href = "http://rpubs.com/prkishore97/ramspredictivekeys", target="_blank")),
             p("Master File", a("Github repository", href = "https://github.com/prkishore97/", target="_blank"))
    )
  ),
  tags$script('
              $(document).on("ready", function (e) {
              $("#sentenceInput").focus()
              });
              ') 
  ))
