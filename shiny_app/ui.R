########## PREP ##########

# Load necessary libraries.

library(shiny)
library(shinythemes)

# Define UI.

ui <- navbarPage(
  theme = shinytheme("flatly"),
  tags$b("Social Connectedness in the Class of 2024"),
  
  ########## FIRST PAGE ##########
  
  tabPanel("The Survey",
           h2(tags$b("Purpose of Our Research")),
           p(
             "[We'll discuss why we want to do this project here]"
           ),
           h3(tags$b("Survey Questions")),
           includeHTML("survey_questions.html"),
           a("View our survey methodology.", href="https://tinyurl.com/sc-2024-survey-methodology"),
           h3(tags$b("Unrelated Placeholder Graph")),
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "plot_type",
                   "Plot Type",
                   c("Option A" = "a", "Option B" = "b")
                 )),
               mainPanel(plotOutput("line_plot")))
           )
  ),
  
  ########## SECOND PAGE ##########
  
  tabPanel("The Social Web",
           h2(tags$b("Total Sample Size")),
           p(
             "Information about the people we were able to get to answer our 
            survey, and a graph showing how they are connected"
           )
  ),
  
  ########## THIRD PAGE ##########
  
  tabPanel("Analyzing the Data",
           h2(tags$b("[Insert Title Here]")),
           p(
             "Depending on how we organize our survey, we will analyze our 
                 results in different ways here."
           )
  ),
  
  ########## FOURTH PAGE ##########
  
  tabPanel("Comment Analysis",
           h2(tags$b("Word Cloud Taken from Survey Comments")),
           p(
             "Here we'll have a graph showing common words taken from the 
                 comments. We will analyze overall common sentiments from the survey 
                 and summarize the results here."
           )
  ),
  
  ########## FIFTH PAGE ##########
  
  tabPanel("Creators", 
           h2(tags$b("About Our Team")),
           p(
             "We'll have bios and pictures of each of our team members here 
                on this page, along with contact information and a link to the 
                GitHub repo."
           ),
           p(
             "This project's GitHub repository lives",
             a("here", href="https://github.com/kmcphie/Social_Connections_2024"),
             "."
           )
  )
)