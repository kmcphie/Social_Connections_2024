########## PREP ##########

# Load necessary libraries.

library(shiny)
library(shinythemes)

# Define UI.

ui <- navbarPage(
  theme = shinytheme("flatly"),
  tags$b("Social Connectedness in the Class of 2024"),
  
  ########## FIRST PAGE ##########
  
  tabPanel("Overview",
           h3(tags$b("Purpose of Our Research")),
           p(
             "We'll discuss why we want to do this project here."
           ),
           h3(tags$b("Collecting Data")),
           p(
             "We sent out a survey to the Class of 2024 to gather the data for 
             our project. More information about the survey and who responded
             will go here."
           ),
           h3(tags$b("Survey Questions")),
           includeHTML("survey_questions.html"),
           a(tags$b("View more information about our survey methodology here."), 
             href="https://tinyurl.com/sc-2024-survey-methodology"),
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
             "Information about the people who answered our survey and a graph
             showing how they are connected will go on this page."
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
  
  tabPanel("Creators", 
           h2(tags$b("About Our Team")),
           p(
             "Bios, pictures, and contact information for of each of our team 
             members will go here on this page."
           ),
           p(
             "This project's GitHub repository lives",
             a("here", href="https://github.com/kmcphie/Social_Connections_2024"),
             "."
           )
  )
)