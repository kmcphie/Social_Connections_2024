########## PREP ##########

# Load necessary libraries.

library(shiny)
library(shinythemes)
library(tidyverse)

# Read in datasets created using the 'gather_raw_data.R' script.

responses <- readRDS("responses.rds")

# Define UI.

ui <- navbarPage(
  theme = shinytheme("flatly"),
  tags$b("Social Connectedness in the Class of 2024"),
  
  ########## FIRST PAGE ##########
  
  tabPanel("Overview",
           includeHTML("project_intro.html"),
           includeHTML("survey_questions.html"),
           a(tags$b("View more information about our survey methodology here."), 
             href="https://tinyurl.com/sc-2024-survey-methodology"),
           h3(tags$b("Respondents")),
           plotOutput("location_dist"),
           plotOutput("gender_dist")
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
             "We will analyze our survey results on this page."
           )
  ),
  
  ########## FOURTH PAGE ##########
  
  tabPanel("About", 
           h2(tags$b("About Our Project")),
           p(
             "This was a final project for",
             a("Gov 50: Data", href="https://www.davidkane.info/files/gov_50_fall_2020.html"),
             "at Harvard College."
           ),
           p(
             "This project's GitHub repository lives",
             a("here", href="https://github.com/kmcphie/Social_Connections_2024"),
             "."
           ),
           h2(tags$b("About Our Team")),
           fluidRow(
             column(3, imageOutput("elliott")),
             column(4, offset = 1, 
                    h3(tags$b("Person")), br(), 
                    p("Hi, I'm a placeholder!")),
           ),
           fluidRow(
             column(3, imageOutput("ava")),
             column(4, offset = 1, 
                    h3(tags$b("Person")), br(), 
                    p("Hi, I'm a placeholder!")),
           ),
           fluidRow(
             column(3, imageOutput("gio")),
             column(4, offset = 1, 
                    h3(tags$b("Person")), br(), 
                    p("Hi, I'm a placeholder!")),
           ),
           fluidRow(
             column(3, imageOutput("katherine")),
             column(4, offset = 1, 
                    h3(tags$b("Person")), br(), 
                    p("Hi, I'm a placeholder!")),
           )
  )
)
