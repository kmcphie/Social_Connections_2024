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
           fluidPage(
             br(),
             br(),
             fluidRow(column(1), column(10,
              h1(tags$b("Social Connectedness in the Harvard Class of 2024"), 
                          align = "center"),
              p(tags$b("Analyzing how Harvard first-years have been forming 
                        social connections during the COVID-19 pandemic."),
                align = "center"),
              br(),
              includeHTML("project_intro.html"),
              a(tags$b("View more information about our survey methodology here."), 
                href="https://tinyurl.com/sc-2024-survey-methodology"),
              br(),
              br(),
              h3(tags$b("Respondents")),
              br(),
              plotOutput("location_dist"),
              br(),
              plotOutput("gender_dist"),
              br(),
              br()
             ))
            )
  ),
  
  ########## SECOND PAGE ##########
  
  tabPanel("The Social Web",
           fluidPage(
             br(),
             br(),
             fluidRow(column(1), column(10,
              h2(tags$b("Total Sample Size")),
              p(
                "Information about the people who answered our survey and a graph
                showing how they are connected will go on this page."
              ),
              br(),
              br()
            ))
           )
  ),
  
  ########## THIRD PAGE ##########
  
  tabPanel("Analyzing the Data",
           fluidPage(
             br(),
             br(),
             fluidRow(column(1), column(10,
              h2(tags$b("[Insert Title Here]")),
              p(
                "We will analyze our survey results on this page."
              ),
              br(),
              br()
             ))
           )
  ),
  
  ########## FOURTH PAGE ##########
  
  tabPanel("About", 
           fluidPage(
             br(),
             br(),
             fluidRow(column(1), column(10,
              h2(tags$b("About Our Project"),
              align = "center"),
              p(
                "This was a final project for",
                a("Gov 50: Data", href="https://www.davidkane.info/files/gov_50_fall_2020.html"),
                "at Harvard College.",
                align = "center"
              ),
              p(
                "This project's GitHub repository lives",
                a("here", href="https://github.com/kmcphie/Social_Connections_2024"),
                ".",
                align = "center"
              ),
              br(),
              h2(tags$b("About Our Team"),
                 align = "center")
             )),
             fluidRow(column(3), column(9,
              fluidRow(
                column(5, imageOutput("elliott")),
                column(5, offset = 0, 
                       h3(tags$b("Person")), br(), 
                       p("Hi, I'm a placeholder!")),
              ),
              br(),
              fluidRow(
                column(5, imageOutput("ava")),
                column(5, offset = 0, 
                       h3(tags$b("Person")), br(), 
                       p("Hi, I'm a placeholder!")),
              ),
              br(),
              fluidRow(
                column(5, imageOutput("gio")),
                column(5, offset = 0, 
                       h3(tags$b("Person")), br(), 
                       p("Hi, I'm a placeholder!")),
              ),
              br(),
              fluidRow(
                column(5, imageOutput("katherine")),
                column(5, offset = 0, 
                       h3(tags$b("Person")), br(), 
                       p("Hi, I'm a placeholder!")),
              ),
              br(),
              br()
             ))
           )
  )
)
