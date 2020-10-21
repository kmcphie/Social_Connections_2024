########## TODO ##########

# add bios and headshots to the about page -- everyone
# spend some time pubbing the survey/getting emails -- everyone
# email yao to ask for a list of first years in the class
# email people already in the network -- Elliott
# use pdftools to parse pdf for emails -- Katherine
# check to see if there is a better package to make the cool graph than the one 
# that Jeremiah used last year -- Ava
# include part about degree of separation -- Gio
# network analysis -- lots of fun R packages -- Gio
# https://kieranhealy.org/blog/archives/2013/06/09/using-metadata-to-find-paul-revere/
# figuring out who is central (and it might be interesting to see if this  matches
# who people think are the most connected) -- Gio

########## PREP ##########

# Load necessary libraries.

library(shiny)
library(shinythemes)
library(tidyverse)

# Read in datasets created using the 'gather_raw_data.R' script.

responses <- readRDS("responses.rds")

# Define UI.

ui <- navbarPage(
  theme = shinytheme("yeti"), # other options: sandstone, flatly, united, slate
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
              a(tags$b("Learn more about our survey methodology here."), 
                href="https://tinyurl.com/sc-2024-survey-methodology"),
              br(),
              br(),
              h3(tags$b("Respondents")),
              br(),
              sidebarLayout(
                sidebarPanel(
                  helpText("Create Survey Respondent Distribution Graph"),
                  selectInput("var", 
                              label = "Choose Variable",
                              choices = c("Gap Year", 
                                          "Location", 
                                          "Living",
                                          "Gender", 
                                          "Race", 
                                          "Pre-Orientation Program",
                                          "Sports"),
                              selected = "Location")
                ),
                mainPanel(plotOutput("respondent_dist"))
              ),
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
                "Information about the people who answered our survey and graphs
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
                a("Gov 50: Data", 
                  href="https://www.davidkane.info/files/gov_50_fall_2020.html"
                ),
                "at Harvard College.",
                align = "center"
              ),
              p(
                "This project's GitHub repository lives",
                a("here", 
                  href="https://github.com/kmcphie/Social_Connections_2024"
                ),
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
