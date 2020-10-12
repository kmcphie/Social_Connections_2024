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
             "How do we become friends with the people we are friends with? Are 
             the friends we make truly representative of our interests or are
             they actually determined by uncontrollable factors like the dorms
             we live in, our extracurriculars, our race, and where we come from?
             In seeking to answer this question and others like it, we mapped
             and analyzed the literal social network of the Harvard Class of
             2024.",
             "We were interested to see how Harvard first-years formed social 
             connections during this unprecedented year in particular, looking
             at the means by which they formed connections and stayed connected
             with each other, the literal web of social connections in the Class
             of 2024, comparing the average level of connectedness to the
             average level of connectedness from last year, and so on.",
           ),
           h3(tags$b("Collecting Data")),
           p(
             "We sent out a survey to the Class of 2024 to gather the data for 
             our project. We publicized our survey over emails scraped from the
             Harvard facebook, Slack, and our own personal social media 
             accounts. So far, we are still working to get as many people to 
             respond as possible, so any results shown are incomplete as of 
             yet."
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
             "We will analyze our survey results on this page."
           )
  ),
  
  ########## FOURTH PAGE ##########
  
  tabPanel("Creators", 
           h2(tags$b("About Our Team")),
           p(
             "Bios, pictures, and contact information for of each of our team 
             members will go on this page."
           ),
           p(
             "This project's GitHub repository lives",
             a("here", href="https://github.com/kmcphie/Social_Connections_2024"),
             "."
           )
  )
)
