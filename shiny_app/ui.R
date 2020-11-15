########## ########## PREP ##########

# Load necessary libraries.

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(igraph)
library(visNetwork)

# Read in datasets created using the 'gather_raw_data.R' script.

responses <- readRDS("responses_final.rds")

# Define UI.

ui <- navbarPage(
  #title = img(src="<LOGO>", height = "40px"), 
  id = "navBar",
  theme = "paper.css",
  #theme = shinytheme("yeti"), # other options: sandstone, flatly, united, slate
  collapsible = TRUE,
  inverse = TRUE,
  windowTitle = "Social Connections 2024",
  position = "fixed-top",
  footer = includeHTML("./www/footer.html"),
  header = tags$style(
    ".navbar-right {float: right !important;}",
    "body {padding-top: 75px;}"),
  tags$b("Social Connectedness in the Class of 2024"),
  
  ########## FIRST PAGE: OVERVIEW ##########
  
  tabPanel("Overview",
           fluidPage(
             fluidRow(column(1), column(10,
              # h3(tags$b("Social Connectedness in the Harvard Class of 2024"), 
              #             align = "center"),
              # h6(tags$b("Analyzing how Harvard first-years have been forming 
              #           social connections during the COVID-19 pandemic."),
              #   align = "center"),
              HTML("<section class='banner'>
                    <h2 class='sc'>Social Connectedness in the Harvard
                    Class of 2024</h2>
                    <p class='sc_description'>Analyzing how Harvard
                    first-years have been forming social connections during the
                    COVID-19 pandemic.</p>
                    </section>"),
              br(),
              includeHTML("project_intro.html"),
              br(),
              sidebarLayout(
                sidebarPanel(
                  #helpText("Create Survey Respondent Distribution Graph"),
                  selectInput("var", 
                              label = "Choose Variable",
                              choices = c("Gap Year", 
                                          "Location", 
                                          "Living",
                                          "Gender", 
                                          "Race", 
                                          "Pre-Orientation Program",
                                          "Sports"),
                              selected = "Living")
                ),
                mainPanel(plotOutput("respondent_dist"))
              ),
              br(),
              br()
             ))
            )
  ),
  
  ########## SECOND PAGE: SOCIAL WEB ##########
  
  tabPanel("The Social Web",
           fluidPage(
             br(),
             br(),
             fluidRow(column(1), column(10,
              h4(tags$b("The Social Web")),
              visNetworkOutput("social_web",  width = "100%", height = "1000px"),
              br(),
              br()
            ))
           )
  ),
  
  ########## THIRD PAGE: MOST CONNECTED ##########
  
  tabPanel("Most Connected",
           fluidPage(
           br(),
           br(),
           fluidRow(column(1), column(10,
            h4(tags$b("The Social Network")),
            plotOutput("static_network"),
            br(),
            br()
            ))
          )
  ),
  
  ########## FOURTH PAGE: ANALYSIS ##########
  
  tabPanel("Analyzing the Data",
           fluidPage(
             fluidRow(column(1), column(10,
              br(),
              br(),
              mainPanel(
                h4(tags$b("Satisfaction with Social Connections")),
                p("Survey respondents were asked to rate their overall level of 
                satisfaction with their social connections with other 
                first-year students. Each respondent was given 5 options: Very 
                Dissatisfied, Dissatisfied, Neutral, Satisfied, or Very 
                Satisfied. If we assign each option to a number, with -2
                corresponding to 'Very Dissatisfied' and 2 corresponding to
                'Very Satisfied', the average satisfaction across all survey
                respondents is 0.17. The graph below shows the distribution of 
                responses. "),
                plotOutput("overall_satisfaction"),
                br(),
                p("When looking at how survey respondents' satisfaction differed
                  by location, interesting trends emerged. Students in the Yard
                  reported higher levels of satisfaction overall, followed by
                  students in the River Houses, then students in the Quad, and 
                  then students living at home (both international students and 
                  students in the US)."),
                plotOutput("satisfaction_by_location"),
                br(),
                p("We were curious to see what the overall satisfaction levels
                  would be for students who took gap years. For people accepted
                  to Class of 2024 who had just graduated high school, there
                  was the option to take a gap year in hopes of escaping a 
                  strange school year and having a more typical freshman year --
                  about 20% opted to do so, in fact. Members of the Class of 
                  2024 who had just gotten off of a gap year, on the other hand, 
                  didn't have the option to take another gap year, so we 
                  wondered if this would correspond to lower levels of 
                  satisfaction overall. However, the data actually showed that 
                  there is roughly the same distribution of satisfaction for 
                  those who took gap years as for the class in general, only
                  slightly more skewed in the 'Dissatisfied' direction."),
                plotOutput("gap_year_satisfaction"),
                br(),
                p("We were also curious to see if of the people on campus, the
                  people with suitemates would be more satisfied on average
                  than people placed in singles. However, here as well, the 
                  trends between the two seemed to be pretty similar."),
                plotOutput("satisfaction_by_living"),
                br(),
                h4(tags$b("Best Way to Make Connections")),
                p("We asked survey respondents what they thought the best way to
                  form connections with people in general was, regardless of if
                  this matched their own personal experience."),
                plotOutput("best_way"),
                br(),
                p("In contrast, here is a graph of the way that first-years
                  actually met each other."),
                plotOutput("meet"),
                br(),
                p("Here are the most common ways first-years stay in contact 
                  with each other."),
                plotOutput("stay_in_contact"),
                br(),
                p("And when first-years do hang out in-person, here's what they
                  do."),
                plotOutput("in_person"),
                br(),
                p("And other random info to incorporate somewhere:
                  Average group size is 4.18299
                  Average closeness is 3.508906 (on 1 - 5 scale)"))
             ))
           )
  ),
  
  ########## FOURTH PAGE: ABOUT ##########
  
  tabPanel("About", 
           fluidPage(
             br(),
             br(),
             fluidRow(column(1), column(10,
              h4(tags$b("About Our Project"),
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
              h4(tags$b("About Our Team"),
                 align = "center")
             )),
             fluidRow(column(3), column(9,
              fluidRow(
                column(5, imageOutput("katherine")),
                column(5, offset = 0, 
                      h5(tags$b("Katherine McPhie")), br(), 
                      p("Hi, I'm Katherine! I'm a first-year undergraduate at 
                        Harvard pursuing a concentration in Computer Science. On 
                        campus I am involved in Harvard Computer Society, Women 
                        in Computer Science, Women Engineers Code, and Harvard 
                        Open Data Project. I also sing alto for University Choir
                        and  play trumpet in the Wind Ensemble! You can reach me 
                        at",
                        a("katherinemcphie@college.harvard.edu",
                          href="mailto:katherinemcphie@college.harvard.edu?Subject=Social%20Connections%20Project"
                        ),
                        ".")),
              ),
              br(),
              fluidRow(
                column(5, imageOutput("elliott")),
                column(5, offset = 0, 
                       h5(tags$b("Elliott Detjen")), br(), 
                       p("I am a first-year undergraduate at Harvard pursuing a 
                         concentration in Economics with a secondary in 
                         Government. On campus, I write for the Harvard 
                         Political and Economics Reviews, participate in the 
                         John Adams Society, and preside as the chairman of the 
                         Harvard Junto. You can reach me at",
                         a("edetjen@college.harvard.edu",
                           href="mailto:edetjen@college.harvard.edu?Subject=Social%20Connections%20Project"
                         ),
                         ".")),
              ),
              br(),
              fluidRow(
                column(5, imageOutput("ava")),
                column(5, offset = 0, 
                       h5(tags$b("Ava Swanson")), br(), 
                       p("Hi, I am Ava Swanson! I am a first-year undergraduate at Harvard pursuing a 
                         concentration in Government. I am involved in the John 
                         Adams Society, Women in Business, and the Institute of 
                         Politics. You can reach me at",
                         a("avaswanson@college.harvard.edu",
                           href="mailto:avaswanson@college.harvard.edu?Subject=Social%20Connections%20Project"
                         ),
                         ".")),
              ),
              br(),
              fluidRow(
                column(5, imageOutput("gio")),
                column(5, offset = 0, 
                       h5(tags$b("Giovanni Salcedo")), br(), 
                       p("Hi! My name is Giovanni Salcedo. I’m a sophomore at 
                         Harvard College planning to concentrate in Government 
                         on the Data Science Track. I hope to use my data 
                         science skills on public policy and social issues in 
                         the future. I love gaming, PC building, and amateur 
                         photography. You can reach me at",
                         a("gsalcedo@college.harvard.edu",
                           href="mailto:gsalcedo@college.harvard.edu?Subject=Social%20Connections%20Project"
                         ),
                         ".")),
              ),
              br(),
              br()
             ))
           )
  )
)
