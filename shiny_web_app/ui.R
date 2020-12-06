########## PREP ##########

# Load necessary libraries.

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(igraph)
library(visNetwork)
library(gt)
library(gtsummary)
library(rstanarm)
library(broom.mixed)
library(ggridges)

# Read in datasets. 
# NB: to preserve anonymity, the original survey responses were read into a 
# private project, which is where cleaning and assigning the random IDs was done. 
# The outputted RDS file was then copied into this project, which is public.

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
  
  ########## FIRST PAGE: ABOUT ##########
  
  tabPanel("About",
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
                              selected = "Location")
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
             #fluidRow(column(1), column(10,
              #h4(tags$b("The Social Web")),
             br(),
             br(),
             p("Of the 1420 members of the class of 2024, a total 393 students 
               responded resulting in a 28% response rate. This network models 
               respondents' connections to their living situation, their 
               participation in pre-orientation programs, and their 
               participation in sports on campus."),
              visNetworkOutput("social_web",  width = "100%", height = "1000px"),
              #br(),
              #br()
            #))
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
            p("When survey respondents were asked to name the person they 
              thought was the most socially connected in the Class of 2024,
              these were the top results. (Note that survey respondents were
              assigned a random ID number when filling out the survey to 
              preserve anonymity.) About 9% of survey respondents chose the
              same individual as the most socially connected. Last year, in
              Jeremiah Kim's final project, 22% of survey respondents chose the
              same individual as the most socially connected, so there is a 
              sharp decrease this year that is perhaps indicative of a more
              fragmented class overall."),
            gt_output("most_connected"),
            br(),
            p("We also wanted to see if the people that survey respondents
              thought were the most connected matched the people that were most
              often listed as close friends by survey respondents. As the graph
              below shows, there is only a very weak negative correlation
              between being voted as the most socially connected and appearing
              in more students' close friend lists, a result not too different 
              from Jeremiah's team's analysis last year. His team concluded
              that the most socially connected are not necessarily close friends
              with the most people, a trend that appears to hold even in a 
              drastically different semester."),
            plotOutput("comparison_graph"),
            br()
            ))
          )
  ),
  
  ########## FOURTH PAGE: ANALYSIS ##########
  
  tabPanel("Analysis",
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
                  by location, interesting trends emerged. Students in the Yard,
                  River Houses, and Quad had similar levels of satisfaction, but
                  students living at home (both international students and 
                  students in the US) were significantly more dissatisfied."),
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
                  this matched their own personal experience. They thought that
                  living in close proximity would be the best way to connect
                  with people, followed by actually being a roommate/suitemate,
                  which is a very similar concept, followed by friend of a
                  friend."),
                plotOutput("best_way"),
                br(),
                p("In contrast, here is a graph of the way that first-years
                  actually met each other. Friend of a friend was actually the
                  top way first-years met, although it only came in third for
                  what people expected. Already knowing someone before coming
                  to Harvard also came in second, surprisingly high. Perhaps
                  this was because first-years had more limited opportunities
                  to meet each other this year and subsequently relied on 
                  previous connections. Third- and fourth-most-common options
                  were living in close proximity and beeing roommates/suitemates
                  just like expected."),
                plotOutput("meet"),
                br(),
                p("Accordingly, in examining the number of times expectations
                  aligned with reality, we graphed the frequency of responses
                  to perceived best ways to form connections and how often they 
                  matched with how students reported meeting one another in 
                  reality. Of those who responded, students in the aggregate 
                  were about 75% accurate in their speculations. Between this
                  finding and those prior, it appears that traditional forms
                  of meeting and sustaining connections held true as the best
                  and most frequent among first-year students, even under 
                  COVID-19 and a hybrid learning and social environment."),
                plotOutput("expectation_matches"),
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
  
  ########## FIFTH PAGE: REGRESSION MODEL ##########
  
  tabPanel("Model",
           fluidPage(
             fluidRow(column(1), column(10,
              br(),
              br(),
              mainPanel(
                h4(tags$b("Satisfaction Regression")),
                p("Our flagship predictive model regressed the variables Group Size, On 
                Campus, and the interaction between the two onto Satisfaction. 
                For context, Group Size indicates the number of virtual 
                relationships that were translated into in-person connections, 
                and On Campus indicates whether or not the first-year student is 
                living on campus during Fall 2020. Satisfaction represents the 
                self-reported level of social satisfaction, with levels 
                \"Very Dissatisfied,\" \"Dissatisfied,\" \"Neutral,\" 
                \"Satisfied,\" and \"Very Satisfied.\" These levels were 
                numerically converted to \"-2,\" \"-1,\" \"0,\" \"1,\" and \"2,\" 
                respectively."),
                br(),
                gt_output("satisfaction_regression"),
                br(),
                p("Included is our output table from our flagship model. The 
                  beta value of our Intercept indicates the predicted value of 
                  satisfaction when \"Group Size\" is equal to zero and the 
                  student is living off-campus with no interaction between the 
                  two predictors. The beta value for group_size reveals the 
                  predicted change in satisfaction for every increase in 1 for 
                  \"Group Size\" while holding \"On Campus\" constant. The beta 
                  value for on_campus represents the predicted change in 
                  satisfaction when \"Group Size\" is held constant and the 
                  student were to switch from living off-campus to on-campus."),
                p("Our interaction term is group_size*on_campus, which we include 
                  because we suspect the value of one predictor will depend on 
                  the value of the other (for instance, we assume Group Size 
                  values are dramatically smaller for students who live off 
                  campus due to lack of opportunity to form an in-person 
                  connection from one that was previously virtual). To interpret 
                  this interaction term, we must do so in the context of the 
                  other predictors. If we add the beta values for the Intercept,
                  group_size, on_campus, and the interaction term, we receive a 
                  value of about 0.11. Essentially, this means that if we 
                  increase a student's group_size by 1 and change campus status 
                  from off to on, we can expect a predicted change of 
                  satisfaction from -0.66 to 0.11. Such a change from a largely 
                  negative to a positive social experience during the hybrid 
                  COVID semester is nothing to sneeze at."),
                br(),
                h4(tags$b("Graphs")),
                p("We have included graphical displays of our flagship model. 
                  The first two represent estimated averages through posterior 
                  distributions on conditional expectations."),
                br(),
                h6(tags$b("Graph 1")),
                plotOutput("graph_1"),
                br(),
                p("In the graph above, we examine the estimated average 
                  satisfaction for on-campus students with Group Sizes 0-10 and 
                  15 and 20. We gather that as Group Size increases, overall 
                  social satisfaction tends to also positively increase."),
                br(),
                h6(tags$b("Graph 2")),
                plotOutput("graph_2"),
                br(),
                p("In our second graph, we examine the predicted average 
                  difference in social satisfaction between on campus students 
                  who have group sizes of 5 and those with group sizes of 0. It 
                  appears there is a median predicted average difference in 
                  satisfaction of roughly 0.27 in favor of those with the larger 
                  group size."),
                br(),
                h6(tags$b("Graph 3")),
                plotOutput("graph_3"),
                br(),
                p("Rather than modeling predicted average distributions after 
                  conditional expectations of student characteristics, graphs 3 
                  through 5 model outputs directly through posterior probability 
                  distributions. Graph 3, using our flagship model, displays the 
                  predicted probability distribution for social satisfaction 
                  between students living on and off campus. There appears to be 
                  a significant difference in predicted satisfactions between 
                  those living on and off campus, with those living off-campus
                  expected to have a decidedly negative social experience 
                  compared to their on-campus counterparts."),
                br(),
                h6(tags$b("Graph 4")),
                plotOutput("graph_4"),
                br(),
                p("Using a new model that specifies between residential location 
                  rather than simply whether a student is on or off campus, we 
                  regressed the location variable from our dataset onto 
                  satisfaction, selecting only to use locations on campus. In 
                  particular, we wanted to analyze differences between those 
                  living in the Yard and those living in the Quad, given that 
                  this year due to dedensified COVID living, a significant 
                  portion of first year students are living in the Quad, which 
                  is characteristically upper-classmen housing. Graph 4 reveals 
                  a higher predicted distribution for satisfaction for those 
                  living in the Yard than those in the Quad, although there is 
                  immense overlap and the difference is not nearly as dramatic 
                  as that for on vs. off campus social satisfaction."),
                br(),
                h6(tags$b("Graph 5")),
                plotOutput("graph_5"),
                br(),
                p("Once again using a similarly new model as the one in Graph 4, 
                  except this time regressing on-campus location on Group Size, 
                  we see a similar disparity in the predicted distributions for 
                  Group Size between Quad and Yard first-years as the 
                  distributions for satisfaction. Yard students tend to have a 
                  higher predicted Group Size, although there is immense 
                  overlap."))
             ))
           )
  ),
  
  ########## SIXTH PAGE: CREATORS ##########
  
  tabPanel("Creators", 
           fluidPage(
             br(),
             br(),
             fluidRow(column(1), column(10,
              h4(tags$b("Acknowledgements & Resources"),
                 align = "center"),
              br(),
              p(
                "This was a final project for",
                a("Gov 50: Data", 
                  href="https://www.davidkane.info/files/gov_50_fall_2020.html"
                ),
                "at Harvard College.",
                align = "center"
              ),
              p("Thanks to Jeremiah Kim and his team, whose",
                a("final project",
                  href="https://jeremiah-kim.shinyapps.io/Social_Connections"
                ),
                "last year was the inspiration for our project this year.",
                align = "center"
              ),
              p(
                "View a list of things we wish we had known before starting this 
                project and our advice to future students",
                a("here.", 
                  href="https://docs.google.com/document/d/17vlDGgshSXExvD02TYnPiRk1ekGbckL6bK43laXSYqE/edit?usp=sharing"
                ),
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
                      br(),
                      br(),
                      h5(tags$b("Katherine McPhie - Project Lead")), br(), 
                      p("Hi, I'm Katherine! I'm a first-year undergraduate at 
                        Harvard pursuing a concentration in Computer Science. On 
                        campus I am involved in Harvard Computer Society, Women 
                        in Computer Science, Women Engineers Code, and Harvard 
                        Open Data Project. I also sing alto in the University 
                        Choir and  play trumpet in the Wind Ensemble! You can 
                        reach me at",
                        a("katherinemcphie@college.harvard.edu.",
                          href="mailto:katherinemcphie@college.harvard.edu?Subject=Social%20Connections%20Project"
                        ))),
              ),
              br(),
              fluidRow(
                column(5, imageOutput("ava")),
                column(5, offset = 0, 
                       h5(tags$b("Ava Swanson")), br(), 
                       p("Hi, I am Ava Swanson! I am a first-year undergraduate at Harvard pursuing a 
                         concentration in Government. I am involved in the John 
                         Adams Society, Women in Business, HCFA, HRC, and the Institute of 
                         Politics. I love baking, enjoy hiking and the great outdoors, and study classical piano! You can reach me at",
                         a("avaswanson@college.harvard.edu.",
                           href="mailto:avaswanson@college.harvard.edu?Subject=Social%20Connections%20Project"
                         ))),
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
                         a("gsalcedo@college.harvard.edu.",
                           href="mailto:gsalcedo@college.harvard.edu?Subject=Social%20Connections%20Project"
                         ))),
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
                         a("edetjen@college.harvard.edu.",
                           href="mailto:edetjen@college.harvard.edu?Subject=Social%20Connections%20Project"
                         ))),
              ),
              br()
             ))
           )
  )
)
