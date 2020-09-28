########## PREP ##########

# Load necessary libraries.

library(shiny)
library(shinythemes)

# Define UI. 

ui <- navbarPage(
    theme = shinytheme("yeti"),
    tags$b("Social Connectedness in the Class of 2024"),
    
    ########## FIRST PAGE ##########
    
    tabPanel("The Survey",
             h2(tags$b("Purpose of Our Research")),
             p(
                 "[We'll discuss why we want to do this project here]"
             ),
             h3(tags$b("Draft of Survey Questions")),
             p(
                 "Have you taken a gap year?
Where are you living this semester?
The Yard
The Quad
The River Houses
Union Dorm
Home (international student)
Home (in the US)
If you are living on campus, are you in a single or do you have suitemates?
Single
Suitemates
Which gender identity do you most identify with?
Specify your race and/or ethnicity
Which pre-orientation, if any, did you participate in?
Are you involved with any sports teams on campus?
 List the four names of the people who you are closest friends with.
Zoom lectures
Breakout room of a class
Entryway
Pre-orientation program
Club/extracurricular
Donut/other randomized Slack bot
Social media
GroupMe
Instagram
Discord
In-person (outside in courtyard or something)
The courtyard
The line of the dining hall
The line to get tested for COVID-19
Meet24
How do you stay in contact with them? (multiple choice)
Same options as above to pick from
How many people have you met online that you now often meet up with in person?
In your experience this year, what has been the best option for forming meaningful connections? 
Zoom lectures
Breakout room of a class
Entryway
Pre-orientation program
Club/extracurricular
Donut/other randomized Slack bot
Social media
GroupMe
Instagram
Discord
In-person (outside in courtyard or something)
The courtyard
The line of the dining hall
The line to get tested for COVID-19
Meet24
 When you do meet with people in-person, where do you go?
The courtyard
A restaurant in the square/the general area
Name the most socially connected person in the class of 2024.
How satisfied are you with your social connections with first-year students at Harvard?
Very Satisfied
Satisfied
Neutral
Dissatisfied
Very Dissatisfied"
             ),
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
             )
    )
)


# Define server logic.

server <- function(input, output) {
    output$line_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        
        ifelse(
            input$plot_type == "a",
            
            # If input$plot_type is "a", plot histogram of "waiting" column 
            # from the faithful dataframe
            
            x   <- faithful[, 2],
            
            # If input$plot_type is "b", plot histogram of "eruptions" column
            # from the faithful dataframe
            
            x   <- faithful[, 1]
        )
        
        # Draw the histogram with the specified number of bins
        
        hist(x, col = 'darkgray', border = 'white')
    })
}

# Run the application.

shinyApp(ui = ui, server = server)