# Read in datasets created using the 'gather_raw_data.R' script.

responses <- readRDS("responses_test.rds")

# Define server logic.

server <- function(input, output) {
  
  ########## FIRST PAGE: OVERVIEW ##########
  
  output$respondent_dist <- renderPlot({
    data <- switch(input$var,
                   "Gap Year" = responses %>%
                                  mutate(gap_year = as_factor(gap_year)) %>%
                                  select(id, gap_year),
                   "Location" = responses %>%
                                  mutate(location = as_factor(location)) %>%
                                  select(id, location),
                   "Living" = responses %>%
                                mutate(living = as_factor(living)) %>%
                                select(id, living),
                   "Gender" = responses %>%
                                mutate(location = as_factor(location)) %>%
                                select(id, location),
                   "Race" = responses %>%
                              mutate(race = as_factor(race)) %>%
                              select(id, race),
                   "Pre-Orientation Program" = responses %>%
                                                 mutate(pre_o = as_factor(pre_o)) %>%
                                                 select(id, pre_o),
                   "Sports" = responses %>%
                                mutate(sports = as_factor(sports)) %>%
                                select(id, sports))
    variable <- switch(input$var,
                       "Gap Year" = responses$gap_year,
                       "Location" = responses$location,
                       "Living" = responses$living,
                       "Gender" = responses$gender,
                       "Race" = responses$race,
                       "Pre-Orientation Program" = responses$pre_o,
                       "Sports" = responses$sports)
    title <- switch(input$var,
                    "Gap Year" = "Survey Respondent Distribution by Gap Year",
                    "Location" = "Survey Respondent Distribution by Location",
                    "Living" = "Survey Respondent Distribution by Living Situation",
                    "Gender" = "Survey Respondent Distribution by Gender",
                    "Race" = "Survey Respondent Distribution by Race",
                    "Pre-Orientation Program" = "Survey Respondent Distribution by Pre-Orientation Program",
                    "Sports" = "Survey Respondent Distribution by Sports Involvement")
    x <- switch(input$var,
                "Gap Year" = "Took a gap year",
                "Location" = "Location for the Fall Semester",
                "Living Situation (if on campus)",
                "Gender" = "Gender",
                "Race" = "Race",
                "Pre-Orientation Program" = "Pre-Orientation Program",
                "Sports" = "Involved in Sports")
    data %>%
      ggplot(aes(x = variable)) + #TODO: figure out how to put this in pct format
        geom_bar(fill = "#6fb4d2") +
        theme_bw() +
        theme(legend.position = "none") +
        labs(
          title = title,
          x = x,
          y = "Count"
        ) +
        theme(title = element_text(size = 14, face = "bold"),
              axis.title.x = element_text(size = 12, face = "plain"),
              axis.title.y = element_text(size = 12, face= "plain"))
        #scale_y_continuous(labels = scales::percent_format())
  })
  
  ########## SECOND PAGE: SOCIAL WEB ##########
  
  ########## THIRD PAGE: ANALYSIS ##########
  
  output$overall_satisfaction <- renderPlot({
    responses %>%
      mutate(satisfaction = as_factor(satisfaction)) %>%
      filter(satisfaction != "NA") %>%
      group_by(satisfaction) %>%
      summarize(count = n(), .groups = "drop") %>%
      ggplot(aes(x = fct_relevel(satisfaction, 
                                 levels = c("Very Dissatisfied",
                                            "Dissatisfied",
                                            "Neutral",
                                            "Satisfied",
                                            "Very Satisfied")), 
                 y = count / sum(count))) +
      geom_col(fill = "#6fb4d2") +
      theme_bw() +
      theme(legend.position = "none") +
      labs(
        title = "Overall Satisfaction with Social Connections Among Harvard First-Years",
        x = "Self-Reported Level of Satisfaction with Social Connections",
        y = "Percent"
      ) +
      theme(title = element_text(size = 14, face = "bold"),
            axis.title.x = element_text(size = 12, face = "plain"),
            axis.title.y = element_text(size = 12, face= "plain")) +
      scale_y_continuous(labels = scales::percent_format())
  })
  
  output$satisfaction_by_location <- renderPlot({
    responses %>%
      mutate(satisfaction = as_factor(satisfaction)) %>%
      filter(satisfaction != "NA") %>%
      select(satisfaction, location) %>%
      filter(location %in% c("The Yard", 
                             "A River House", 
                             "At Home",
                             "The Quad")) %>%
      group_by(satisfaction, location) %>%
      summarize(count = n(), .groups = "drop") %>%
      ggplot(aes(x = fct_relevel(satisfaction,
                                 levels = c("Very Dissatisfied",
                                            "Dissatisfied",
                                            "Neutral",
                                            "Satisfied",
                                            "Very Satisfied")), 
                 y = count / sum(count))) +
      geom_col() +
      facet_wrap(~location) +
      geom_col(fill = "#6fb4d2") +
      theme_bw() +
      labs(title = "Overall Satisfaction with Social Connections \n Among Harvard First-Years by Location",
           x = "Self-Reported Level of Satisfaction with Social Connections",
           y = "Percent") +
      theme(title = element_text(size = 14, face = "bold"),
            axis.title.x = element_text(size = 12, face = "plain"),
            axis.title.y = element_text(size = 12, face= "plain")) +
      scale_x_discrete(labels = c("Very \n Dissatisfied",
                                  "Dissatisfied",
                                  "Neutral",
                                  "Satisfied",
                                  "Very \n Satisfied")) +
      scale_y_continuous(labels = scales::percent_format())
  })
  
  ########## FOURTH PAGE: ABOUT ##########
  
  output$katherine <- renderImage({
    list(src = "www/katherine.png", 
         width = 300, 
         height = 400)
  }, deleteFile = FALSE)
  
  output$elliott <- renderImage({
    list(src = "www/elliott.png",
         width = 300, 
         height = 310)
  }, deleteFile = FALSE)
  
  output$ava <- renderImage({
    list(src = "www/ava.png",
         width = 300, 
         height = 400)
  }, deleteFile = FALSE)
  
  output$gio <- renderImage({
    list(src = "www/gio.png",
         width = 300, 
         height = 400)
  }, deleteFile = FALSE)
  
}