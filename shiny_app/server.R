# Read in datasets created using the 'gather_raw_data.R' script.

responses <- readRDS("responses.rds")

# Define server logic.

server <- function(input, output) {
  
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
                    "Living" = 
                      "Survey Respondent Distribution by Living Situation",
                    "Gender" = "Survey Respondent Distribution by Gender",
                    "Race" = "Survey Respondent Distribution by Race",
                    "Pre-Orientation Program" = 
                      "Survey Respondent Distribution by Pre-Orientation Program",
                    "Sports" = 
                      "Survey Respondent Distribution by Sports Involvement")
    x <- switch(input$var,
                "Gap Year" = "Took a gap year",
                "Location" = "Location for the Fall Semester",
                "Living Situation (if on campus)",
                "Gender" = "Gender",
                "Race" = "Race",
                "Pre-Orientation Program" = "Pre-Orientation Program",
                "Sports" = "Involved in Sports")
    legend <- switch(input$var,
                "Gap Year" = "Gap Year",
                "Location" = "Location",
                "Living Situation",
                "Gender" = "Gender",
                "Race" = "Race",
                "Pre-Orientation Program" = "Pre-Orientation Program",
                "Sports" = "Sports")
    data %>%
      ggplot(aes(x = variable, fill = variable)) +
        geom_bar() +
        theme_bw() +
        # theme(title = element_text(family = "Avenir"),
        #       text = element_text(family = "Avenir"))
        theme(legend.position = "top") +
        labs(
          title = title,
          x = x,
          y = "Count"
        )
  })
  
  output$elliott <- renderImage({
    list(src = "person.png", 
         width = 300, 
         height = 400)
  }, deleteFile = FALSE)
  
  output$ava <- renderImage({
    list(src = "person.png", 
         width = 300, 
         height = 400)
  }, deleteFile = FALSE)
  
  output$gio <- renderImage({
    list(src = "person.png", 
         width = 300, 
         height = 400)
  }, deleteFile = FALSE)
  
  output$katherine <- renderImage({
    list(src = "person.png", 
         width = 300, 
         height = 400)
  }, deleteFile = FALSE)
  
}