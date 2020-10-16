# Read in datasets created using the 'gather_raw_data.R' script.

responses <- readRDS("responses.rds")

# Define server logic.

server <- function(input, output) {
  
  output$location_dist <- renderPlot({
    
    responses %>%
      mutate(location = as_factor(location)) %>%
      select(id, location) %>%
      ggplot(aes(x = location, fill = location)) +
        geom_bar() +
        theme_bw() +
        labs(
          title = "Survey Respondent Distribution by Location",
          x = "Location for Fall Semester",
          y = "Count"
        )
    
  })
  
  output$gender_dist <- renderPlot({
    
    responses %>%
      mutate(gender = as_factor(gender)) %>%
      select(id, gender) %>%
      ggplot(aes(x = gender, fill = gender)) +
        geom_bar() +
        theme_bw() +
        labs(
          title = "Survey Respondent Distribution by Gender",
          x = "Gender",
          y = "Count"
        )
    
  })
  
}