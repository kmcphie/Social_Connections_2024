########## PREP ##########

# Load necessary libraries.

library(readxl)

# Load in the data.

responses <- read_excel("shiny_app/raw_data/survey_responses.xlsx")

write_rds(responses, "shiny_app/responses.rds")
