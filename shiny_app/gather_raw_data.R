########## PREP ##########

# Load necessary libraries.

library(readxl)

# Load in the data.

responses_1 <- read_excel("shiny_app/raw_data/responses_1.xlsx")

# Clean the data and add an ID column.

responses_clean <- responses_1 %>%
  rename(gap_year = "1. Have you taken a gap year?",
         location = "2. Where are you living this semester?",
         living = "3. If you are living on campus, are you in a single or do you have suitemates?",
         gender  = "4. Which gender identity do you most identify with?",
         race = "5. Specify your race and/or ethnicity.",
         pre_o = "6. Which pre-orientation program, if any, did you participate in?",
         sports = "7. Are you involved with any sports teams on campus?",
         best = "8. In your opinion, which of the following is best for forming meaningful connections (even if this doesn’t match your own experience)?",
         p1_name = "Name of the first person (first and last)",
         p1_meet = "Where/how did you first meet this person?...10",
         p1_closeness = "How close do you feel to this person on a scale of 1 to 5?...11",
         p2_name = "Name of the second person (first and last)",
         p2_meet = "Where/how did you first meet this person?...13",
         p2_closeness = "How close do you feel to this person on a scale of 1 to 5?...14",
         p3_name = "Name of the third person (first and last)",
         p3_meet = "Where/how did you first meet this person?...16",
         p3_closeness = "How close do you feel to this person on a scale of 1 to 5?...17",
         p4_name = "Name of the fourth person (first and last)",
         p4_meet = "Where/how did you first meet this person?...19",
         p4_closeness = "How close do you feel to this person on a scale of 1 to 5?...20",
         contact = "10. How do you stay in contact with these four people? (If multiple options apply, please pick the primary one.)",
         group_size = "11. How many people have you met virtually that you now often meet up with in-person? (Please enter a single number)",
         in_person = "12. When you do meet with people in-person, where do you go or what do you do? (Please remember that this survey is completely anonymous and your answers will not be used to get you in trouble.)",
         most_connected = "13. Name the person in the Class of 2024 who you think is the most socially connected. (First and last name)",
         satisfaction = "14. How satisfied are you with your social connections with other first-year students at Harvard?",
         name = "First and last name (this won't be linked to your responses ever, we just need to assign you a random ID)") %>%
  mutate(id = 1:nrow(responses_1), .before = 1)

# Read the cleaned data into a new RDS document.

write_rds(responses_clean, "shiny_app/responses_test.rds")
