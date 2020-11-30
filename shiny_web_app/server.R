# Read in datasets. 
# NB: to preserve anonymity, the original survey responses were read into a 
# private project, which is where cleaning and assigning the random IDs was done. 
# The outputted RDS file was then copied into this project, which is public.

responses <- readRDS("responses_final.rds")
expectations <- readRDS("expectations.rds")

# Define server logic.

server <- function(input, output) {
  
  ########## FIRST PAGE: OVERVIEW ##########
  
  output$respondent_dist <- renderPlot({
    data <- switch(input$var,
                   "Gap Year" = responses %>%
                     mutate(gap_year = as_factor(gap_year)) %>%
                     select(id, gap_year) %>%
                     mutate(count = n()),
                   "Location" = responses %>%
                     mutate(location = as_factor(location)) %>%
                     select(id, location) %>%
                     mutate(count = n()),
                   "Living" = responses %>%
                     mutate(living = as_factor(living)) %>%
                     select(id, living) %>%
                     mutate(count = n()),
                   "Gender" = responses %>%
                     mutate(location = as_factor(location)) %>%
                     select(id, location) %>%
                     mutate(count = n()),
                   "Race" = responses %>%
                     mutate(race = as_factor(race)) %>%
                     select(id, race) %>%
                     mutate(count = n()),
                   "Pre-Orientation Program" = responses %>%
                     mutate(pre_o = as_factor(pre_o)) %>%
                     select(id, pre_o) %>%
                     mutate(count = n()),
                   "Sports" = responses %>%
                     mutate(sports = as_factor(sports)) %>%
                     select(id, sports)) %>%
      mutate(count = n())
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
                    "Pre-Orientation Program" = "Survey Respondent Distribution by \nPre-Orientation Program",
                    "Sports" = "Survey Respondent Distribution by Sports Involvement")
    # x <- switch(input$var,
    #             "Gap Year" = "Took a gap year",
    #             "Location" = "Location for the Fall Semester",
    #             "Living Situation (if on campus)",
    #             "Gender" = "Gender",
    #             "Race" = "Race",
    #             "Pre-Orientation Program" = "Pre-Orientation Program",
    #             "Sports" = "Involved in Sports")
    data %>%
      ggplot(aes(x = fct_infreq(variable))) + 
      geom_bar(aes(y = after_stat(count / sum(count))),
               fill = "#6fb4d2") +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none") +
      labs(
        title = title,
        x = NULL, # x
        y = "Percent" # "Count"
      ) +
      theme(title = element_text(size = 14, face = "bold"),
            axis.title.x = element_text(size = 12, face = "plain"),
            axis.title.y = element_text(size = 12, face= "plain")) +
      scale_y_continuous(labels = scales::percent_format())
  })
  
  ########## SECOND PAGE: SOCIAL WEB ##########
  
  output$social_web <- renderVisNetwork({
    color <- brewer.pal(10, "Blues")
    
    nodes2 <- read_csv("data/nodes2.csv")
    edges2 <- read_csv("data/edges2.csv")
    
    visNetwork(nodes2, edges2) %>%
      visGroups(groupname = "Dorms", color = color[5],
                shape = "square", size = 65) %>%
      visGroups(groupname = "Pre-Orientation", color = color[3], 
                shape = "square", size = 45) %>%
      visGroups(groupname = "Sports", color = color[1], 
                shape = "square", size = 45) %>%
      visGroups(groupname = "Student", shape = "circle") %>%
      visGroups(groupname = "Off Campus", color = color[7], 
                shape = "square", size = 65) %>%
      
      # add functionality to highlight close connections when hovering over node
      
      visOptions(nodesIdSelection = list(enabled = TRUE,
                                         style = "margin-bottom: -30px; visibility: hidden"),
                 highlightNearest = list(enabled = T, degree = 2, hover = T),
                 selectedBy = "group") %>%
      
      # adjust physics to decrease load time
      
      visPhysics(solver = "forceAtlas2Based",
                 timestep = 0.5,
                 minVelocity = 1,
                 maxVelocity = 30,
                 forceAtlas2Based = list(gravitationalConstant = -200, damping = 1),
                 stabilization = list(iterations = 300, updateInterval = 10),
                 adaptiveTimestep = TRUE) %>%
      
      # add legend for groups
      
      visLegend(zoom = FALSE) %>%
      
      # disable graph movement within window
      
      visInteraction(dragView = FALSE, 
                     zoomView = TRUE) %>%
      visNodes(label = "label", size = 50)
  })
  
  ########## THIRD PAGE: MOST CONNECTED ##########
  
  output$static_network <- renderPlot({
    color <- brewer.pal(4, "Set3")
  
  edges_full <- responses %>%
    select(id, first_id, second_id, third_id, fourth_id) %>%
    pivot_longer(cols = 2:5,
                 names_to = "degree", 
                 values_to = "endpoint") %>%
    mutate(colors = case_when(
      degree == "first_id" ~ color[1],
      degree == "second_id" ~ color[2],
      degree == "third_id" ~ color[3],
      degree == "fourth_id" ~ color[4]
    ))
  
  output$most_connected <- render_gt({
    responses %>%
      select(most_connected) %>%
      mutate(most_connected = ifelse(most_connected == 289,
                                     NA, most_connected)) %>% 
      drop_na() %>%
      group_by(most_connected) %>%
      summarize(count = n(), .groups = "drop") %>%
      arrange(desc(count)) %>%
      head(10) %>%
      rename("ID" = most_connected,
             "Number of Votes" = count) %>%
      gt() %>%
      tab_header(title = "Top Ten Socially Connected First-Years: Survey")
  })
  
  # Specified the edges, nodes, and top four friends
  
  edges <- edges_full %>% 
    select(id, endpoint)
  
  nodes <- responses %>% 
    select(id) 
  
  first <- responses %>% 
    select(first_id) 
  
  second <- responses %>% 
    select(second_id) 
  
  third <- responses %>% 
    select(third_id) 
  
  fourth <- responses %>% 
    select(fourth_id) 
  
  all_names <- full_join(fourth, full_join(third, full_join(first, second, by = c("first_id"="second_id")), by=c("third_id" = "first_id")), by=c("fourth_id" = "third_id"))
  
  
  nodes <- unique(full_join(nodes, all_names, by=c("id"="fourth_id")))
  
  g <- graph_from_data_frame(d = edges, vertices = nodes, directed=FALSE)
  
  l <- layout_on_sphere(g)
  
  
  plot(g, vertex.label="", layout = l, edge.width = 1, vertex.size=0.5, edge.color = edges_full$colors)
  
  title("Network",cex.main=3,col.main="black")
  
  legend("bottomright", c("First","Second", "Third", "Fourth"), pch=21,
         col="#777777", pt.bg=edges_full$colors, pt.cex=1, cex=.8)
  })
  
  ########## FOURTH PAGE: ANALYSIS ##########
  
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
        title = "Overall Satisfaction with Social Connections \n Among Harvard First-Years",
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
      mutate(location = ifelse(location == "At Home (in the US)" | 
                                 location == "At Home (international student)",
                               "At Home",
                               location)) %>%
      filter(location %in% c("The Yard", 
                             "A River House", 
                             "At Home",
                             "The Quad")) %>%
      group_by(location, satisfaction) %>%
      summarize(count = n(), .groups = "drop") %>%
      group_by(location) %>%
      mutate(perc = count / sum(count)) %>%
      ggplot(aes(x = fct_relevel(satisfaction,
                                 levels = c("Very Dissatisfied",
                                            "Dissatisfied",
                                            "Neutral",
                                            "Satisfied",
                                            "Very Satisfied")), 
                 y = perc)) +
      geom_col(fill = "#6fb4d2") +
      facet_wrap(~location) +
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
  
  output$gap_year_satisfaction <- renderPlot({
    responses %>%
      mutate(satisfaction = as_factor(satisfaction)) %>%
      select(satisfaction, gap_year) %>%
      filter(satisfaction != "NA",
             gap_year == "Yes") %>%
      group_by(satisfaction, gap_year) %>%
      summarize(count = n(), .groups = "drop") %>%
      ggplot(aes(x = fct_relevel(satisfaction,
                                 levels = c("Very Dissatisfied",
                                            "Dissatisfied",
                                            "Neutral",
                                            "Satisfied",
                                            "Very Satisfied")), 
                 y = count / sum(count))) +
      geom_col() +
      geom_col(fill = "#6fb4d2") +
      theme_bw() +
      labs(title = "Overall Satisfaction with Social Connections \n Among Harvard First-Years Who Took a Gap Year Last Year",
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
  
  output$satisfaction_by_living <- renderPlot({
    responses %>%
      
      mutate(satisfaction = as_factor(satisfaction)) %>%
      filter(satisfaction != "NA",
             living != "Not Applicable") %>%
      select(satisfaction, living) %>%
      group_by(living, satisfaction) %>%
      summarize(count = n(), .groups = "drop") %>%
      group_by(living) %>%
      mutate(perc = count / sum(count)) %>%
      ggplot(aes(x = fct_relevel(satisfaction,
                                 levels = c("Very Dissatisfied",
                                            "Dissatisfied",
                                            "Neutral",
                                            "Satisfied",
                                            "Very Satisfied")), 
                 y = perc)) +
      geom_col() +
      facet_wrap(~living) +
      geom_col(fill = "#6fb4d2") +
      theme_bw() +
      labs(title = "Overall Satisfaction with Social Connections \n Among Harvard First-Years by Living Situation",
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
  
  output$best_way <- renderPlot({
    responses %>%
      select(best) %>%
      mutate(count = n()) %>%
      ggplot(aes(x = fct_infreq(best))) +
      geom_col(aes(y = count / sum(count)),
               fill = "#6fb4d2") +
      coord_flip() +
      theme_bw() +
      labs(
        title = "Perceived Best Way \n to Form Connections",
        x = NULL,
        y = "Percent"
      ) +
      theme(title = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 14, face= "plain")) +
      scale_y_continuous(labels = scales::percent_format())
  })
  
  output$meet <- renderPlot({
    meet_1 <- responses %>%
      mutate(meet = p1_meet) %>%
      select(meet)
    
    meet_2 <- responses %>%
      mutate(meet = p2_meet) %>%
      select(meet)
    
    meet_3 <- responses %>%
      mutate(meet = p3_meet) %>%
      select(meet)
    
    meet_4 <- responses %>%
      mutate(meet = p4_meet) %>%
      select(meet)
    
    meet_merged <- rbind(meet_1, meet_2, by = "meet")
    meet_merged <- rbind(meet_merged, meet_3, by = "meet")
    meet_merged <- rbind(meet_merged, meet_4, by = "meet")
    
    meet_merged %>%
      mutate(count = n()) %>%
      filter(meet != "NA" & meet != "meet") %>%
      ggplot(aes(x = fct_infreq(meet))) +
      geom_col(aes(y = count / sum(count)),
               fill = "#6fb4d2") +
      coord_flip() +
      theme_bw() +
      labs(
        title = "Way First-Years \n Met Closest Friends",
        x = NULL,
        y = "Percent"
      ) +
      theme(title = element_text(size = 14, face = "bold"),
            axis.title.x = element_text(size = 12, face = "plain"),
            axis.title.y = element_text(size = 12, face= "plain")) +
      scale_y_continuous(labels = scales::percent_format())
  })
  
  output$stay_in_contact <- renderPlot({
    responses %>%
      mutate(contact = as_factor(contact)) %>%
      filter(contact != "NA" & contact != "contact") %>%
      group_by(contact) %>%
      summarize(count = n(), .groups = "drop") %>%
      ggplot(aes(x = fct_infreq(contact))) +
      geom_col(aes(y = count / sum(count)),
               fill = "#6fb4d2") +
      theme_bw() +
      coord_flip() +
      theme(legend.position = "none") +
      labs(
        title = "How First-Years \n Stay Connected",
        x = NULL,
        y = "Percent"
      ) +
      theme(title = element_text(size = 14, face = "bold"),
            axis.title.x = element_text(size = 12, face = "plain"),
            axis.title.y = element_text(size = 12, face= "plain")) +
      scale_y_continuous(labels = scales::percent_format())
  })
  
  output$in_person <- renderPlot({
    responses %>%
      mutate(in_person = as_factor(in_person)) %>%
      filter(in_person != "NA" & in_person != "in_person") %>%
      group_by(in_person) %>%
      summarize(count = n(), .groups = "drop") %>%
      ggplot(aes(x = fct_infreq(in_person))) +
      geom_col(aes(y = count / sum(count)),
               fill = "#6fb4d2") +
      theme_bw() +
      coord_flip() +
      theme(legend.position = "none") +
      labs(
        title = "Activities That First-Years \n Do In-Person",
        x = NULL,
        y = "Percent"
      ) +
      theme(title = element_text(size = 14, face = "bold"),
            axis.title.x = element_text(size = 12, face = "plain"),
            axis.title.y = element_text(size = 12, face= "plain")) +
      scale_y_continuous(labels = scales::percent_format())
  })
  
  output$expectation_matches <- renderPlot({
    expectations %>%
      select(best, p_meet_total) %>%
      mutate(reality = if_else(best %in% p_meet_total, TRUE, FALSE)) %>%
      drop_na() %>%
      ggplot(aes(x = reality)) +
      geom_bar(fill = "#6fb4d2") +
      theme_bw() +
      labs(x = "Response Matches Reality?", y = "Number of Responses",
           title = "Frequency of Expectations Meeting Reality") +
      annotate("text", x = 1, y = 140, label = "84") +
      annotate("text", x = 2, y = 270, label = "246") +
      theme(title = element_text(size = 14, face = "bold"),
            axis.title.x = element_text(size = 12, face = "plain"),
            axis.title.y = element_text(size = 12, face= "plain")) +
      scale_x_discrete(labels = c("No", "Yes"))
  })
  
  ########## FIFTH PAGE: REGRESSION MODEL ##########
  
  output$satisfaction_regression <- render_gt({
    model_data <- responses %>%
      mutate(gender = str_replace(gender, 
                                  pattern = "Genderqueer/Gender Non-Conforming", 
                                  replacement = "Other")) %>%
      mutate(gender = str_replace(gender, pattern = "Prefer not to say", 
                                  replacement = "Other")) %>%
      mutate(on_campus = if_else(!location %in% c("At Home (in the US)", 
                                                  "At Home (international student)"), TRUE, FALSE)) %>%
      select(on_campus, gender, satisfaction, group_size, location, race) %>%
      # mutate(on_campus = as.numeric(on_campus)) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very Satisfied", replacement = "2")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Satisfied", replacement = "1")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Neutral", replacement = "0")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Dissatisfied", replacement = "-1")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very Dissatisfied", replacement = "-2")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very -1", replacement = "-2"))
    
    model_data$satisfaction <- as.integer(as.character(model_data$satisfaction))
    model_data$group_size <- as.integer(as.character(model_data$group_size))
    
    fit_gs <- stan_glm(data = model_data,
                       formula = satisfaction ~ group_size + on_campus + on_campus:group_size + 1,
                       family = gaussian(),
                       refresh = 0)
    
    tbl_regression(fit_gs, intercept = TRUE) %>%
      as_gt() %>%
      tab_header(title = "Satisfaction Regression",
                 subtitle = 
                   "The Effect of Group Size and On-Campus Living on Social Satisfaction")
  })
  
  output$graph_1 <- renderPlot({
    model_data <- responses %>%
      mutate(gender = str_replace(gender, 
                                  pattern = "Genderqueer/Gender Non-Conforming", 
                                  replacement = "Other")) %>%
      mutate(gender = str_replace(gender, pattern = "Prefer not to say", 
                                  replacement = "Other")) %>%
      mutate(on_campus = if_else(!location %in% c("At Home (in the US)", 
                                                  "At Home (international student)"), TRUE, FALSE)) %>%
      select(on_campus, gender, satisfaction, group_size, location, race) %>%
      # mutate(on_campus = as.numeric(on_campus)) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very Satisfied", replacement = "2")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Satisfied", replacement = "1")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Neutral", replacement = "0")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Dissatisfied", replacement = "-1")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very Dissatisfied", replacement = "-2")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very -1", replacement = "-2"))
    
    model_data$satisfaction <- as.integer(as.character(model_data$satisfaction))
    model_data$group_size <- as.integer(as.character(model_data$group_size))
    
    fit_gs <- stan_glm(data = model_data,
                       formula = satisfaction ~ group_size + on_campus + on_campus:group_size + 1,
                       family = gaussian(),
                       refresh = 0)
    
    new_data = tibble(group_size = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20),
                      on_campus = TRUE)
    
    preds <- posterior_epred(fit_gs, newdata = new_data) %>%
      as_tibble() %>%
      rename("0" = `1`, "1" = `2`, "2" = `3`, "3" = `4`, "4" = `5`, "5" = `6`,
             "6" = `7`, "7" = `8`, "8" = `9`, "9" = `10`, "10" = `11`, "15" = `12`,
             "20" = `13`)
    
    long_preds <- preds %>%
      pivot_longer(cols = "0":"20", 
                   names_to = "Group_Size",
                   values_to = "Satisfaction")
    
    long_preds$"Group_Size" <- factor(long_preds$"Group_Size", levels= c("0":"20"))
    
    long_preds %>%
      ggplot(aes(x = `Satisfaction`, y = Group_Size, fill = stat(x))) +
      geom_density_ridges_gradient(bandwidth = 0.2) +
      scale_fill_viridis_c(name = "Group Size") +
      theme_classic() +
      labs(title = "Estimated Average Social Satisfaction", subtitle = 
             "Group Sizes 0-20 for On-Campus Students", y = "Group Size")
  })
  
  output$graph_2 <- renderPlot({
    model_data <- responses %>%
      mutate(gender = str_replace(gender, 
                                  pattern = "Genderqueer/Gender Non-Conforming", 
                                  replacement = "Other")) %>%
      mutate(gender = str_replace(gender, pattern = "Prefer not to say", 
                                  replacement = "Other")) %>%
      mutate(on_campus = if_else(!location %in% c("At Home (in the US)", 
                                                  "At Home (international student)"), TRUE, FALSE)) %>%
      select(on_campus, gender, satisfaction, group_size, location, race) %>%
      # mutate(on_campus = as.numeric(on_campus)) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very Satisfied", replacement = "2")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Satisfied", replacement = "1")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Neutral", replacement = "0")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Dissatisfied", replacement = "-1")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very Dissatisfied", replacement = "-2")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very -1", replacement = "-2"))
    
    model_data$satisfaction <- as.integer(as.character(model_data$satisfaction))
    model_data$group_size <- as.integer(as.character(model_data$group_size))
    
    fit_gs <- stan_glm(data = model_data,
                       formula = satisfaction ~ group_size + on_campus + on_campus:group_size + 1,
                       family = gaussian(),
                       refresh = 0)
    
    new_data = tibble(group_size = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20),
                      on_campus = TRUE)
    
    preds <- posterior_epred(fit_gs, newdata = new_data) %>%
      as_tibble() %>%
      rename("0" = `1`, "1" = `2`, "2" = `3`, "3" = `4`, "4" = `5`, "5" = `6`,
             "6" = `7`, "7" = `8`, "8" = `9`, "9" = `10`, "10" = `11`, "15" = `12`,
             "20" = `13`)
    
    preds %>%
      as_tibble() %>%
      mutate(diff = `5` - `0`) %>%
      ggplot(aes(x = diff)) +
      geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100, 
                     fill = "#6fb4d2", alpha = 0.6) +
      theme_bw() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(y = "Percentage", x = "Difference", title = 
             "Estimated Average Difference in Social Satisfaction", subtitle = 
             "Between On-Campus Students with Group Sizes 5 and 0")
  })
  
  output$graph_3 <- renderPlot({
    model_data <- responses %>%
      mutate(gender = str_replace(gender, 
                                  pattern = "Genderqueer/Gender Non-Conforming", 
                                  replacement = "Other")) %>%
      mutate(gender = str_replace(gender, pattern = "Prefer not to say", 
                                  replacement = "Other")) %>%
      mutate(on_campus = if_else(!location %in% c("At Home (in the US)", 
                                                  "At Home (international student)"), TRUE, FALSE)) %>%
      select(on_campus, gender, satisfaction, group_size, location, race) %>%
      # mutate(on_campus = as.numeric(on_campus)) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very Satisfied", replacement = "2")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Satisfied", replacement = "1")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Neutral", replacement = "0")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Dissatisfied", replacement = "-1")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very Dissatisfied", replacement = "-2")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very -1", replacement = "-2"))
    
    model_data$satisfaction <- as.integer(as.character(model_data$satisfaction))
    model_data$group_size <- as.integer(as.character(model_data$group_size))
    
    fit_gs <- stan_glm(data = model_data,
                       formula = satisfaction ~ group_size + on_campus + on_campus:group_size + 1,
                       family = gaussian(),
                       refresh = 0)
    
    fit_gs %>%
      as_tibble() %>%
      mutate(on = `(Intercept)` + on_campusTRUE) %>%
      rename(off = `(Intercept)`) %>%
      select(on, off) %>%
      pivot_longer(cols = off:on, 
                   names_to = "parameters",
                   values_to = "satisfaction") %>%
      ggplot(aes(satisfaction, fill = parameters)) +
      geom_histogram(aes(y = after_stat(count/sum(count))),
                     alpha = 0.5, 
                     bins = 100, 
                     position = "identity") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(labels = c("Off Campus", "On Campus"),
                        values = c("#6fb4d2", "#203e5f")) +
      theme_bw() +
      labs(x = "Social Satisfaction", 
           y = "Probabiliy", 
           title = "Posterior Probability Distribution", 
           subtitle = "Average Satisfaction for On vs. Off Campus Students", 
           fill = "Living Situation")
  })
  
  output$graph_4 <- renderPlot({
    model_data <- responses %>%
      mutate(gender = str_replace(gender, 
                                  pattern = "Genderqueer/Gender Non-Conforming", 
                                  replacement = "Other")) %>%
      mutate(gender = str_replace(gender, pattern = "Prefer not to say", 
                                  replacement = "Other")) %>%
      mutate(on_campus = if_else(!location %in% c("At Home (in the US)", 
                                                  "At Home (international student)"), TRUE, FALSE)) %>%
      select(on_campus, gender, satisfaction, group_size, location, race) %>%
      # mutate(on_campus = as.numeric(on_campus)) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very Satisfied", replacement = "2")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Satisfied", replacement = "1")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Neutral", replacement = "0")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Dissatisfied", replacement = "-1")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very Dissatisfied", replacement = "-2")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very -1", replacement = "-2"))
    
    model_data$satisfaction <- as.integer(as.character(model_data$satisfaction))
    model_data$group_size <- as.integer(as.character(model_data$group_size))
    
    model_data3 <- model_data %>%
      filter(location %in% c("The Yard", "The Quad", "A River House")) %>%
      mutate(location = str_replace(location, 
                                    pattern = "The Yard", 
                                    replacement = "yard")) %>%
      mutate(location = str_replace(location, 
                                    pattern = "The Quad", 
                                    replacement = "quad")) %>%
      mutate(location = str_replace(location, 
                                    pattern = "A River House", 
                                    replacement = "river"))
    # filter(location == c("yard", "quad"))
    
    yvq_model_1 <- stan_glm(data = model_data3,
                            formula = satisfaction ~ location,
                            refresh = 0)
    
    yvq_model_clean1 <- yvq_model_1 %>%
      as_tibble() %>%
      mutate(yard = `(Intercept)` + locationyard + locationriver) %>%
      rename(quad = `(Intercept)`) %>%
      select(yard, quad) %>%
      pivot_longer(cols = yard:quad, 
                   names_to = "parameters",
                   values_to = "satisfaction")
    
    yvq_model_clean1 %>%
      ggplot(aes(satisfaction, fill = parameters)) +
      geom_histogram(aes(y = after_stat(count/sum(count))),
                     alpha = 0.5, 
                     bins = 100, 
                     position = "identity") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(labels = c("Quad", "Yard"),
                        values = c("#6fb4d2", "#203e5f")) +
      theme_bw() +
      labs(x = "Social Satisfaction", 
           y = "Probability", 
           title = "Posterior Probability Distribution", 
           subtitle = "Average Satisfaction for Quad vs. Yard Students",
           caption = "Model Formula: satisfaction ~ location", 
           fill = "Living Situation")
  })
  
  output$graph_5 <- renderPlot({
    model_data <- responses %>%
      mutate(gender = str_replace(gender, 
                                  pattern = "Genderqueer/Gender Non-Conforming", 
                                  replacement = "Other")) %>%
      mutate(gender = str_replace(gender, pattern = "Prefer not to say", 
                                  replacement = "Other")) %>%
      mutate(on_campus = if_else(!location %in% c("At Home (in the US)", 
                                                  "At Home (international student)"), TRUE, FALSE)) %>%
      select(on_campus, gender, satisfaction, group_size, location, race) %>%
      # mutate(on_campus = as.numeric(on_campus)) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very Satisfied", replacement = "2")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Satisfied", replacement = "1")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Neutral", replacement = "0")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Dissatisfied", replacement = "-1")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very Dissatisfied", replacement = "-2")) %>%
      mutate(satisfaction = str_replace(
        satisfaction, pattern = "Very -1", replacement = "-2"))
    
    model_data$satisfaction <- as.integer(as.character(model_data$satisfaction))
    model_data$group_size <- as.integer(as.character(model_data$group_size))
    
    model_data3 <- model_data %>%
      filter(location %in% c("The Yard", "The Quad", "A River House")) %>%
      mutate(location = str_replace(location, 
                                    pattern = "The Yard", 
                                    replacement = "yard")) %>%
      mutate(location = str_replace(location, 
                                    pattern = "The Quad", 
                                    replacement = "quad")) %>%
      mutate(location = str_replace(location, 
                                    pattern = "A River House", 
                                    replacement = "river"))
    # filter(location == c("yard", "quad"))
    
    yvq_model_2 <- stan_glm(data = model_data3,
                            formula = group_size ~ location,
                            refresh = 0)
    
    yvq_model_clean2 <- yvq_model_2 %>%
      as_tibble() %>%
      mutate(yard = `(Intercept)` + locationyard + locationriver) %>%
      rename(quad = `(Intercept)`) %>%
      select(yard, quad) %>%
      pivot_longer(cols = yard:quad, 
                   names_to = "parameters",
                   values_to = "group_size")
    
    yvq_model_clean2 %>%
      ggplot(aes(group_size, fill = parameters)) +
      geom_histogram(aes(y = after_stat(count/sum(count))),
                     alpha = 0.5, 
                     bins = 100, 
                     position = "identity") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(labels = c("Quad", "Yard"),
                        values = c("#6fb4d2", "#203e5f")) +
      theme_bw() +
      labs(x = "Group Size", y = "Probability", 
           title = "Posterior Probability Distribution", 
           subtitle = "Average Group Size for Quad vs. Yard Students",
           caption = "Model Formula: group_size ~ location", 
           fill = "Living Situation")
  })


  ########## SIXTH PAGE: ABOUT ##########
  
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