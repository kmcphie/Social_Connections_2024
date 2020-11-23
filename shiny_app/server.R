# Read in datasets created using the 'gather_raw_data.R' script.

responses <- readRDS("responses_final.rds")

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
  
  output$satisfaction_by_living <- renderPlot({
    responses %>%
      mutate(satisfaction = as_factor(satisfaction)) %>%
      filter(satisfaction != "NA",
             living != "Not Applicable") %>%
      select(satisfaction, living) %>%
      group_by(satisfaction, living) %>%
      summarize(count = n(), .groups = "drop") %>%
      ggplot(aes(x = fct_relevel(satisfaction,
                                 levels = c("Very Dissatisfied",
                                            "Dissatisfied",
                                            "Neutral",
                                            "Satisfied",
                                            "Very Satisfied")), 
                 y = count / sum(count))) +
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
        title = "Ways That First-Years \n Stay Connected",
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
        title = "Things That First-Years \n Do In-Person",
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
    labs(x = "Matches", y = "Count",
         title = "Frequency of Reality Meeting Expectations") +
    annotate("text", x = 1, y = 140, label = "84") +
    annotate("text", x = 2, y = 270, label = "246") +
    theme(title = element_text(size = 14, face = "bold"),
          axis.title.x = element_text(size = 12, face = "plain"),
          axis.title.y = element_text(size = 12, face= "plain"))
  
})
  
  ########## FIFTH PAGE: ABOUT ##########
  
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