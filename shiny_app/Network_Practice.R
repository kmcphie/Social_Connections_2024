# Networking Practice 

library(igraph)
library(readr)
library(tidyverse)
library(shiny)
library(shinythemes)
library(readxl)

responses <- readRDS("responses.rds")

# Home/Desktop/Gov50Projects/Social_Connections_2024/shiny_app/


actorNetwork <- graph_from_data_frame(d=movies, vertices=actors, directed=F)

# RPubs Walk Through ------------------------------------------------------


actors <- read_csv("https://raw.githubusercontent.com/OPER682-Tucker/Social-Network-Analysis/master/Actors.csv",
                   col_type = cols(
                     Actor = col_character(),
                     Gender = col_character(),
                     BestActorActress = col_character()
                   ))

movies <- read_csv("https://raw.githubusercontent.com/OPER682-Tucker/Social-Network-Analysis/master/Movies.csv",
                   col_type = cols(
                     `Actor 1` = col_character(),
                     `Actor 2` = col_character(),
                     Movie = col_character()
                   ) )

# The first step in building the network is to create an igraph object. 
# The d variable takes the edges connecting the actor nodes that are held in the
# movies dataframe that was created, the vertices variable takes the actor nodes
# that are listed in the actors dataframe.

# The vertices variable requires one column of node identifiers, which in this 
# case are the actors’ names. The d argument requires a dataframe in two columns
# of connections between vertice identifiers.

actorNetwork <- graph_from_data_frame(d=movies, vertices=actors, directed=F)

plot(actorNetwork)

# color by movie 

E(actorNetwork)$color <- ifelse(E(actorNetwork)$Movie == "Forest Gump", "green", 
                                ifelse(E(actorNetwork)$Movie == "Apollo 13", 
                                       "black",
                                       "orange"))

plot(actorNetwork)

# color by charaterics of the actor
V(actorNetwork)$color <- ifelse(V(actorNetwork)$BestActorActress == "Winner", 
                                "gold",
                                ifelse(V(actorNetwork)$BestActorActress == 
                                         "Nominated","grey",
                                       "lightblue"))

plot(actorNetwork)

# creates a legend but this doesn't work for me 

# plot(actorNetwork, vertex.frame.color="white") %>%
# 
# legend("bottomright", c("Winner","Nominee", "Not Nominated"), pch=21,
#   col="#777777", pt.bg=c("gold","grey","lightblue"), pt.cex=2, cex=.8)
# 
# 
# legend("topleft", c("Forest Gump","Apollo 13", "The Rock"), 
#        col=c("green","black","orange"), lty=1, cex=.8)


# Star Wars Example -------------------------------------------------------


# http://pablobarbera.com/big-data-upf/html/02a-networks-intro-visualization.html

edges <- read.csv("data/star-wars-network-edges.csv")
head(edges)

nodes <- read.csv("data/star-wars-network-nodes.csv")
head(nodes)

# We can use the graph_from_data_frame function, which takes two arguments: d, 
# the data frame with the edge list in the first two columns; and vertices, a 
# data frame with node data with the node label in the first column. (Note that 
# igraph calls the nodes vertices, but it’s exactly the same thing.)

g <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)
g

# Use graph_from_data_frame and then plot() to plot it 


# Jeremiah's Code ---------------------------------------------------------


output$mark_plot <- renderVisNetwork({
  nodes2 <- read_csv("data/nodes2.csv")
  edges2 <- read_csv("data/edges2.csv")
  
  # Changed shape, color, and size for each group
  
  visNetwork(nodes2, edges2) %>%
    visGroups(groupname = "Dorms", color = "darkblue", shape = "square", size = 65) %>%
    visGroups(groupname = "Pre-Orientation", color = "darkred", shape = "square", size = 45) %>%
    visGroups(groupname = "Sports", color = "darkgreen", shape = "square", size = 45) %>%
    
    # Added functionality to highlight close connections when hovering over node
    
    visOptions(nodesIdSelection = list(enabled = TRUE,
                                       style = "margin-bottom: -30px; visibility: hidden"),
               highlightNearest = list(enabled = T, degree = 2, hover = T),
               selectedBy = "group") %>%
    
    # Adjusted physics to decrease load time
    
    visPhysics(
      solver = "forceAtlas2Based", 
      timestep = 0.5,
      minVelocity = 1,
      maxVelocity = 30,
      forceAtlas2Based = list(gravitationalConstant = -200, damping = 1),
      stabilization = list(iterations = 300, updateInterval = 10),
      adaptiveTimestep = TRUE) %>%
    
    # Added legend for groups
    
    visLegend(zoom = FALSE) %>%
    
    # Disabled graph movement within window
    
    visInteraction(dragView = FALSE, 
                   zoomView = FALSE,
                   navigationButtons = TRUE)
  
  
})

f