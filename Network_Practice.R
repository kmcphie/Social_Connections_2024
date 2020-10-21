# Networking Practice

library(igraph)
library(readr)
library(tidyverse)
library(shiny)
library(shinythemes)
library(readxl)

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


responses <- readRDS("responses.rds")

# Home/Desktop/Gov50Projects/Social_Connections_2024/shiny_app/

excel_responses <- read_excel("shiny_app/raw_data/survey_responses.xlsx")

actorNetwork <- graph_from_data_frame(d=movies, vertices=actors, directed=F)