library(tidyverse)
library(readxl)
library(ggplot2)
library(usmap)

knitr::opts_chunk$set(echo = TRUE)

#Read in of election Dataset
election_orig <- read.csv("~/601 Final Project/posts/_data/1976-2020-president.csv")

view(election1)
#Summarize Dataset election_orig
print(summarytools::dfSummary(election_orig))

#Select Pertinant Data from dataset
election1 <- election_orig %>%
  select(year, state, candidate, party_detailed, writein, candidatevotes, totalvotes) %>%
  mutate(state = tolower(state)) %>%
  mutate(percentvotes = (candidatevotes/totalvotes) * 100) %>%
  mutate(candidate = case_when(
    writein == TRUE ~ "Write-In Votes",
    writein == FALSE ~ candidate)) %>%
  mutate(party_detailed = case_when(
    writein == TRUE ~ "Other",
    writein == FALSE ~ party_detailed)) %>%
  select(!writein)

#Summarize Dataset election1
print(summarytools::dfSummary(election1))



#Visualization 1: totalvotes by state heat map

#Step 1: create data frame where each state is assigned totalvotes for each year
election_participation <- election1 %>%
  select(year, state, totalvotes) %>%
  distinct() %>%
  pivot_wider(names_from = year,
              values_from = totalvotes,
              id_cols = state)

#Step 2: create data frame to represent geographic data of each state
mapdata <- us_map() %>%
  select(x, y, group, full) %>%
  mutate(state = full) %>%
  select(!full)

mapdata$state <- tolower(mapdata$state)

#Step 3: Join mapdata and election_participation by state, remove subregion,
#pivot so all data is organized by year and total_votes
mapdata_totalvotes <- left_join(mapdata, election_participation, by = "state") %>%
  pivot_longer(cols = c("1976" : "2020"),
               names_to = "year",
               values_to = "total_votes")

#Step 4: Visualize Data and create heat map for each state using facet
map_totalvotes <- ggplot(mapdata_totalvotes, aes(x = x,
                                                 y = y,
                                                 fill = total_votes,
                                                 group = group)) +
  facet_wrap(vars(year)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(name = "Total Votes", low = "white", high = "red", na.value = "grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())

map_totalvotes





#Visualization 2: winner of election year by state mapping

#Step 1: create data frame where each state is assigned max percentvotes for each year & state
election_winners <- election1 %>%
  select(!candidatevotes & !totalvotes & !candidate) %>%
  group_by(year, state) %>%
  slice_max(n = 1, percentvotes) %>%
  pivot_wider(names_from = year,
              values_from = party_detailed,
              id_cols = state)

#Step 2: Join mapdata and election_winners by state, remove subregion,
#pivot so all data is organized by year and Party
mapdata_winners <- left_join(mapdata, election_winners, by = "state") %>%
  pivot_longer(cols = c("1976" : "2020"),
               names_to = "year",
               values_to = "Party")

#Step 3: Visualize Data and create heat map for each state using facet
map_winners <- ggplot(mapdata_winners, aes(x = x,
                                           y = y,
                                           fill = Party,
                                           group = group)) +
  facet_wrap(vars(year)) +
  geom_polygon(color = "black") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())


#Step 4: create color list for each party
color_list <- c("DEMOCRAT" = "blue", "DEMOCRATIC-FARMER-LABOR" = "green", "other" = "yellow", "REPUBLICAN" = "red")

#Step 5: Visualize Data and implement color list
map_winners + scale_colour_manual(values = color_list,aesthetics = c("colour", "fill"))



#Visualization 3 & 4: growth of total_votes
total_votes_data <- election1 %>%
  select(year, state, totalvotes) %>%
  distinct()

total_votes_group <- ggplot(total_votes_data, aes(x = year,
                                                  y = totalvotes,
                                                  group = state,
                                                  color = state)) +
  geom_line() +
  ggtitle("Total Votes Growth")

total_votes_group

total_votes_indv <- ggplot(total_votes_data, aes(x = year,
                                                 y = totalvotes,
                                                 group = state,
                                                 color = state)) +
  facet_wrap(vars(state)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_line() +
  theme(legend.position = "none") +
  ggtitle("Total Votes Growth by State")

total_votes_indv



#Visualization 5 & 6: growth of total_votes
writein_votes_data <- election1 %>%
  filter(candidate == "Write-In Votes") %>%
  select(year, state, totalvotes)

writein_votes_group <- ggplot(writein_votes_data, aes(x = year,
                                                  y = totalvotes,
                                                  group = state,
                                                  color = state)) +
  geom_line() +
  ggtitle("Write-In Votes Growth")

writein_votes_group

writein_votes_indv <- ggplot(writein_votes_data, aes(x = year,
                                                 y = totalvotes,
                                                 group = state,
                                                 color = state)) +
  facet_wrap(vars(state)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_line() +
  theme(legend.position = "none") + 
  ggtitle("Write-In Votes Growth by State")

writein_votes_indv
