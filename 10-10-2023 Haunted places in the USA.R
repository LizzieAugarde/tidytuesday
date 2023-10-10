library(tidytuesdayR)
library(tidyverse)

### 10/10/2023 - haunted places in the USA 

haunted_places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-10/haunted_places.csv')

#number of places by state 
number_by_state <- haunted_places %>%
  group_by(state) %>%
  summarise(number_places = n()) %>%
  arrange(desc(number_places)) %>%
  mutate(state = factor(state, levels = state))

number_by_state_graph <- ggplot(number_by_state, aes(x = state, y = number_places)) +
    geom_bar(stat = "identity", fill = "#3eb0d2") +
    labs(x = "State", y = "Number of haunted places") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 700),
                       breaks = scales::breaks_width(100)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=90, hjust = 1))


#number of places by state area size 
library(rvest)
library(stringr)

webpage <- read_html("https://www.states101.com/populations")

table_node <- html_node(webpage, "#populationTable")
table_content <- html_table(table_node)
head(table_content)

state_size <- table_content %>%
  mutate(area = gsub("\\s*\\([^\\)]+\\)","", as.character(`Land Area (sq mi)`))) %>%
  mutate(area = as.numeric(gsub(",", "", area)))

number_by_state_size <- left_join(number_by_state, state_size, by = c("state" = "State"))

number_by_state_size <- number_by_state_size %>%
  mutate(number_per_sq_mile = (number_places/area)*1000) %>%
  mutate(number_per_sq_mile_cat = case_when(number_per_sq_mile < 10 ~ "Less than 10", 
                                            number_per_sq_mile >= 10 & number_per_sq_mile < 20 ~ "10 to 20",
                                            number_per_sq_mile >= 20 & number_per_sq_mile < 40 ~ "20 to 40",
                                            number_per_sq_mile >= 40 & number_per_sq_mile < 60 ~ "40 to 60",
                                            TRUE ~ "More than 60"))

number_by_state_size_graph <- ggplot(number_by_state_size, aes(x = state, y = number_per_sq_mile)) +
  geom_bar(stat = "identity", fill = "#3eb0d2") +
  labs(x = "State", y = "Number of haunted places per 1000 square miles") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))
         
number_by_state_size_graph


#number of places by state area size plotted on a basic thematic
library(tigris) # for boundary data
library(tmap) # for mapping functions

states <- states(cb = TRUE) # downloading states boundary file

states <- left_join(states, number_by_state_size, by = c("NAME" = "state"))

states <- states %>% 
  filter(!is.na(number_per_sq_mile), !NAME %in% c("Hawaii", "Alaska"))

tm_shape(states) + 
  tm_fill(col = "number_per_sq_mile_cat") +
  tm_borders()


#number of places by state area size plotted on a hexagonal thematic map 
