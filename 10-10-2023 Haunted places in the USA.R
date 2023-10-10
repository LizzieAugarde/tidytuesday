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

ggplot(aes(x = state, y = number_places)) +
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

number_by_state <- left_join(number_by_state, state_size, by = c("state" = "State"))

number_by_state <- number_by_state %>%
  mutate(number_per_sq_mile = (number_places/area)*1000)

         