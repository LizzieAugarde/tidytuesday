library(tidytuesdayR)
library(tidyverse)

### 24/10/2023 - Patient Risk Profiles

patient_risk_profiles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-24/patient_risk_profiles.csv')


# grouping into risk by age group, sex and prior condition
patient_risk_profiles <- patient_risk_profiles %>%
  pivot_longer(cols = starts_with("age group"),
               names_to = "age_group", 
               values_to = "number_age_group") %>%
  pivot_longer(cols = starts_with("Sex"), 
               names_to = "sex", 
               values_to = "number_sex") %>%
  pivot_longer(cols = ends_with(" in prior year"), 
               names_to = "prior_condition", 
               values_to = "number_prior_condition") %>%
  mutate(age_group = trimws(gsub('age group: ', '', age_group)), 
         sex = gsub('Sex = ', '', sex)) %>%
  mutate(prior_condition = ifelse(number_prior_condition == 0, paste0("No ", prior_condition), prior_condition)) %>%
  filter(number_age_group == 1, number_sex == 1) %>%
  select(-c(number_age_group, number_sex, number_prior_condition, personId)) %>%
  mutate(across(where(is.numeric), ~.*100))

#mean risk across groups of patients by age, sex and prior conditions
mean_risk_by_group <- patient_risk_profiles %>%
  group_by(age_group, sex, prior_condition) %>%
  summarise(across(everything(), list(mean))) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  mutate(prior_condition = str_to_title(str_replace(prior_condition, " in prior year", ""))) %>%
  mutate(sex = str_to_title(sex))

#tidying column names for app
colstrings_to_remove <- c("predicted risk of ", "predicted risk of  ", "_1")

for (string in colstrings_to_remove) {
  colnames(mean_risk_by_group) <- str_replace(colnames(mean_risk_by_group), string, "")
  colnames(mean_risk_by_group) <- str_trim(colnames(mean_risk_by_group))
}
