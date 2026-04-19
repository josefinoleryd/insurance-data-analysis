source("scripts/01_load_data.R")
library(tidyverse)

# Funktion för att hitta typvärdet (mode)

get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Städa och Feature Engineering i samma pipe

data_clean <- data_raw %>%
  mutate(
    
    # Standardisera text
    across(where(is.character), ~str_to_lower(str_trim(.))),
    
    # Hantera saknade värden
    bmi = if_else(
      is.na(bmi),
      median(bmi, na.rm = TRUE),
      bmi),
    annual_checkups = if_else(
      is.na(annual_checkups),
      median(annual_checkups, na.rm = TRUE),
      annual_checkups),
    exercise_level = if_else(
      is.na(exercise_level),
      get_mode(exercise_level),
      exercise_level),
    
    # Logiska variabler
    smoker = recode(smoker, "yes" = TRUE, "no" = FALSE),
    chronic_condition = recode(chronic_condition, "yes" = TRUE, "no" = FALSE),
    
    # Kategoriska variabler till faktorer
    region = as.factor(region),
    exercise_level = as.factor(exercise_level),
    plan_type = as.factor(plan_type),
    sex = as.factor(sex),
    
    # Nya variabler
    bmi_category = case_when(
      bmi < 18.5 ~ "underweight",
      bmi < 25.0 ~ "normal",
      bmi < 30.0 ~ "overweight",
      TRUE ~ "obese"
    ),
    age_category = case_when(
      age < 35 ~ "young",
      age < 55 ~ "middle-aged",
      TRUE ~ "senior"
    ),
    prior_events = prior_accidents + prior_claims,
    risk_score = 
      as.integer(smoker) +
      as.integer(chronic_condition) +
      if_else(bmi >= 30, 1, 0),
    
    # Göra nya variablerna till faktorer
    bmi_category = as.factor(bmi_category),
    age_category = as.factor(age_category)
)

glimpse(data_clean)
summary(data_clean)
str(data_clean)
         