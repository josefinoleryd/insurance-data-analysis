source("scripts/01_load_data.R")
library(tidyverse)

# Funktion för att hitta typvärdet (mode)

get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

data_clean <- data_raw %>%
  mutate(
    
    # Standardisera text
    across(where(is.character), ~str_to_lower(str_trim(.))),
    
    # Logiska variabler
    smoker = recode(smoker, "yes" = TRUE, "no" = FALSE),
    chronic_condition = recode(chronic_condition, "yes" = TRUE, "no" = FALSE),
    
    # Hantera saknade värden
    bmi = if_else(
      is.na(bmi),
      median(bmi, na.rm = TRUE),
      bmi
    ),
    annual_checkups = if_else(
      is.na(annual_checkups),
      median(annual_checkups, na.rm = TRUE),
      annual_checkups
    ),
    exercise_level = if_else(
      is.na(exercise_level),
      get_mode(exercise_level),
      exercise_level
    ),
    
    # Kategoriska variabler till faktorer
    region = as.factor(region),
    exercise_level = as.factor(exercise_level),
    plan_type = as.factor(plan_type),
    sex = as.factor(sex),

)

glimpse(data_clean)
summary(data_clean)
         