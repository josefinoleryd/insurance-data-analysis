# ---- s2-ladda-in-paket-och-source

source("scripts/01_load_data.R")
library(tidyverse)

# ---- s2-funktion-för-typvärde

get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# ---- s2-städa-och-feature-engineering

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
    
    # Logiska variabler för beräkningar
    smoker_logic = recode(smoker, "yes" = TRUE, "no" = FALSE),
    chronic_condition_logic = recode(chronic_condition, "yes" = TRUE, "no" = FALSE),
    
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
      as.integer(smoker_logic) +
      as.integer(chronic_condition_logic) +
      if_else(bmi >= 30, 1, 0),
    
    # Kategoriska variabler till faktorer
    smoker = factor(smoker_logic, levels = c(FALSE, TRUE), labels = c("no", "yes")),
    chronic_condition = factor(chronic_condition_logic, levels = c(FALSE, TRUE), labels = c("no", "yes")),
    region = as.factor(region),
    exercise_level = factor(exercise_level, levels = c("low", "medium", "high")),
    plan_type = factor(plan_type, levels = c("basic", "standard", "premium")),
    sex = as.factor(sex),
    bmi_category = factor(bmi_category, levels = c("underweight", "normal", "overweight", "obese")),
    age_category = factor(age_category, levels = c("young", "middle-aged", "senior"))
) %>%
  select(-smoker_logic, -chronic_condition_logic)

# ---- kontrollera-att-det-blev-korrekt

glimpse(data_clean)

data_clean %>%
  select(where(is.numeric)) %>%
  summary()

data_clean %>%
  select(where(is.factor)) %>%
  summary()
         
