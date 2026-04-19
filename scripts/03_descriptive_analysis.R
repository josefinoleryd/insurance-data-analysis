source("scripts/02_prepare_data.R")
library(tidyverse)
library(ggcorrplot)

numeric_vars <- data_clean %>%
  select(charges, age, bmi, prior_accidents, prior_claims, annual_checkups, prior_events, risk_score)

cor_matrix <- cor(numeric_vars)

corr_plot <- ggcorrplot(cor_matrix,
                     type = "upper",
                     lab = TRUE,
                     lab_size = 3,
                     colors = c("#E4672E", "white", "#6D9EC1"),
                     title = "Korrelation mellan variabler och försäkringskostnad (charges)",
                     ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))

corr_plot
ggsave("output/eda/corr_plot.png", plot = corr_plot, width = 7, height = 7, dpi = 300)