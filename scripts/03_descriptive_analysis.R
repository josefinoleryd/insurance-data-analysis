source("scripts/02_prepare_data.R")
library(tidyverse)
library(ggcorrplot)

# Skapa korrelationsmatris för att se intressanta variabler ----

numeric_vars <- data_clean %>%
  select(age, bmi, prior_accidents, prior_claims, annual_checkups, prior_events, risk_score, charges)

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
ggsave("output/analysis/corr_plot.png", plot = corr_plot, width = 7, height = 7)

# Boxplots grupper med olika hälsorisker och livsstilar ----

health_risk_plot <- data_clean %>%
  select(charges, smoker, chronic_condition, bmi_category, risk_score) %>%
  mutate(across(-charges, as.character)) %>%
  pivot_longer(-charges, names_to = "variable", values_to = "value") %>%
  mutate(value = factor(value, levels = c(
    "no", "yes",
    "underweight", "normal", "overweight", "obese",
    "0", "1", "2", "3"
  ))) %>%
  ggplot(aes(x = value, y = charges)) +
  geom_boxplot(
    fill = "lightblue",
    color = "black",
    outlier.alpha = 0.6) +
  facet_wrap(~variable, scales = "free_x") + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold")) +
  labs(
    title = "Kostnadsskillnader baserat på hälso- och riskfaktorer",
    x = "Kategori",
    y = "Kostnad (charges)"
  )

health_risk_plot
ggsave("output/analysis/health_risk_plot.png", plot = health_risk_plot, width = 7, height = 7)

# Boxplots demografiska grupper ----

demographic_plot <- data_clean %>%
  select(charges, age_category, sex) %>%
  mutate(across(-charges, as.character)) %>%
  pivot_longer(-charges, names_to = "variable", values_to = "value") %>%
  mutate(value = factor(value, levels = c(
    "young", "middle-aged", "senior",
    "male", "female"
  ))) %>%
  ggplot(aes(x = value, y = charges)) +
  geom_boxplot(
    fill = "lightblue",
    color = "black",
    outlier.alpha = 0.6) +
  facet_wrap(~variable, scales = "free_x") + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold")) +
  labs(
    title = "Kostnadsskillnader baserat på ålder och kön",
    x = "Kategori",
    y = "Kostnad (charges)"
  )

demographic_plot
ggsave("output/analysis/demographic_plot.png", plot = demographic_plot, width = 7, height = 7)


# Stapeldiagram över genomsnittlig kostnad per Risk Score ----

# Skapa sammanfattande tabell

risk_score_summary <- data_clean %>%
  group_by(risk_score) %>%
  summarise(
    Antal = n(),
    Medelvärde = mean(charges),
    Median = median(charges)
  )

print(risk_score_summary)

# Plotta i stapeldiagram

risk_summary_plot <- risk_score_summary %>%
  pivot_longer(cols = c(Medelvärde, Median),
               names_to = "Statistiktyp",
               values_to = "Kostnad") %>%
  ggplot(aes(x = factor(risk_score), y = Kostnad, fill = Statistiktyp)) +
  geom_col(position = "dodge", color = "black") +
  geom_text(aes(label = round(Kostnad, 0)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Medelvärde" = "lightblue", "Median" = "steelblue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Jämförelse av medelvärde och median per risk score",
    x = "Risk score (0-3)",
    y = "Kostnad (charges)",
    fill = "Mått"
  )

risk_summary_plot
ggsave("output/analysis/risk_summary_plot.png", plot = risk_summary_plot, width = 7, height = 7)
