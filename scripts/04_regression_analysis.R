source("scripts/03_descriptive_analysis.R")
library(tidyverse)
library(modelsummary)
library(webshot2)
library(broom)



# Börjar med scatterplots över interessanta kontinuerliga variabler ----

# age

scatter_age <- ggplot(data_clean, aes(x = age, y = charges)) +
  geom_point(
    shape = 21, 
    fill = "steelblue", 
    color = "black", 
    alpha = 0.6,
    size = 2) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold")
  ) +
  labs(
    title = "Samband mellan ålder och försäkringskostnad",
    x = "Ålder",
    y = "Kostnad (charges)"
  )

# Visa och spara plotten

scatter_age

ggsave("output/regression/scatter_age.png", plot = scatter_age, width = 7, height = 5)

# bmi

scatter_bmi <- ggplot(data_clean, aes(x = bmi, y = charges)) +
  geom_point(
    shape = 21, 
    fill = "steelblue",
    color = "black",
    alpha = 0.6,
    size = 2) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold")
  ) +
  labs(
    title = "Samband mellan BMI och försäkringskostnad",
    x = "BMI",
    y = "Kostnad (charges)"
  )

# Visa och spara plotten

scatter_bmi

ggsave("output/regression/scatter_bmi.png", plot = scatter_bmi, width = 7, height = 5)



# Skapa enkel regressionsmodell charges vs risk_score (baseline) ----

model_baseline <- lm(charges ~ risk_score, data = data_clean)

summary(model_baseline)



# Skapa en multipel modell charges vs age + smoker + bmi_category + chronic_condition ---- 

# Ändra referensnivå för BMI-kategori till "normal"

data_clean$bmi_category <- relevel(data_clean$bmi_category, ref = "normal")

model_multi <- lm(charges ~ age + smoker + bmi_category + chronic_condition, data = data_clean)

summary(model_multi)



# Skapa en "extrem" multipel modell charges vs age + smoker + bmi_category + chronic_condition + prior_events ----

model_extreme <- lm(charges ~ age + smoker + bmi_category + chronic_condition + prior_events, data_clean)

summary(model_extreme)



# Skapa en modell med en interaktionsterm (smoker x bmi_category)

model_interaction <- lm(charges ~ age + smoker * bmi_category + chronic_condition + prior_events, data = data_clean)

summary(model_interaction)



# Sammanfattning ----

# Skapa en snygg jämförande tabell

# "Layout inställningar"

gof_template <- list(
  list("raw" = "nobs", "clean" = "Antal obs.", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "R2", "fmt" = 3),
  list("raw" = "adj.r.squared", "clean" = "R2 just.", "fmt" = 3),
  list("raw" = "rmse", "clean" = "Resid. std. fel", "fmt" = 2),
  list("raw" = "statistic", "clean" = "F-statistik", "fmt" = 1),
  list("raw" = "p.value", "clean" = "F (p-värde)", "fmt" = 3)
)

# Lista över modellerna

models <- list(
  "Modell 1: Baseline" = model_baseline,
  "Modell 2: Multipel" = model_multi,
  "Modell 3: Multipel + prior_events" = model_extreme,
  "Modell 4: Interaktion" = model_interaction
)

# Tabellen

modelsummary(models,
             output = "output/regression/summary_table.png",
             fmt = 2,
             stars = TRUE,
             gof_map = gof_template,
             title = "Jämförelse: Från enkel till komplex modell")



# Utvärdering ----

# Prediktioner och residualer

pred_resid_data <- bind_rows(
  data_clean %>%
    mutate(predicted = predict(model_multi),
           residual = charges - predicted,
           model = "Modell 2: Multipel"),
  data_clean %>%
    mutate(predicted = predict(model_interaction),
           residual = charges - predicted,
           model = "Modell 4: Interaktion")
)

# Residualer vs predikterade värden 

plot_resid_pred <- ggplot(pred_resid_data, aes(x = predicted, y = residual)) +
  geom_point(alpha = 0.3, color = "steelblue") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  facet_wrap(~ model, scales = "free_x") +
  theme_minimal() +
  labs(title = "Residualanalys",
       x = "Modellens gissning (Predicted)",
       y = "Felmarginal (Residual)")

plot_resid_pred
ggsave("output/regression/plot_resid_pred.png", plot_resid_pred, width = 10, height = 5)


# Faktiskt vs predikterat pris 

plot_actual_charge <- ggplot(pred_resid_data, aes(x = predicted, y = charges)) +
  geom_point(alpha = 0.3, color = "steelblue") + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ model) +
  theme_minimal() + 
  labs(title = "Faktiskt vs Predikterat pris",
       x = "Modellens gissning (Predicted)",
       y = "Faktiskt kostnad (Actual)")

plot_actual_charge
ggsave("output/regression/plot_actual_charge.png", plot_actual_charge, width = 10, height = 5)


# QQ-plot 

plot_qq <- ggplot(pred_resid_data, aes(sample = residual)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ model) +
  theme_minimal() +
  labs(title = "QQ-plot: normalfördelning av fel")

plot_qq
ggsave("output/regression/plot_qq.png", plot_qq, width = 10, height = 5)





















