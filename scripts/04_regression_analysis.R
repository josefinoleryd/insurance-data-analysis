# ---- s4-ladda-in-paket-och-source

source("scripts/03_descriptive_analysis.R")
library(tidyverse)
library(modelsummary)
library(webshot2)
library(broom)

# ---- s4-scatterplots-intressanta-kontinuerliga-variabler

# ---- s4-age

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

# ---- s4-visa-plotten-age 

scatter_age

# ---- s4-spara-plotten-age

ggsave("output/regression/scatter_age.png", plot = scatter_age, width = 7, height = 5)

# ---- s4-bmi

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

# ---- s4-visa-plotten-bmi

scatter_bmi

# ---- s4-spara-plotten-bmi

ggsave("output/regression/scatter_bmi.png", plot = scatter_bmi, width = 7, height = 5)




# ---- s4-enkel-regressionsmodell-charges-vs-risk_score

model_baseline <- lm(charges ~ risk_score, data = data_clean)

summary(model_baseline)




# ---- s4-multipel-modell-prior_events-risk_score


model_multi <- lm(charges ~ prior_events + risk_score, data = data_clean)

# ---- s4-summary-multipel-modell-prior_events-risk_score

summary(model_multi)




# ---- s4-multipel-modell-prior_events-smoker-bmi_category-chronic_condition

# Ändra referensnivå för BMI-kategori till "normal"

data_clean$bmi_category <- relevel(data_clean$bmi_category, ref = "normal")

model_without_risk_score <- lm(charges ~ prior_events + smoker + bmi_category + chronic_condition, data = data_clean)

# ---- s4-summary-model-without-risk-score

summary(model_without_risk_score)




# ---- s4-"extrem"-multipel-modell-alla-variabler

model_extreme <- lm(charges ~ age_category + smoker + bmi_category + chronic_condition + prior_events, data = data_clean)

summary(model_extreme)



# ---- s4-sammanfattning

# Skapa en snygg jämförande tabell

# "Layout inställningar"

gof_template <- list(
  list("raw" = "nobs", "clean" = "Antal obs.", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "R2", "fmt" = 3),
  list("raw" = "adj.r.squared", "clean" = "R2 just.", "fmt" = 3),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 2),
  list("raw" = "bic", "clean" = "BIC", "fmt" = 2),
  list("raw" = "rmse", "clean" = "Resid. std. fel", "fmt" = 2),
  list("raw" = "statistic", "clean" = "F-statistik", "fmt" = 1),
  list("raw" = "p.value", "clean" = "F (p-värde)", "fmt" = 3)
)

# Lista över modellerna

models <- list(
  "Modell 1: Baseline" = model_baseline,
  "Modell 2: Multipel" = model_multi,
  "Modell 3: Orginalvariabler istället för risk_score" = model_without_risk_score,
  "Modell 4: Alla intressanta variabler" = model_extreme
)

# Tabellen

modelsummary(models,
             output = "output/regression/summary_table.png",
             fmt = 2,
             stars = TRUE,
             gof_map = gof_template,
             title = "Jämförelse: Från enkel till komplex modell")

knitr::include_graphics("output/regression/summary_table.png")

# ---- s4-utvärdering

# ---- s4-prediktioner-och-residualer

pred_resid_data <- bind_rows(
  data_clean %>%
    mutate(predicted = predict(model_multi),
           residual = charges - predicted,
           model = "Modell 2: Multipel"),
  data_clean %>%
    mutate(predicted = predict(model_extreme),
           residual = charges - predicted,
           model = "Modell 4: Max")
)

# ---- s4-residualer-vs-predikterade värden 

plot_resid_pred <- ggplot(pred_resid_data, aes(x = predicted, y = residual)) +
  geom_point(alpha = 0.3, color = "steelblue") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  facet_wrap(~ model, scales = "free_x") +
  theme_minimal() +
  labs(title = "Residualanalys",
       x = "Modellens gissning (Predicted)",
       y = "Felmarginal (Residual)")

# ---- s4-visa-resid-pred-plot

plot_resid_pred

# ---- s4-spara-resid-pred-plot

ggsave("output/regression/plot_resid_pred.png", plot_resid_pred, width = 10, height = 5)


# ---- s4-faktiskt-vs-predikterat pris 

plot_actual_charge <- ggplot(pred_resid_data, aes(x = predicted, y = charges)) +
  geom_point(alpha = 0.3, color = "steelblue") + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ model) +
  theme_minimal() + 
  labs(title = "Faktiskt vs Predikterat pris",
       x = "Modellens gissning (Predicted)",
       y = "Faktiskt kostnad (Actual)")

# ---- s4-visa-acutal-charge-plot

plot_actual_charge

# ---- s4-spara-actual-charge-plot

ggsave("output/regression/plot_actual_charge.png", plot_actual_charge, width = 10, height = 5)


# ---- s4-QQ-plot 

plot_qq <- ggplot(pred_resid_data, aes(sample = residual)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ model) +
  theme_minimal() +
  labs(title = "QQ-plot: normalfördelning av fel")

# ---- s4-visa-qq-plot

plot_qq

# ---- s4-spara-qq-plot

ggsave("output/regression/plot_qq.png", plot_qq, width = 10, height = 5)





















