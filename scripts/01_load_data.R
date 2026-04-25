# ---- s1-ladda-in-paket

library(tidyverse)
library(naniar)

# ---- s1-ladda-data

data_raw <- read_csv("data/insurance_costs.csv", show_col_types = FALSE)

# ---- s1-storlek-och-struktur

glimpse(data_raw)

# ---- s1-dubbletter

# ---- s1-identiska-rader

data_raw %>%
  duplicated() %>%
  sum()

# ---- s1-förekommer-samma-kund-flera-gånger?

data_raw %>%
  count(customer_id) %>%
  filter(n > 1)

# ---- s1-identiska-rader-bortser-customer_id

data_raw %>%
  select(-customer_id) %>%
  duplicated() %>%
  sum()

# ---- s1-saknade-värden

data_raw %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything()) %>%
  mutate(andel = value / nrow(data_raw) * 100) %>%
  arrange(desc(andel))

# ---- s1-fler-NA-per-observation

gg_miss_upset(data_raw)

# ---- s1-spara-ner-plotten-som-png

png(
  filename = "output/eda/more_NA_per_obs.png",
  width = 1200, 
  height = 800,
  res = 150
)

gg_miss_upset(data_raw)

dev.off()

# ---- s1-inkonsekvenser-kategoriska-variabler

data_raw %>%
  select(where(is.character), -customer_id) %>%
  pivot_longer(everything()) %>%
  distinct(name, value) %>%
  group_by(name) %>%
  summarise(
    kategorier = paste(value, collapse = ", ")
  )

# ---- s1-inkonsekvenser-numeriska-variabler

data_raw %>%
  select(where(is.numeric)) %>%
  summary()

# ---- s1-fördelning-kategoriska-variabler

data_raw %>%
  select(where(is.character), -customer_id) %>%
  pivot_longer(everything()) %>%
  count(name, value) %>%
  group_by(name) %>%
  mutate(andel = n / sum(n) * 100) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  print(n = Inf)

# ---- s1-fördelning-och-outliers-numeriska-variabler

# ---- s1-histogram

hist_plot_eda <- data_raw %>%
  select(age, bmi, charges) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") + 
  facet_wrap(~name, scales = "free") +
  labs(
    title = "Fördelning av age, bmi och charges",
    x = "Värde",
    y = "Antal"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# ---- s1-visa-histogram

hist_plot_eda

# ---- s1-spara-plotten

ggsave("output/eda/hist_plot_eda.png", plot = hist_plot_eda, width = 6, height = 4)

# ---- s1-boxplot

box_plot_eda <- data_raw %>%
  select(age, bmi, charges) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(y = value)) +
  geom_boxplot(fill = "lightblue", color = "black") + 
  facet_wrap(~name, scales = "free") +
  labs(
    title = "Boxplots för age, bmi och charges",
    x = NULL,
    y = "Värde"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
  )

# ---- s1-visa-boxplot

box_plot_eda

# ---- s1-spara plotten

ggsave("output/eda/box_plot_eda.png", plot = box_plot_eda, width = 6, height = 4)

