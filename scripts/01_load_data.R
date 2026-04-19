# Ladda in paket ----
library(tidyverse)

# Ladda in datan ----
data_raw <- read_csv("data/insurance_costs.csv")

# Datasetet storlek och struktur ----
View(data_raw)
dim(data_raw)
glimpse(data_raw)
str(data_raw)

# Saknade värden, antal och andel ----

data_raw %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything()) %>%
  mutate(andel = value / nrow(data_raw) * 100) %>%
  arrange(desc(andel))

# Inkonsekvenser i datat, kategoriska variabler ----

data_raw %>%
  select(where(is.character), -customer_id) %>%
  pivot_longer(everything()) %>%
  count(name, value) %>%
  group_by(name) %>%
  summarise(
    kategorier = paste(value, collapse = ", ")
  )

# Inkonsekvenser i datat, numeriska variabler ----
data_raw %>%
  select(where(is.numeric)) %>%
  summary()

# Fördelning och outliers i datat ----
# Histogram
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

# Visa och spara plotten
hist_plot_eda

ggsave("output/eda/hist_plot_eda.png", plot = hist_plot_eda, width = 6, height = 4)

# Boxplot
box_plot_eda <- data_raw %>%
  select(age, bmi, charges) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(y = value)) +
  geom_boxplot(fill = "lightblue", color = "black") + 
  facet_wrap(~name, scales = "free") +
  labs(
    title = "Boxplots för age, bmi och charges",
    x = NULL,
    y = "Antal"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Visa och spara plotten
box_plot_eda

ggsave("output/eda/box_plot_eda.png", plot = box_plot_eda, width = 6, height = 4)





