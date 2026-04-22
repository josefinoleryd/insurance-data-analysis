# Ladda in paket ----

library(tidyverse)
library(naniar)

# Ladda in datan ----

data_raw <- read_csv("data/insurance_costs.csv")

# Datasetet storlek och struktur ----

View(data_raw)
dim(data_raw)
glimpse(data_raw)
str(data_raw)

# Dubletter ----

# Identiska rader

data_raw %>%
  duplicated() %>%
  sum()

# Förekommer samma customer_id mer än en gång?

data_raw %>%
  count(customer_id) %>%
  filter(n > 1)

# Saknade värden, antal och andel ----

data_raw %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything()) %>%
  mutate(andel = value / nrow(data_raw) * 100) %>%
  arrange(desc(andel))

# Undersöka om det finns fler NA än 1 per observation

gg_miss_upset(data_raw)

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

# Fördelning i datat, kategoriska variabler ----

data_raw %>%
  select(where(is.character), -customer_id) %>%
  pivot_longer(everything()) %>%
  count(name, value) %>%
  group_by(name) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  print(n = Inf)

# Fördelning och outliers i datat, numeriska variabler ----

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

