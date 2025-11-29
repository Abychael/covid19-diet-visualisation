
library(tidyverse)


df_food <- read_csv("C:/Users/Abby Bett/Downloads/archive/Food_Supply_Quantity_kg_Data.csv")


glimpse(df_food)


sum(is.na(df))
colSums(is.na(df))

df_clean <- df %>%
  mutate(across(where(is.numeric),
                ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))

df_clean <- df_clean %>%
  mutate(fruit_veg = Fruits + Vegetables,
         cases_per_million = (TotalCases / Population) * 1e6,
         log_cases_pm = log1p(cases_per_million))



p1 <- ggplot(df_clean, aes(fruit_veg, log_cases_pm)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Fruit & Vegetable Supply vs log(COVID-19 Cases per Million)",
    x = "Fruit & Vegetable Supply (kg/person/year)",
    y = "log(1 + COVID-19 Cases/1M population)",
    caption = "Source: Kaggle COVID-19 Healthy Diet Dataset"
  ) +
  theme_minimal()

ggsave("plot_scatter_fv_vs_cases.png", p1, dpi = 300)


p2 <- ggplot(df_clean, aes(fruit_veg)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Fruit & Vegetable Supply",
    x = "Fruit & Vegetable Supply (kg/person/year)",
    y = "Number of Countries",
    caption = "Source: Kaggle Dataset"
  ) +
  theme_minimal()

ggsave("plot_hist_fruitveg.png", p2, dpi = 300)


df_clean <- df_clean %>% mutate(fv_group = ntile(fruit_veg, 3))

p3 <- df_clean %>%
  group_by(fv_group) %>%
  summarise(mean_cases_pm = mean(cases_per_million)) %>%
  ggplot(aes(x = factor(fv_group), y = mean_cases_pm)) +
  geom_col() +
  labs(
    title = "COVID-19 Cases per Million by Diet Tertile",
    x = "Fruit & Vegetable Supply Group (Low → High)",
    y = "Mean COVID-19 Cases/1M",
    caption = "Error bars removed for accessibility"
  ) +
  theme_minimal()

ggsave("plot_bar_cases_by_group.png", p3, dpi = 300)
