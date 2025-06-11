# PA Covid cases & deaths dataset
# Lily Fransen

library(tidyverse)
covid_cases_deaths <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/covid_cases_deaths.csv")
theme_set(theme_light())

# Q1: Is population size associated with higher/lower per-capita COVID-19 case/death rates?

covid_cases_deaths |> 
  filter(date == latest_date) |> 
  arrange(desc(cases_cume_rate)) |> 
  slice(1:10) |> 
  ggplot(aes(x = fct_reorder(county, cases_cume_rate), y = cases_cume_rate)) +
  geom_col(fill = "lightblue") +
  coord_flip() +
  labs(title = "Top 10 Counties by avg case rate",
       y = "Cases per 100k", x = "County") +
  theme_minimal()

covid_cases_deaths |> 
  filter(date == latest_date) |> 
  arrange(desc(deaths_cume_rate)) |> 
  slice(1:10) |> 
  ggplot(aes(x = fct_reorder(county, deaths_cume_rate), y = deaths_cume_rate)) +
  geom_col(fill = "lightblue") +
  coord_flip() +
  labs(title = "Top 10 Counties by avg death rate",
       y = "Deaths per 100k", x = "County") +
  theme_minimal()

covid_cases_deaths |> 
  filter(date == last_date) |> 
  select(county, population, cases_cume_rate) |> 
  ggplot(aes(x = population, y = cases_cume_rate)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "pink") +
  labs(
    title = "Do More Populous Counties Have Higher COVID-19 Case Rates?",
    x = "Population",
    y = "Cumulative COVID-19 case rate per 100k") 

covid_cases_deaths |> 
  filter(date == last_date) |> 
  select(county, population, deaths_cume_rate) |> 
  ggplot(aes(x = population, y = deaths_cume_rate)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "pink") +
  labs(
    title = "Do More Populous Counties Have Higher COVID-19 Death Rates?",
    x = "Population",
    y = "Cumulative COVID-19 death rate per 100k") 

# Q2: Does case rate correspond with death rate?

covid_cases_deaths |> 
  filter(date == max(date, na.rm = TRUE)) |> 
  slice_max(order_by = population, n = 5) |> 
  ggplot(aes(x = population, y = cases_cume, size = deaths_cume_rate)) +
  geom_point(color = "pink", alpha = 0.6) +
  geom_text_repel(aes(label = county), size = 4) +
  scale_size_continuous(name = "Deaths per 100k") +
  labs(title = "Top 5 Most Populated PA Counties: COVID-19 Cases vs Population",
       x = "Population",
       y = "Cumulative Cases") +
  theme_minimal()

covid_cases_deaths |> 
  filter(date == last_date) |> 
  slice_head(n = 25) |> 
  select(county, cases_cume, deaths_cume) |> 
  pivot_longer(
    cols = c(cases_cume, deaths_cume),
    names_to = "metric",
    values_to = "count") |> 
  mutate(
    metric = recode(metric,
    cases_cume = "Cases",
    deaths_cume = "Deaths")) |>
  ggplot(aes(x = reorder(county, -count), y = count, fill = metric)) +
  geom_col(
    position = position_dodge(width = 0.8), 
    width = 0.7) +  
  scale_fill_manual(values = c("Deaths" = "red", "Cases" = "lightblue")) + 
  geom_text(
    data = function(d) d |> filter(metric == "Deaths"), 
    aes(label = count), 
    position = position_dodge(width = 0.8),
    hjust = -0.1,
    size = 3) +
  coord_flip() +
  labs(
    title = "Cumulative COVID-19 Cases and Deaths in 25 PA Counties",
    subtitle = paste("Data as of", last_date),
    x = "County",
    y = "Cumulative Counts",
    fill = "Metric") 

# Cluster analysis

# ***K-means try #1***
latest_data <- covid_cases_deaths |>
  filter(date == max(date, na.rm = TRUE))

clustering_data <- latest_data |>
  select(county, population, cases_cume, deaths_cume) |>
  drop_na()

scaled_data <- clustering_data |>
  select(-county) |>
  scale()

set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 3)

clustering_data <- clustering_data |>
  mutate(cluster = factor(kmeans_result$cluster))

ggplot(clustering_data, aes(x = cases_cume, y = deaths_cume, color = cluster)) +
  geom_point(alpha = 0.5, size = 2) +
  labs(title = "K-means PA County Clustering",
       x = "Cumulative cases", y = "Cumulative deaths") +
  theme_minimal()

# ***K-means try #2***

county_level <- covid_cases_deaths |>
  group_by(county) |>
  summarize(
    cases_cume_rate = max(cases_cume_rate, na.rm = TRUE),
    deaths_cume_rate = max(deaths_cume_rate, na.rm = TRUE)
  )

county_scaled <- county_level |>
  select(-county) |>
  scale()

kmeans_result <- kmeans(county_scaled, centers = 4, nstart = 25)
county_level$cluster <- as.factor(kmeans_result$cluster)

ggplot(county_level, aes(x = cases_cume_rate, y = deaths_cume_rate, color = cluster)) +
  geom_point(alpha = 0.5, size = 2) +
  stat_ellipse(level = 0.95, linetype = "dashed") +  
  ggthemes::scale_color_colorblind() + 
  ggrepel::geom_text_repel(aes(label = county), size = 2, max.overlaps = 50) +
  labs(
    title = "K-means PA County Clustering",
    x = "Cumulative case rate",
    y = "Cumulative death rate"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

table(county_level$cluster)

# Dataset "overview" checks

glimpse(covid_cases_deaths) 

covid_cases_deaths |> 
  distinct(county) |> 
  count()

range(covid_cases_deaths$date)

dim(covid_cases_deaths)









