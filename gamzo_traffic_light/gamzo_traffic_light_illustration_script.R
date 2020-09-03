# gamzo traffic light dynamics illustration

library(tidyverse)
town_data_raw <- readxl::read_excel("gamzo_traffic_light/geographic-summary-per-day-2020-09-02.xlsx")

town_data <- town_data_raw %>% 
  filter(town_code != "0000") %>% 
  mutate_all(~ if_else(. == "<15", NA_character_, .)) %>% 
  select(-starts_with("new_")) %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  mutate(across(c(accumulated_tested:accumulated_deaths), .fns = as.numeric)) %>% 
  filter(!is.na(accumulated_cases)) %>% 
  group_by(town, date) %>% 
  summarize(across(accumulated_tested:accumulated_deaths, sum)) %>% 
  # complete(
  #   tibble(date = seq.Date(from = lubridate::as_date("2020-04-01"), 
  #                          to = lubridate::as_date("2020-09-01"), by = 1))
  # ) %>% 
  arrange(town, date) %>% 
  group_by(town) %>% 
  mutate(across(accumulated_tested:accumulated_deaths,
                ~lag(., n = 7),
                .names = "lag7_{col}"))

# town population from CBS ----

town_pop <- read_csv("gamzo_traffic_light/population_madaf_2019_4.csv",
                     locale = locale(encoding = "ISO-8859-8"))

# constants as defined in the formula ----

k <- 2
m <- 8

# tibble for score computations ----

town_for_formula <- town_data %>% 
  select(town, date, accumulated_tested, accumulated_cases, 
         lag7_accumulated_tested, lag7_accumulated_cases) %>% 
  left_join(town_pop) %>% 
  mutate(P = (accumulated_cases-lag7_accumulated_cases) / 
           (accumulated_tested - lag7_accumulated_tested)) %>% 
  mutate(N = (accumulated_cases - lag7_accumulated_cases)/n_2019*10000) %>% 
  mutate(lag7_N = lag(N, n = 7)) %>% 
  mutate(G = N/lag7_N) %>% 
  mutate(score = k + log(N*G^2) + P/m) %>%
  filter(is.finite(score)) %>% 
  mutate(score = if_else(score>10, 10,
                         if_else(score < 0, 0, score)))

# filter just a few towns ----

write_csv(town_for_formula, "gamzo_traffic_light/selected_towns_processed.csv")
