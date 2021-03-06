# The script analyzes the lab tests turnover

# The script analyzes turnover of Corona lab tests
# Retrieved from 

library(tidyverse)

theme_set(saridr::theme_sarid())

turnover_raw <- readxl::read_excel("data/corona_lab_tests_ver003.xlsx")

tests_per_day <- turnover_raw %>% 
  group_by(is_first_test) %>% 
  count(result_date) %>% 
  mutate(test_type = case_when(is_first_test == "No" ~ "Repeated",
                               is_first_test == "Yes" ~ "First"))

ggplot(tests_per_day, aes(fill = test_type, x = result_date, y = n)) + 
  geom_col() + 
  guides(fill = guide_legend("First/repeated test")) + 
  ggtitle("Number of Corona PCR tests in Israel") +
  scale_y_continuous(breaks = seq(0, 12000, by = 2000)) +
  xlab("Date") + ylab("Number of tests performed") +
  labs(caption = "Data source: MOH open data @ https://data.gov.il/dataset/covid-19\nGenerated by Sarid Research Institute: https://www.sarid-ins.co.il") + 
  coord_cartesian(ylim = c(0, 12500))

