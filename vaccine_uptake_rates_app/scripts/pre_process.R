library(tidyverse)

# Prep data for use (will be cached) --------------------------------------
# source: https://data.gov.il/dataset/covid-19/resource/12c9045c-1bf4-478a-a9e1-1e876cc2e182
vacci_raw <- read_csv("data/2021-02-03 - vaccinated_city_table_ver_0013.csv")

# source: https://data.gov.il/dataset/residents_in_israel_by_communities_and_age_groups/resource/64edd0ee-3d5d-43ce-8562-c336c24dbc1f
cbs_data <- read_csv("data/2021-02-06 - population_by_town.csv",
                     locale = locale(encoding = "ISO-8859-8-I"), 
                     skip = 1,
                     col_names = c("city_code",
                                   "city_name",
                                   "district_symbol",
                                   "district_name",
                                   "mna_code",
                                   "mna_name",
                                   "regional_code",
                                   "regional_name",
                                   "total",
                                   "age_0_5",
                                   "age_6_18",
                                   "age_19_45",
                                   "age_46_55",
                                   "age_56_64",
                                   "age_65+")) %>% 
  mutate(adjusted_age_0_19 = age_0_5 + age_6_18 + age_19_45*1/27,
         adjusted_age_20_39 = age_19_45*20/27,
         adjusted_age_40_59 = age_19_45*6/27 + age_46_55 + age_56_64*4/9,
         adjusted_age_60_plus = age_56_64*5/9 + `age_65+`) %>% 
  select(-starts_with("age")) %>% 
  mutate(across(starts_with("adjusted"), round)) %>% 
  select(-(district_symbol:total), -city_name) %>% 
  pivot_longer(-city_code, 
               names_to = "age_group", values_to = "population") %>% 
  mutate(age_group = str_remove(age_group, "adjusted_age_")) %>% 
  mutate(age_group = str_replace(age_group, "_", "-")) %>% 
  mutate(age_group = str_replace(age_group, "-plus", "+"))

# Final table -------------------------------------------------------------

vacci <- vacci_raw %>% 
  rename(city_name = CityName,
         city_code = CityCode) %>% 
  pivot_longer(cols = contains("_dose_"),
               names_to = "vaccine_type", 
               values_to = "vaccinated") %>% 
  mutate(vaccinated = if_else(vaccinated == "<15", "0.0", vaccinated)) %>% 
  mutate(vaccinated = parse_number(vaccinated)) %>% 
  mutate(dose = case_when(str_detect(vaccine_type, "first") ~ 1,
                          str_detect(vaccine_type, "second") ~ 2,
                          TRUE ~ -99)) %>% 
  mutate(age_group = str_remove(vaccine_type, "first_dose_|second_dose_")) %>% 
  mutate(age_group = recode_factor(age_group,
                                   "20-29" = "20-39",
                                   "30-39" = "20-39",
                                   "40-49" = "40-59",
                                   "50-59" = "40-59",
                                   "60-69" = "60+",
                                   "70-79" = "60+",
                                   "80-89" = "60+",
                                   "90+" = "60+")) %>% 
  group_by(city_name, city_code, Date,
           dose, age_group) %>% 
  summarize(vaccinated = sum(vaccinated)) %>% 
  left_join(cbs_data)

write_csv(vacci, "data/vacci.csv")

# Example chart ----

chart_prep <- vacci %>% 
  filter(!is.na(population)) %>% 
  group_by(city_name, dose, Date, population, age_group) %>% 
  summarize(total_vaccinated = sum(vaccinated)) %>% 
  filter(age_group == "60+",
         city_name %in% c("תל אביב - יפו",
                          "בני ברק",
                          "נתניה",
                          "ירושלים",
                          "חיפה"))

comparison_chart <- chart_prep %>%
  filter(dose == 1) %>% 
  mutate(prop = total_vaccinated/population) %>% 
  ggplot(aes(x = Date, y = prop, color = city_name)) + 
  geom_line(size = 1) + 
  saridr::theme_sarid() + 
  ylab("Population [%]") + 
  ggtitle("\u202bחיסון מנה ראשונה של COVID19 בישראל\nרשויות נבחרות, גילאי 60 ומעלה") +
  scale_y_continuous(labels = scales::percent_format(1)) + 
  guides(color = guide_legend("עיר")) +
  labs(caption = "\u202bמקור הנתונים: מאגר COVID-19 של משרד הבריאות מתחסנים על פי יישוב\n\u202bויז'ואליזציה מכון שריד: https://www.sarid-ins.co.il") + 
  theme(plot.title = element_text(hjust = 1),
        plot.subtitle = element_text(hjust = 1))
