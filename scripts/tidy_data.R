library(tidyverse)

# By Federal State --------------------------------------------------------

## 2016
states_days_2016 <- readxl::read_excel("data/raw/sonderauswertung-sterbefaelle.xlsx", 
                                       sheet = "BL_2016_Tage", 
                                       skip = 10, 
                                       n_max = 16, 
                                       col_names = FALSE)

states_days_2016_clean <- states_days_2016 %>%
  select(-`...368`) %>%
  pivot_longer(cols = 2:ncol(.), values_to = "n.deaths") %>%
  rename(federal_state = `...1`) %>%
  select(-name) %>%
  group_by(federal_state) %>%
  mutate(date = seq(lubridate::ymd("2016-01-01"), lubridate::ymd("2016-12-31"), by = "1 day"),
         week = lubridate::week(date)) %>%
  ungroup()

## 2017
states_days_2017 <- readxl::read_excel("data/raw/sonderauswertung-sterbefaelle.xlsx", 
                                       sheet = "BL_2017_Tage", 
                                       skip = 10, 
                                       n_max = 16, 
                                       col_names = FALSE)

states_days_2017_clean <- states_days_2017 %>%
  select(-`...367`) %>%
  pivot_longer(cols = 2:ncol(.), values_to = "n.deaths") %>%
  rename(federal_state = `...1`) %>%
  select(-name) %>%
  group_by(federal_state) %>%
  mutate(date = seq(lubridate::ymd("2017-01-01"), lubridate::ymd("2017-12-31"), by = "1 day"),
         week = lubridate::week(date)) %>%
  ungroup()

## 2018
states_days_2018 <- readxl::read_excel("data/raw/sonderauswertung-sterbefaelle.xlsx", 
                                       sheet = "BL_2018_Tage", 
                                       skip = 10, 
                                       n_max = 16, 
                                       col_names = FALSE)

states_days_2018_clean <- states_days_2018 %>%
  select(-`...367`) %>%
  pivot_longer(cols = 2:ncol(.), values_to = "n.deaths") %>%
  rename(federal_state = `...1`) %>%
  select(-name) %>%
  group_by(federal_state) %>%
  mutate(date = seq(lubridate::ymd("2018-01-01"), lubridate::ymd("2018-12-31"), by = "1 day"),
         week = lubridate::week(date)) %>%
  ungroup()

## 2019
states_days_2019 <- readxl::read_excel("data/raw/sonderauswertung-sterbefaelle.xlsx", 
                                       sheet = "BL_2019_Tage", 
                                       skip = 10, 
                                       n_max = 16, 
                                       col_names = FALSE)

states_days_2019_clean <- states_days_2019 %>%
  select(-`...367`) %>%
  pivot_longer(cols = 2:ncol(.), values_to = "n.deaths") %>%
  rename(federal_state = `...1`) %>%
  select(-name) %>%
  group_by(federal_state) %>%
  mutate(date = seq(lubridate::ymd("2019-01-01"), lubridate::ymd("2019-12-31"), by = "1 day"),
         week = lubridate::week(date)) %>%
  ungroup()

## 2020
states_days_2020 <- readxl::read_excel("data/raw/sonderauswertung-sterbefaelle.xlsx", 
                                       sheet = "BL_2020_Tage", 
                                       skip = 10, 
                                       n_max = 16, 
                                       col_names = FALSE)

states_days_2020_clean <- states_days_2020 %>%
  pivot_longer(cols = 2:ncol(.), values_to = "n.deaths") %>%
  rename(federal_state = `...1`) %>%
  select(-name) %>%
  group_by(federal_state) %>%
  mutate(date = seq(lubridate::ymd("2020-01-01"), lubridate::ymd("2020-04-26"), by = "1 day"),
         week = lubridate::week(date)) %>%
  ungroup()

## first known cases, source: https://de.wikipedia.org/wiki/COVID-19-Pandemie_in_Deutschland
date_known_cases <- tibble(federal_state = c("Schleswig-Holstein", "Hamburg", "Niedersachsen", 
                                             "Bremen", "Nordrhein-Westfalen", "Hessen", 
                                             "Rheinland-Pfalz", "Baden-Württemberg", "Bayern", 
                                             "Saarland", "Berlin", "Brandenburg", 
                                             "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", 
                                             "Thüringen"),
                           date_first_infection = c("2020-02-27", "2020-02-27", "2020-02-29",
                                                    "2020-02-29", "2020-02-15", "2020-02-02",
                                                    "2020-02-26", "2020-02-25", "2020-01-28",
                                                    "2020-03-03", "2020-03-01", "2020-03-02",
                                                    "2020-03-04", "2020-03-02", "2020-03-08",
                                                    "2020-03-02")) %>%
  mutate(date_first_infection = lubridate::ymd(date_first_infection))

## combined
states_days_allyears <- states_days_2016_clean %>%
  bind_rows(states_days_2017_clean) %>%
  bind_rows(states_days_2018_clean) %>%
  bind_rows(states_days_2019_clean) %>%
  bind_rows(states_days_2020_clean) %>%
  mutate(year = forcats::as_factor(lubridate::year(date)),
         yday = lubridate::yday(date)) %>%
  left_join(date_known_cases)

states_days_summary <- states_days_allyears %>%
  filter(year != 2020) %>%
  group_by(federal_state, yday) %>%
  summarise(mean.n.deaths = mean(n.deaths),
            sd.n.deaths = sd(n.deaths),
            median.n.deaths = median(n.deaths),
            p25.n.deaths = quantile(n.deaths, probs = 0.25),
            p75.n.deaths = quantile(n.deaths, probs = 0.75)) %>%
  ungroup() %>%
  mutate(upper_bound.n.deaths = mean.n.deaths + sd.n.deaths / 2,
         lower_bound.n.deaths = mean.n.deaths - sd.n.deaths / 2,
         upper_bound.mean.excess_mortality = upper_bound.n.deaths / mean.n.deaths,
         lower_bound.mean.excess_mortality = lower_bound.n.deaths / mean.n.deaths,
         upper_bound.median.excess_mortality = p75.n.deaths / median.n.deaths,
         lower_bound.median.excess_mortality = p25.n.deaths / median.n.deaths)

states_days <- states_days_allyears %>%
  left_join(states_days_summary) %>%
  mutate(median.excess_mortality = n.deaths - median.n.deaths,
         mean.excess_mortality = n.deaths - mean.n.deaths,
         ratio.median.excess_mortality = n.deaths / median.n.deaths,
         ratio.mean.excess_mortality = n.deaths / mean.n.deaths) %>%
  group_by(federal_state) %>%
  mutate(rolling_mean.ratio.median.excess_mortality = zoo::rollmean(ratio.median.excess_mortality, 7, fill = NA, align = "right"),
         rolling_mean.ratio.mean.excess_mortality = zoo::rollmean(ratio.mean.excess_mortality, 7, fill = NA, align = "right")) %>%
  ungroup()

remove(states_days_2016, states_days_2016_clean, states_days_2017, states_days_2017_clean, 
       states_days_2018, states_days_2018_clean, states_days_2019, states_days_2019_clean,
       states_days_2020, states_days_2020_clean, states_days_allyears, states_days_summary)

write_csv(states_days, path = "data/tidy/mortality_byState.csv")
write_rds(states_days, path = "data/tidy/mortality_byState.rds")


# By Age Group ------------------------------------------------------------

## 2016
germany_ages_2016 <- readxl::read_excel("data/raw/sonderauswertung-sterbefaelle.xlsx",
                                        sheet = "D_2016_Tage",
                                        skip = 10,
                                        n_max = 12,
                                        col_names = FALSE)

germany_ages_2016_clean <- germany_ages_2016 %>%
  select(-`...368`) %>%
  pivot_longer(cols = 2:ncol(.), values_to = "n.deaths") %>%
  rename(age_category = `...1`) %>%
  mutate(age_category = forcats::as_factor(age_category)) %>%
  select(-name) %>%
  group_by(age_category) %>%
  mutate(date = seq(lubridate::ymd("2016-01-01"), lubridate::ymd("2016-12-31"), by = "1 day"),
         week = lubridate::week(date)) %>%
  ungroup()

## 2017
germany_ages_2017 <- readxl::read_excel("data/raw/sonderauswertung-sterbefaelle.xlsx",
                                        sheet = "D_2017_Tage",
                                        skip = 10,
                                        n_max = 12,
                                        col_names = FALSE)

germany_ages_2017_clean <- germany_ages_2017 %>%
  select(-`...367`, -`...368`) %>%
  pivot_longer(cols = 2:ncol(.), values_to = "n.deaths") %>%
  rename(age_category = `...1`) %>%
  mutate(age_category = forcats::as_factor(age_category)) %>%
  select(-name) %>%
  group_by(age_category) %>%
  mutate(date = seq(lubridate::ymd("2017-01-01"), lubridate::ymd("2017-12-31"), by = "1 day"),
         week = lubridate::week(date)) %>%
  ungroup()

## 2018
germany_ages_2018 <- readxl::read_excel("data/raw/sonderauswertung-sterbefaelle.xlsx",
                                        sheet = "D_2018_Tage",
                                        skip = 10,
                                        n_max = 12,
                                        col_names = FALSE)

germany_ages_2018_clean <- germany_ages_2018 %>%
  select(-`...367`, -`...368`) %>%
  pivot_longer(cols = 2:ncol(.), values_to = "n.deaths") %>%
  rename(age_category = `...1`) %>%
  mutate(age_category = forcats::as_factor(age_category)) %>%
  select(-name) %>%
  group_by(age_category) %>%
  mutate(date = seq(lubridate::ymd("2018-01-01"), lubridate::ymd("2018-12-31"), by = "1 day"),
         week = lubridate::week(date)) %>%
  ungroup()

## 2019
germany_ages_2019 <- readxl::read_excel("data/raw/sonderauswertung-sterbefaelle.xlsx",
                                        sheet = "D_2019_Tage",
                                        skip = 10,
                                        n_max = 12,
                                        col_names = FALSE)

germany_ages_2019_clean <- germany_ages_2019 %>%
  select(-`...367`) %>%
  pivot_longer(cols = 2:ncol(.), values_to = "n.deaths") %>%
  rename(age_category = `...1`) %>%
  mutate(age_category = forcats::as_factor(age_category)) %>%
  select(-name) %>%
  group_by(age_category) %>%
  mutate(date = seq(lubridate::ymd("2019-01-01"), lubridate::ymd("2019-12-31"), by = "1 day"),
         week = lubridate::week(date)) %>%
  ungroup()

## 2020
germany_ages_2020 <- readxl::read_excel("data/raw/sonderauswertung-sterbefaelle.xlsx",
                                        sheet = "D_2020_Tage",
                                        skip = 10,
                                        n_max = 12,
                                        col_names = FALSE)

germany_ages_2020_clean <- germany_ages_2020 %>%
  pivot_longer(cols = 2:ncol(.), values_to = "n.deaths") %>%
  rename(age_category = `...1`) %>%
  mutate(age_category = forcats::as_factor(age_category)) %>%
  select(-name) %>%
  group_by(age_category) %>%
  mutate(date = seq(lubridate::ymd("2020-01-01"), lubridate::ymd("2020-04-26"), by = "1 day"),
         week = lubridate::week(date)) %>%
  ungroup()

## combined

germany_ages_allyears <- germany_ages_2016_clean %>%
  bind_rows(germany_ages_2017_clean) %>%
  bind_rows(germany_ages_2018_clean) %>%
  bind_rows(germany_ages_2019_clean) %>%
  bind_rows(germany_ages_2020_clean) %>%
  mutate(year = forcats::as_factor(lubridate::year(date)),
         yday = lubridate::yday(date))

germany_ages_summary <- germany_ages_allyears %>%
  group_by(date) %>%
  mutate(proportion = n.deaths / sum(n.deaths)) %>%
  ungroup() %>%
  filter(year != 2020) %>%
  group_by(age_category, yday) %>%
  summarise(mean.n.deaths = mean(n.deaths),
            sd.mean.n.deaths = sd(n.deaths),
            median.n.deaths = median(n.deaths),
            p25.n.deaths = quantile(n.deaths, probs = 0.25),
            p75.n.deaths = quantile(n.deaths, probs = 0.75),
            mean.proportion = mean(proportion),
            median.proportion = median(proportion),
            p25.proportion = quantile(proportion, probs = 0.25),
            p75.proportion = quantile(proportion, probs = 0.75)) %>%
  ungroup() %>%
  mutate(upper_bound.mean.deaths = mean.n.deaths + sd.mean.n.deaths / 2,
         lower_bound.mean.deaths = mean.n.deaths - sd.mean.n.deaths / 2,
         upper_bound.mean.excess_mortality = upper_bound.mean.deaths / mean.n.deaths,
         lower_bound.mean.excess_mortality = lower_bound.mean.deaths / mean.n.deaths,
         upper_bound.median.excess_mortality = p75.n.deaths / median.n.deaths,
         lower_bound.median.excess_mortality = p25.n.deaths / median.n.deaths)

germany_ages <- germany_ages_allyears %>%
  left_join(germany_ages_summary) %>%
  mutate(median.excess_mortality = n.deaths - median.n.deaths,
         mean.excess_mortality = n.deaths - mean.n.deaths,
         ratio.median.excess_mortality = n.deaths / median.n.deaths,
         ratio.mean.excess_mortality = n.deaths / mean.n.deaths)  %>%
  group_by(age_category) %>%
  mutate(rolling_mean.ratio.median.excess_mortality = zoo::rollmean(ratio.median.excess_mortality, 7, fill = NA, align = "right"),
         rolling_mean.ratio.mean.excess_mortality = zoo::rollmean(ratio.mean.excess_mortality, 7, fill = NA, align = "right")) %>%
  ungroup()

remove(germany_ages_2016, germany_ages_2016_clean, germany_ages_2017, germany_ages_2017_clean,
       germany_ages_2018, germany_ages_2018_clean, germany_ages_2019, germany_ages_2019_clean,
       germany_ages_2020, germany_ages_2020_clean, germany_ages_allyears, germany_ages_summary)

write_csv(germany_ages, "data/tidy/mortality_byAge.csv")
write_rds(germany_ages, "data/tidy/mortality_byAge.rds")
