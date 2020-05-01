library(tidyverse)
library(hrbrthemes)

# data
## 2016
states_days_2016 <- readxl::read_excel("data/destatis/sonderauswertung-sterbefaelle.xlsx", 
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
states_days_2017 <- readxl::read_excel("data/destatis/sonderauswertung-sterbefaelle.xlsx", 
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
states_days_2018 <- readxl::read_excel("data/destatis/sonderauswertung-sterbefaelle.xlsx", 
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
states_days_2019 <- readxl::read_excel("data/destatis/sonderauswertung-sterbefaelle.xlsx", 
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
states_days_2020 <- readxl::read_excel("data/destatis/sonderauswertung-sterbefaelle.xlsx", 
                                       sheet = "BL_2020_Tage", 
                                       skip = 10, 
                                       n_max = 16, 
                                       col_names = FALSE)

states_days_2020_clean <- states_days_2020 %>%
  pivot_longer(cols = 2:ncol(.), values_to = "n.deaths") %>%
  rename(federal_state = `...1`) %>%
  select(-name) %>%
  group_by(federal_state) %>%
  mutate(date = seq(lubridate::ymd("2020-01-01"), lubridate::ymd("2020-04-05"), by = "1 day"),
         week = lubridate::week(date)) %>%
  ungroup()

## first known cases
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
         lower_bound.mean.excess_mortality = lower_bound.n.deaths / mean.n.deaths)

states_days <- states_days_allyears %>%
  left_join(states_days_summary) %>%
  mutate(median.excess_mortality = n.deaths - median.n.deaths,
         mean.excess_mortality = n.deaths - mean.n.deaths,
         ratio.median.excess_mortality = n.deaths / median.n.deaths,
         ratio.mean.excess_mortality = n.deaths / mean.n.deaths)

states_weeks_allyears <- states_days_allyears %>%
  group_by(federal_state, week, year) %>%
  summarise(n.deaths = sum(n.deaths))

states_weeks_summary <- states_weeks_allyears %>%
  filter(year != 2020) %>%
  group_by(federal_state, week) %>%
  summarise(median.n.deaths = median(n.deaths),
            mean.n.deaths = mean(n.deaths))

remove(states_days_2016, states_days_2016_clean, states_days_2017, states_days_2017_clean, 
       states_days_2018, states_days_2018_clean, states_days_2019, states_days_2019_clean,
       states_days_2020, states_days_2020_clean)

excess_deaths_byState <- states_days %>%
  filter(year == 2020) %>%
  group_by(federal_state) %>%
  summarise(median.excess_mortality = sum(median.excess_mortality))

excess_deaths_byState_february <- states_days %>%
  filter(date >= "2020-02-01") %>%
  group_by(federal_state) %>%
  summarise(median.excess_mortality = sum(median.excess_mortality))

# all of germany, age groups

## 2016
germany_ages_2016 <- readxl::read_excel("data/destatis/sonderauswertung-sterbefaelle.xlsx",
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
germany_ages_2017 <- readxl::read_excel("data/destatis/sonderauswertung-sterbefaelle.xlsx",
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
germany_ages_2018 <- readxl::read_excel("data/destatis/sonderauswertung-sterbefaelle.xlsx",
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
germany_ages_2019 <- readxl::read_excel("data/destatis/sonderauswertung-sterbefaelle.xlsx",
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
germany_ages_2020 <- readxl::read_excel("data/destatis/sonderauswertung-sterbefaelle.xlsx",
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
  mutate(date = seq(lubridate::ymd("2020-01-01"), lubridate::ymd("2020-04-05"), by = "1 day"),
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
         ratio.mean.excess_mortality = n.deaths / mean.n.deaths)

write_rds(germany_ages, "data/destatis/clean/deaths_germany_byAge.rds")

# graphs ------------------------------------------------------------------

## cumulative number of excess deaths by state from beginning of year
excess_deaths_byState %>%
  mutate(federal_state = fct_reorder(as_factor(federal_state), median.excess_mortality)) %>%
  ggplot(aes(x = federal_state,
             y = median.excess_mortality)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "steelblue") +
  coord_flip() +
  theme_ft_rc() +
  labs(title = "Cumulative Excess Deaths by Federal State",
       subtitle = "January 1 to April 5 2020, baseline: median of years 2016 to 2019, \ndata for 2019 and 2020 preliminary",
       x = NULL,
       y = "Cumulative Excess Deaths",
       caption = "Data: Destatis, Analysis: @tfleauxk")

## cumulative number of excess deaths by state from february 1
excess_deaths_byState_february %>%
  mutate(federal_state = fct_reorder(as_factor(federal_state), median.excess_mortality)) %>%
  ggplot(aes(x = federal_state,
             y = median.excess_mortality)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "steelblue") +
  coord_flip() +
  theme_ft_rc() +
  labs(title = "Cumulative Excess Deaths by Federal State",
       subtitle = "February 1 to April 5 2020, baseline: median of years 2016 to 2019, \ndata for 2019 and 2020 preliminary",
       x = NULL,
       y = "Cumulative Excess Deaths",
       caption = "Data: Destatis, Analysis: @tfleauxk")

states_weeks_allyears %>%
  ggplot(aes(x = week,
             y = n.deaths,
             color = year)) +
  geom_line() +
  facet_wrap(~ federal_state) +
  theme_ipsum_rc()

ggplot() +
  geom_point(data = states_weeks_allyears,
             mapping = aes(x = week,
                           y = n.deaths,
                           color = year)) +
  geom_line(data = states_weeks_summary,
            mapping = aes(x = week,
                          y = mean.n.deaths),
            color = "red") +
  facet_wrap(~ federal_state) +
  theme_ipsum_rc()

## only NRW

ggplot() +
  geom_point(data = subset(states_weeks_allyears, federal_state == "Nordrhein-Westfalen"),
             mapping = aes(x = week,
                           y = n.deaths,
                           color = year)) +
  geom_line(data = subset(states_weeks_summary, federal_state == "Nordrhein-Westfalen"),
            mapping = aes(x = week,
                          y = median.n.deaths),
            color = "red") +
  geom_vline(xintercept = 9) +
  theme_ipsum_rc() +
  labs(title = "Mortality from All Causes in Northrhine-Westphalia",
       subtitle = "2016-2020, data for 2019 and 2020 preliminary, weekly aggregates",
       x = "Week",
       y = "Number of deaths",
       color = NULL,
       caption = "Data: Destatis, Analysis: @florentinkraemer")

states_days %>%
  filter(federal_state == "Nordrhein-Westfalen") %>%
  ggplot() +
  geom_linerange(aes(x = yday,
                  ymin = p25.n.deaths,
                  ymax = p75.n.deaths),
              color = "light grey") +
  geom_smooth(data = subset(states_days, federal_state == "Nordrhein-Westfalen" & year == "2020"),
            mapping = aes(x = yday,
                          y = n.deaths)) +
  theme_ft_rc()

states_days %>%
  filter(federal_state == "Baden-Württemberg") %>%
  ggplot() +
  geom_ribbon(aes(x = yday,
                  ymin = lower_bound.n.deaths,
                  ymax = upper_bound.n.deaths)) +
  geom_smooth(data = subset(states_days, federal_state == "Baden-Württemberg" & year == "2020"),
              mapping = aes(x = yday,
                            y = n.deaths)) +
  theme_ft_rc()

states_days %>%
  ggplot() +
  geom_ribbon(aes(x = yday,
                  ymin = lower_bound.n.deaths,
                  ymax = upper_bound.n.deaths),
              color = "lightgrey",
              fill = "lightgrey") +
  geom_smooth(data = subset(states_days, year == "2020"),
              mapping = aes(x = yday,
                            y = n.deaths),
              se = FALSE) +
  facet_wrap(~ federal_state) +
  theme_ft_rc()

## Excess Mortality from All Causes by Federal State, Mean
states_days %>%
  ggplot() +
  geom_ribbon(aes(x = yday,
                  ymin = lower_bound.mean.excess_mortality,
                  ymax = upper_bound.mean.excess_mortality),
              color = "grey30",
              fill = "grey30") +
  geom_hline(yintercept = 1) +
  geom_smooth(data = subset(states_days, year == "2020"),
              mapping = aes(x = yday,
                            y = ratio.mean.excess_mortality,
                            color = year),
              se = TRUE) +
  facet_wrap(~ federal_state) +
  theme_ft_rc() +
  scale_y_continuous(breaks = c(0.7, 1, 1.3)) +
  labs(x = "Day of Year",
       y = "Ratio of 2020 deaths to mean 2016-2019 deaths",
       color = NULL,
       title = "Excess Mortality from All Causes by Federal State",
       subtitle = "Baseline: mean value of years 2016-2019, shaded grey ribbon: one standard deviation \ndata for 2019 and 2020 preliminary",
       caption = "Data: destatis, Analysis: @tfleauxk")

# excess mortality

states_days %>%
  filter(year == 2020) %>%
  mutate(federal_state = forcats::fct_reorder(forcats::as_factor(federal_state), date_first_infection)) %>%
  ggplot(aes(x = date,
             y = median.excess_mortality,
             color = year)) +
  geom_rect(aes(xmin = date_first_infection,
                xmax = max(date),
                ymin = -Inf,
                ymax = Inf),
            color = "light grey",
            fill = "light grey") +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(~ federal_state) +
  theme_ft_rc() +
  labs(title = "Excess Mortality from All Causes by German Federal States",
       subtitle = "Baseline: median value of years 2016-2019, data for 2019 and 2020 preliminary 
       \nshaded grey area refers to presence of infected persons",
       x = "Date",
       y = "Number of excess deaths",
       color = NULL,
       caption = "Data: Destatis, Analysis: @tfleauxk")

states_days %>%
  filter(year == 2020) %>%
  mutate(federal_state = forcats::fct_reorder(forcats::as_factor(federal_state), date_first_infection)) %>%
  ggplot(aes(x = date,
             y = percent.excess_mortality,
             color = year)) +
  geom_rect(aes(xmin = date_first_infection,
                xmax = max(date),
                ymin = -Inf,
                ymax = Inf),
            color = "light grey",
            fill = "light grey") +
  geom_line() +
  geom_hline(yintercept = 1) +
  facet_wrap(~ federal_state) +
  theme_ipsum_rc() +
  labs(title = "Excess Mortality from All Causes by German Federal States",
       subtitle = "Baseline: median value of years 2016-2019, data for 2019 and 2020 preliminary 
       \nshaded grey area refers to presence of infected persons",
       x = "Date",
       y = "Ratio of 2020 deaths to median 2016-2019 deaths",
       color = NULL,
       caption = "Data: Destatis, Analysis: @tfleauxk")

ggsave(filename = "excess_mortality_ratio.pdf", path = "plots/", device = cairo_pdf)

# by age categories

germany_ages <- read_rds("data/destatis/clean/deaths_germany_byAge.rds")

germany_ages_2020_clean %>%
  ggplot(aes(x = date,
             y = n.deaths,
             color = age_category)) +
  geom_line() +
  theme_ft_rc()

germany_ages %>%
  filter(year == 2020) %>%
  ggplot(aes(x = date,
             y = ratio.median.excess_mortality,
             color = year)) +
  geom_line() +
  geom_hline(yintercept = 1, color = "light grey") +
  facet_wrap(~ age_category) +
  theme_ft_rc() +
  labs(title = "Excess Mortality from All Causes by Age Categories",
       subtitle = "Baseline: median value of years 2016-2019, data for 2019 and 2020 preliminary \nshaded grey area refers to presence of infected persons",
       x = "Date",
       y = "Ratio of 2020 deaths to median 2016-2019 deaths",
       color = NULL,
       caption = "Data: Destatis, Analysis: @tfleauxk")

germany_ages %>%
  ggplot() +
  geom_ribbon(aes(x = yday,
                  ymin = lower_bound.mean.excess_mortality,
                  ymax = upper_bound.mean.excess_mortality),
              color = "grey30",
              fill = "grey30") +
  geom_smooth(aes(x = yday,
                  y = ratio.mean.excess_mortality,
                  color = year),
              data = subset(germany_ages, year == 2020)) +
  geom_hline(yintercept = 1, color = "grey70") +
  facet_wrap(~ age_category) +
  theme_ft_rc() +
  labs(title = "Excess Mortality from All Causes by Age Categories",
       subtitle = "Baseline: mean value of years 2016-2019, shaded grey ribbon: one standard deviation \ndata for 2019 and 2020 preliminary",
       x = "Date",
       y = "Ratio of 2020 deaths to mean 2016-2019 deaths",
       color = NULL,
       caption = "Data: Destatis, Analysis: Florentin Krämer")

ggsave(filename = "excess_mortality_age_mean.pdf", path = "plots/", 
       device = cairo_pdf, 
       width = 29.7, height = 21, units = "cm")

germany_ages %>%
  ggplot() +
  geom_ribbon(aes(x = yday,
                  ymin = lower_bound.median.excess_mortality,
                  ymax = upper_bound.median.excess_mortality),
              color = "grey30",
              fill = "grey30") +
  geom_smooth(aes(x = yday,
                  y = ratio.median.excess_mortality,
                  color = year),
              data = subset(germany_ages, year == 2020)) +
  geom_hline(yintercept = 1, color = "grey70") +
  facet_wrap(~ age_category) +
  theme_ft_rc() +
  labs(title = "Excess Mortality from All Causes by Age Categories",
       subtitle = "Relative to median value of years 2016-2019. Shaded grey ribbon: interquartile range. \nData for 2019 and 2020 preliminary. Details: https://tinyurl.com/yacmwu9d.",
       x = "Date",
       y = "Ratio of 2020 deaths to median 2016-2019 deaths",
       color = NULL,
       caption = "Data: destatis, Analysis: Florentin Krämer")

ggsave(filename = "excess_mortality_age_median.pdf", path = "plots/", 
       device = cairo_pdf, 
       width = 29.7, height = 21, units = "cm")

germany_ages_summary %>%
  group_by(age_category) %>%
  summarise(mean.proportion = mean(mean.proportion)) %>%
  ggplot(aes(x = age_category,
             y = mean.proportion)) +
    geom_col() +
    theme_ft_rc() +
  labs(title = "Distribution of Deaths by Age Category",
       subtitle = "Average over data from 2016-2019 (2019 preliminary)",
       x = NULL,
       y = NULL,
       caption = "Data: Destatis, Analysis: @tfleauxk")
