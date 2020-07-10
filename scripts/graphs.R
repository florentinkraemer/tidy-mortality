library(tidyverse)
library(hrbrthemes)
library(ggalt)

# By State ----------------------------------------------------------------

## Excess Mortality from All Causes by Federal State, Mean
states_days %>%
  ggplot() +
  geom_ribbon(aes(x = yday,
                  ymin = lower_bound.mean.excess_mortality,
                  ymax = upper_bound.mean.excess_mortality),
              color = "grey30",
              fill = "grey30") +
  geom_hline(yintercept = 1) +
  geom_line(data = subset(states_days, year == "2020"),
            mapping = aes(x = yday,
                          y = rolling_mean.ratio.mean.excess_mortality,
                          color = year)) +
  facet_wrap(~ federal_state) +
  theme_ft_rc() +
  scale_y_continuous(breaks = c(0.7, 1, 1.3)) +
  labs(x = "Day of Year",
       y = "7-day rolling mean of ratio of 2020 deaths to mean 2016-2019 deaths",
       color = NULL,
       title = "Excess Mortality from All Causes by Federal State",
       subtitle = "Relative to mean value of years 2016-2019. Shaded grey ribbon: two standard deviations. \nData for 2019 and 2020 preliminary. Details: https://tinyurl.com/yacmwu9d.",
       caption = "Data: Destatis, Analysis: Florentin Krämer")

ggsave(filename = "excess_mortality_state_mean.pdf", path = "graphs/", device = cairo_pdf, 
       width = 29.7, height = 21, units = "cm")

ggsave(filename = "excess_mortality_state_mean.png", path = "graphs/", type = "cairo-png", 
       width = 29.7, height = 21, units = "cm", dpi = 300)

## Excess Mortality from All Causes by Federal State, Median
states_days %>%
  ggplot() +
  geom_ribbon(aes(x = yday,
                  ymin = lower_bound.median.excess_mortality,
                  ymax = upper_bound.median.excess_mortality),
              color = "grey30",
              fill = "grey30") +
  geom_hline(yintercept = 1) +
  geom_line(data = subset(states_days, year == "2020"),
            mapping = aes(x = yday,
                          y = rolling_mean.ratio.median.excess_mortality,
                          color = year)) +
  facet_wrap(~ federal_state) +
  theme_ft_rc() +
  scale_y_continuous(breaks = c(0.7, 1, 1.3)) +
  labs(x = "Day of Year",
       y = "Ratio of 2020 deaths to median 2016-2019 deaths",
       color = NULL,
       title = "Excess Mortality from All Causes by Federal State",
       subtitle = "Relative to median value of years 2016-2019. Shaded grey ribbon: interquartile range. \nData for 2019 and 2020 preliminary. Details: https://tinyurl.com/yacmwu9d.",
       caption = "Data: Destatis, Analysis: Florentin Krämer")

ggsave(filename = "excess_mortality_state_median.pdf", path = "graphs/", device = cairo_pdf, 
       width = 29.7, height = 21, units = "cm")

ggsave(filename = "excess_mortality_state_median.png", path = "graphs/", type = "cairo-png", 
       width = 29.7, height = 21, units = "cm", dpi = 300)

## Cumulative Excess Deaths by Federal State, Median
states_days %>%
  filter(date >= "2020-03-01") %>%
  mutate(excess_deaths = case_when(n.deaths < p25.n.deaths ~ n.deaths - p25.n.deaths,
                                   n.deaths > p75.n.deaths ~ n.deaths - p75.n.deaths,
                                   n.deaths >= p25.n.deaths & n.deaths <= p75.n.deaths ~ 0)) %>%
  group_by(federal_state) %>%
  summarise(excess_deaths = sum(excess_deaths)) %>%
  ggplot(aes(x = fct_reorder(as_factor(federal_state), excess_deaths),
             y = excess_deaths)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "steelblue") +
  coord_flip() +
  theme_ft_rc() +
  labs(title = "Cumulative Excess Deaths by Federal State",
       subtitle = "Relative to median value of years 2016-2019.\nMarch 1, 2020 to June 7, 2020.",
       x = NULL,
       y = "Cumulative Number of Excess Deaths",
       caption = "Data: Destatis, Analysis: Florentin Krämer")

ggsave(filename = "cum_excess_mortality_state_median.pdf", path = "graphs/", device = cairo_pdf, 
       width = 29.7, height = 21, units = "cm")

ggsave(filename = "cum_excess_mortality_state_median.png", path = "graphs/", type = "cairo-png", 
       width = 29.7, height = 21, units = "cm", dpi = 300)

## Average Ratio of Excess Deaths by Federal State, Median
states_days %>%
  filter(date >= "2020-03-01") %>%
  mutate(ratio.deaths = n.deaths / median.n.deaths) %>%
  group_by(federal_state) %>%
  summarise(ratio.deaths = mean(ratio.deaths)) %>%
  ggplot(aes(x = fct_reorder(as_factor(federal_state), ratio.deaths),
             y = ratio.deaths)) +
  # geom_point(color = "steelblue", size = 3) +
  geom_lollipop(point.size = 3, point.colour = "steelblue", color = "grey70") +
  geom_hline(yintercept = 1, color = "steelblue") +
  coord_flip() +
  theme_ft_rc() +
  labs(title = "Ratio of 2020 Deaths to Median Value of Deaths by Federal State",
       subtitle = "Relative to median value of years 2016-2019.\nMarch 1, 2020 to June 7, 2020.",
       x = NULL,
       y = "Ratio of Deaths to Median Value of Deaths",
       caption = "Data: Destatis, Analysis: Florentin Krämer")

ggsave(filename = "ratio_excess_mortality_state_median.pdf", path = "graphs/", device = cairo_pdf, 
       width = 29.7, height = 21, units = "cm")

ggsave(filename = "ratio_excess_mortality_state_median.png", path = "graphs/", type = "cairo-png", 
       width = 29.7, height = 21, units = "cm", dpi = 300)

## Excess Deaths over Time

cum_excess_deaths.byState <- states_days %>%
  filter(date >= "2020-03-01") %>%
  mutate(excess_deaths = case_when(n.deaths < p25.n.deaths ~ n.deaths - p25.n.deaths,
                                   n.deaths > p75.n.deaths ~ n.deaths - p75.n.deaths,
                                   n.deaths >= p25.n.deaths & n.deaths <= p75.n.deaths ~ 0)) %>%
  group_by(federal_state) %>%
  summarise(cum_excess_deaths = sum(excess_deaths)) %>%
  mutate(flag_positive = forcats::as_factor(if_else(cum_excess_deaths > 0, "Positive", "Negative")))

states_days %>%
  filter(date >= "2020-03-01") %>%
  left_join(cum_excess_deaths.byState) %>%
  mutate(excess_deaths = case_when(n.deaths < p25.n.deaths ~ n.deaths - p25.n.deaths,
                                   n.deaths > p75.n.deaths ~ n.deaths - p75.n.deaths,
                                   n.deaths >= p25.n.deaths & n.deaths <= p75.n.deaths ~ 0)) %>%
  ggplot(aes(x = date,
             y = excess_deaths,
             color = flag_positive)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-23")),
             linetype = 4,
             color = "grey70") + # corresponds to March 23, 2020 - on this day, social contact limitations became effective
  facet_wrap(~ federal_state) +
  theme_ft_rc() +
  scale_color_manual(values = c("Positive" = "#852828", "Negative" = "#719861")) +
  scale_y_continuous(breaks = c(-100, 0, 100)) +
  guides(color = FALSE) +
  labs(title = "Excess Deaths by Federal State over Time",
       subtitle = "Vertical line corresponds to March 23, 2020, the day on which strict social distancing measures were implemented. \nColors encode cumulative excess deaths: green if negative, red if positive.",
       x = NULL,
       y = "Number of Excess Deaths",
       color = NULL,
       caption = "Data: Destatis, Analysis: Florentin Krämer")

ggsave(filename = "excess_deaths_time_state.pdf", path = "graphs/", device = cairo_pdf, 
       width = 29.7, height = 21, units = "cm")

ggsave(filename = "excess_deaths_time_state.png", path = "graphs/", type = "cairo-png", 
       width = 29.7, height = 21, units = "cm", dpi = 300)

# By Age Group ------------------------------------------------------------

## Excess Mortality from All Causes by Age Group, Mean

germany_ages %>%
  ggplot() +
  geom_ribbon(aes(x = yday,
                  ymin = lower_bound.mean.excess_mortality,
                  ymax = upper_bound.mean.excess_mortality),
              color = "grey30",
              fill = "grey30") +
  geom_line(aes(x = yday,
                y = rolling_mean.ratio.mean.excess_mortality,
                color = year),
            data = subset(germany_ages, year == 2020)) +
  geom_hline(yintercept = 1, color = "grey70") +
  facet_wrap(~ age_category) +
  theme_ft_rc() +
  labs(title = "Excess Mortality from All Causes by Age Categories",
       subtitle = "Relative to mean value of years 2016-2019. Shaded grey ribbon: two standard deviations. \nData for 2019 and 2020 preliminary. Details: https://tinyurl.com/yacmwu9d.",
       x = "Day of Year",
       y = "Ratio of 2020 deaths to mean 2016-2019 deaths",
       color = NULL,
       caption = "Data: Destatis, Analysis: Florentin Krämer")

ggsave(filename = "excess_mortality_age_mean.pdf", path = "graphs/", device = cairo_pdf, 
       width = 29.7, height = 21, units = "cm")

ggsave(filename = "excess_mortality_age_mean.png", path = "graphs/", type = "cairo-png", 
       width = 29.7, height = 21, units = "cm", dpi = 300)

## Excess Mortality from All Causes by Age Group, Median

germany_ages %>%
  ggplot() +
  geom_ribbon(aes(x = yday,
                  ymin = lower_bound.median.excess_mortality,
                  ymax = upper_bound.median.excess_mortality),
              color = "grey30",
              fill = "grey30") +
  geom_line(aes(x = yday,
                y = rolling_mean.ratio.median.excess_mortality,
                color = year),
            data = subset(germany_ages, year == 2020)) +
  geom_hline(yintercept = 1, color = "grey70") +
  facet_wrap(~ age_category) +
  theme_ft_rc() +
  labs(title = "Excess Mortality from All Causes by Age Categories",
       subtitle = "Relative to median value of years 2016-2019. Shaded grey ribbon: interquartile range. \nData for 2019 and 2020 preliminary. Details: https://tinyurl.com/yacmwu9d.",
       x = "Day of Year",
       y = "Ratio of 2020 deaths to median 2016-2019 deaths",
       color = NULL,
       caption = "Data: Destatis, Analysis: Florentin Krämer")

ggsave(filename = "excess_mortality_age_median.pdf", path = "graphs/", device = cairo_pdf, 
       width = 29.7, height = 21, units = "cm")

ggsave(filename = "excess_mortality_age_median.png", path = "graphs/", type = "cairo-png", 
       width = 29.7, height = 21, units = "cm", dpi = 300)

## Cumulative Excess Deaths by Age Group, Median
germany_ages %>%
  filter(date >= "2020-03-01") %>%
  mutate(excess_deaths = case_when(n.deaths < p25.n.deaths ~ n.deaths - p25.n.deaths,
                                   n.deaths > p75.n.deaths ~ n.deaths - p75.n.deaths,
                                   n.deaths >= p25.n.deaths & n.deaths <= p75.n.deaths ~ 0)) %>%
  group_by(age_category) %>%
  summarise(excess_deaths = sum(excess_deaths)) %>%
  ggplot(aes(x = fct_reorder(as_factor(age_category), excess_deaths),
             y = excess_deaths)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "steelblue") +
  coord_flip() +
  theme_ft_rc() +
  labs(title = "Cumulative Excess Deaths by Age Category",
       subtitle = "Relative to median value of years 2016-2019.\nMarch 1, 2020 to June 7, 2020.",
       x = NULL,
       y = "Cumulative Number of Excess Deaths",
       caption = "Data: Destatis, Analysis: Florentin Krämer")

ggsave(filename = "cum_excess_mortality_age_median.pdf", path = "graphs/", device = cairo_pdf, 
       width = 29.7, height = 21, units = "cm")

ggsave(filename = "cum_excess_mortality_age_median.png", path = "graphs/", type = "cairo-png", 
       width = 29.7, height = 21, units = "cm", dpi = 300)

## Average Ratio of Excess Deaths by Age Group, Median
germany_ages %>%
  filter(date >= "2020-03-01") %>%
  mutate(ratio.deaths = n.deaths / median.n.deaths) %>%
  group_by(age_category) %>%
  summarise(ratio.deaths = mean(ratio.deaths)) %>%
  ggplot(aes(x = age_category,
             y = ratio.deaths)) +
  # geom_point(color = "steelblue", size = 3) +
  geom_lollipop(point.size = 3, point.colour = "steelblue", color = "grey70") +
  geom_hline(yintercept = 1, color = "steelblue") +
  coord_flip() +
  theme_ft_rc() +
  labs(title = "Ratio of 2020 Deaths to Median Value of Deaths by Age Category",
       subtitle = "Relative to median value of years 2016-2019.\nMarch 1, 2020 to June 7, 2020.",
       x = NULL,
       y = "Ratio of Deaths to Median Value of Deaths",
       caption = "Data: Destatis, Analysis: Florentin Krämer")

ggsave(filename = "ratio_excess_mortality_age_median.pdf", path = "graphs/", device = cairo_pdf, 
       width = 29.7, height = 21, units = "cm")

ggsave(filename = "ratio_excess_mortality_age_median.png", path = "graphs/", type = "cairo-png", 
       width = 29.7, height = 21, units = "cm", dpi = 300)

## Excess Deaths over Time

cum_excess_deaths.byAge <- germany_ages %>%
  filter(date >= "2020-03-01") %>%
  mutate(excess_deaths = case_when(n.deaths < p25.n.deaths ~ n.deaths - p25.n.deaths,
                                   n.deaths > p75.n.deaths ~ n.deaths - p75.n.deaths,
                                   n.deaths >= p25.n.deaths & n.deaths <= p75.n.deaths ~ 0)) %>%
  group_by(age_category) %>%
  summarise(cum_excess_deaths = sum(excess_deaths)) %>%
  mutate(flag_positive = forcats::as_factor(if_else(cum_excess_deaths > 0, "Positive", "Negative")))

germany_ages %>%
  filter(date >= "2020-03-01") %>%
  left_join(cum_excess_deaths.byAge) %>%
  mutate(excess_deaths = case_when(n.deaths < p25.n.deaths ~ n.deaths - p25.n.deaths,
                                   n.deaths > p75.n.deaths ~ n.deaths - p75.n.deaths,
                                   n.deaths >= p25.n.deaths & n.deaths <= p75.n.deaths ~ 0)) %>%
  ggplot(aes(x = date,
             y = excess_deaths,
             color = flag_positive)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-23")),
             linetype = 4,
             color = "grey70") + # corresponds to March 23, 2020 - on this day, social contact limitations became effective
  facet_wrap(~ age_category) +
  theme_ft_rc() +
  scale_color_manual(values = c("Positive" = "#852828", "Negative" = "#719861")) +
  scale_y_continuous(breaks = c(-100, 0, 100)) +
  guides(color = FALSE) +
  labs(title = "Excess Deaths by Age Category over Time",
       subtitle = "Vertical line corresponds to March 23, 2020, the day on which strict social distancing measures were implemented. \nColors encode cumulative excess deaths: green if negative, red if positive.",
       x = NULL,
       y = "Number of Excess Deaths",
       caption = "Data: Destatis, Analysis: Florentin Krämer")

ggsave(filename = "excess_deaths_time_age.pdf", path = "graphs/", device = cairo_pdf, 
       width = 29.7, height = 21, units = "cm")

ggsave(filename = "excess_deaths_time_age.png", path = "graphs/", type = "cairo-png", 
       width = 29.7, height = 21, units = "cm", dpi = 300)
