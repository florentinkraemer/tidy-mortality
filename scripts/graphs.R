library(tidyverse)
library(hrbrthemes)

# By State ----------------------------------------------------------------

excess_deaths_byState <- states_days %>%
  filter(year == 2020) %>%
  group_by(federal_state) %>%
  summarise(median.excess_mortality = sum(median.excess_mortality))

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
  geom_smooth(data = subset(states_days, year == "2020"),
              mapping = aes(x = yday,
                            y = ratio.median.excess_mortality,
                            color = year),
              se = TRUE) +
  facet_wrap(~ federal_state) +
  theme_ft_rc() +
  scale_y_continuous(breaks = c(0.7, 1, 1.3)) +
  labs(x = "Day of Year",
       y = "Ratio of 2020 deaths to median 2016-2019 deaths",
       color = NULL,
       title = "Excess Mortality from All Causes by Federal State",
       subtitle = "Baseline: median value of years 2016-2019, shaded grey ribbon: interquartile range \ndata for 2019 and 2020 preliminary",
       caption = "Data: Destatis, Analysis: Florentin Krämer")

ggsave(filename = "excess_mortality_state_median.pdf", path = "graphs/", device = cairo_pdf, 
       width = 29.7, height = 21, units = "cm")

ggsave(filename = "excess_mortality_state_median.png", path = "graphs/", type = "cairo-png", 
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

ggsave(filename = "excess_mortality_age_median.pdf", path = "graphs/", device = cairo_pdf, 
       width = 29.7, height = 21, units = "cm")

ggsave(filename = "excess_mortality_age_median.png", path = "graphs/", type = "cairo-png", 
       width = 29.7, height = 21, units = "cm", dpi = 300)
