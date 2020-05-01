
# By State ----------------------------------------------------------------

excess_deaths_byState <- states_days %>%
  filter(year == 2020) %>%
  group_by(federal_state) %>%
  summarise(median.excess_mortality = sum(median.excess_mortality))