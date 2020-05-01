# tidy-mortality
Clean and [tidy mortality data](data/) (.csv and .rds) for 2016 to 2020 from the Federal Statistical Office of Germany. The original dataset in .xlsx format is available at [this link](https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Sterbefaelle-Lebenserwartung/Tabellen/sonderauswertung-sterbefaelle.html?nn=209016). Note that data for 2019 and 2020 are preliminary and have not yet undergone plausibility checks.

*Disclaimer*: I do not take responsibility for any errors either in the original dataset or in my cleaned-up version. Use with caution.

# Graphs

The tidied data allow for different decompositions: by *federal state* and by *age group*. The raw dataset also contains mortality data for combinations of broader age groups (0-64 and 65+) and all federal states. In what follows, I first calculate the median number of deaths by federal state/age group for all years prior to 2020 (2016 to 2019). To provide a measure of how spread out these numbers are over the years, I also calculate the interquartile range, i.e. the range between the 25th and 75th percentile. Data for 2020 are then plotted as a smooth function using loess smoothing.

## By Federal State

### Excess Mortality

![Excess Mortality by Federal State Relative to Median of 2016 to 2019](https://github.com/florentinkraemer/tidy-mortality/blob/master/graphs/excess_mortality_state_median.png)
Download as [pdf](https://github.com/florentinkraemer/tidy-mortality/blob/master/graphs/excess_mortality_state_median.pdf) or [png](https://github.com/florentinkraemer/tidy-mortality/blob/master/graphs/excess_mortality_state_median.png).

### Cumulative Excess Deaths

The following plot displays the cumulative number of deaths from January 1 until April 5, 2020 that can reasonably be considered "out of the ordinary", i.e. that fall outside the interquartile range. Note that we may have both positive and negative excess mortality. The calculation proceeds as follows: daily differences between the 25^th percentile and the number of deaths if the number of deaths is below the 25^th percentile, resp. daily differences between the 75^th percentile and the number of deaths if the number of deaths is above the 75^th percentile are summed up over the relevant date range. If the number of deaths falls within the interquartile range, excess deaths are recorded as zero.

![Excess Mortality by Federal State Relative to Median of 2016 to 2019](https://github.com/florentinkraemer/tidy-mortality/blob/master/graphs/cum_excess_mortality_state_median.png)
Download as [pdf](https://github.com/florentinkraemer/tidy-mortality/blob/master/graphs/cum_excess_mortality_state_median.pdf) or [png](https://github.com/florentinkraemer/tidy-mortality/blob/master/graphs/cum_excess_mortality_state_median.png).

### Average Ratio of Excess Deaths

## Excess Mortality By Age Group

![Excess Mortality by Age Group Relative to Median of 2016 to 2019](https://github.com/florentinkraemer/tidy-mortality/blob/master/graphs/excess_mortality_age_median.png)
Download as [pdf](https://github.com/florentinkraemer/tidy-mortality/blob/master/graphs/excess_mortality_age_median.pdf) or [png](https://github.com/florentinkraemer/tidy-mortality/blob/master/graphs/excess_mortality_age_median.png).



