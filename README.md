# tidy-mortality
Clean and tidy mortality data for 2016 to 2020 from the Federal Statistical Office of Germany. The original dataset in .xlsx format is available at [this link](https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Sterbefaelle-Lebenserwartung/Tabellen/sonderauswertung-sterbefaelle.html?nn=209016). Note that data for 2019 and 2020 are preliminary and have not yet undergone plausibility checks.

*Disclaimer*: I do not take responsibility for any errors either in the original dataset or in my cleaned-up version. Use with caution.

# Graphs

The tidied data allow for different decompositions: by *federal state* and by *age group*. The raw dataset also contains mortality data for combinations of broader age groups (0-64 and 65+) and all federal states. In what follows, I first calculate the median number of deaths by federal state/age group for all years prior to 2020 (2016 to 2019). To provide a measure of how spread out these numbers are over the years, I also calculate the interquartile range, i.e. the range between the 25th and 75th percentile. Data for 2020 are then plotted as a smooth function using loess smoothing.

## By Federal State

