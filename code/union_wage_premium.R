#Import libraries
library(tidyverse)
library(epiextractr)
library(epidatatools)
library(labelled)
library(realtalk)

# Import CPS org data, load as many years necessary to get sufficient sample sizes or desired for time series.
cps_org <- load_org(2020:2024, "year", "age", "statefips", "wage", "union", "lfstat", "orgwgt", "a_earnhour", "cow1") %>%
  # Age and labor force restrictions, non-imputed wages.
  filter(age >= 16, cow1 <= 5, a_earnhour != 1,
    !is.na(wage))

## Method 1: For point-in-time comparisons or for states that have small sample sizes and require pooling. ##

# Create a dataframe for wage data and calculate an averaged median wage (corrects for wages clumping around round numbers).
# Note: divide orgwgt by as many months are in your pool.
wage_single <- cps_org |>
  mutate(union = to_factor(union)) |>
  summarise(
      wage_median = averaged_median(
        x = wage, 
        w = orgwgt/60,  
        quantiles_n = 9L, 
        quantiles_w = c(1:4, 5, 4:1)),
        n=n(),
        .by=c(union, statefips))

# Separate out union versus nonunion wages and calculate the percent difference, or union wage premium.
wage_dif_single <- wage_single |>
  mutate(union_stat = case_when(union == "Not union represented" ~ "nonunion", union == "Union represented" ~ "union")) |>
  pivot_wider(id_cols = statefips, names_from = union_stat, values_from = wage_median) |>
  mutate(diff = ((union-nonunion)/nonunion))|>
mutate(state = to_factor(statefips)) |>
select(statefips, state, everything())

## Method 2: For inflation-adjusted time series (note: use only with sufficient sample sizes). ##

# Calculate real wage over time: load CPI data from realtalk
cpi_data <- realtalk::c_cpi_u_annual

# Set base year to 2024
cpi2024 <- cpi_data$c_cpi_u[cpi_data$year==2024]

# Create a dataframe with wage information by year.
# Note: change statefips to whatever state you prefer.
wage_series <- cps_org |>
  filter(statefips == 36) |>
  mutate(union = to_factor(union)) |>
  summarise(
     wage_median = averaged_median(
     x = wage, 
     w = orgwgt/12,  
     quantiles_n = 9L, 
     quantiles_w = c(1:4, 5, 4:1)),
     n=n(),
     .by = c(year, union)) |>
  # Merge annual CPI data to data frame by year
  left_join(cpi_data, by='year') |>
  # Inflation adjust wages
 mutate(real_wage = wage_median * (cpi2024/c_cpi_u)) |>
select(year, union, real_wage)

# Separate out union versus nonunion wages and calculate the percent difference, or union wage premium.
wage_dif_series <- wage_series |>
  mutate(union_stat = case_when(union == "Not union represented" ~ "nonunion", union == "Union represented" ~ "union")) |>
  pivot_wider(id_cols = year, names_from = union_stat, values_from = real_wage) |>
  mutate(diff = ((union-nonunion)/nonunion)) |>
  select(year, nonunion, union, diff)

##test