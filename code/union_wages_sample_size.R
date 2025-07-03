# import libraries
library(zoo)
library(tidyverse)
library(here)
library(epiextractr)
library(epidatatools)
library(openxlsx)
library(labelled)
library(slider)


# import CPS org data
cps_org <- load_org(1999:2024, "year", "month", "age", "statefips", "ind17", "pubfed", "cow1", "pubsec",
                    "wage", "statefips", "union", "emp", "lfstat", "orgwgt", "a_earnhour") %>%
  # Age and labor force restrictions, non-imputed
  filter(age >= 16, lfstat == 1, a_earnhour != 1)

union_wage <- cps_org |>
  mutate(union = to_factor(union)) |>
  summarise(wage = weighted.mean(wage, w = orgwgt/12, na.rm = TRUE),
            n=n(),
            .by=c(union, statefips, year)) |>
  mutate(state = to_factor(statefips))

union_5 <- cps_org |>
  filter(union == 1) |>
  group_by(statefips,year) |>
  summarize(
  n_count = sum(emp,na.rm=TRUE) )%>%
  arrange(statefips,year) %>%
  group_by(statefips) %>%
  mutate(rolling_total = slider::slide_dbl(n_count,sum,.before=4,.complete=TRUE)) |>
  mutate(state = to_factor(statefips))

union_9 <- cps_org |>
  filter(union == 1) |>
  group_by(statefips,year) |>
  summarize(
    n_count = sum(emp,na.rm=TRUE) )%>%
  arrange(statefips,year) %>%
  group_by(statefips) %>%
  mutate(rolling_total = slider::slide_dbl(n_count,sum,.before=8,.complete=TRUE)) |>
  mutate(state = to_factor(statefips))

# write Excel file
union_sample_sizes <- createWorkbook()
#add worksheets
addWorksheet(union_sample_sizes, sheetName = "wage data")
addWorksheet(union_sample_sizes, sheetName = "sample sizes")
#write data
writeData(union_sample_sizes, x=union_wage, sheet = "wage data", startCol = 1, startRow = 1, colNames = TRUE)
writeData(union_sample_sizes, x=union_5, sheet = "sample sizes", startCol = 1, startRow = 1, colNames = TRUE)
writeData(union_sample_sizes, x=union_9, sheet = "sample sizes", startCol = 7, startRow = 1, colNames = TRUE)

#export
saveWorkbook(union_sample_sizes, "union_sample_sizes.xlsx", overwrite = TRUE)