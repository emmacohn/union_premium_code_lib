#import libraries
library(zoo)
library(tidycensus)
library(tidyverse)
library(here)
library(epiextractr)
library(epidatatools)
library(openxlsx)
library(labelled)
library(slider)

# Questions -- combining for AAPI (some SWADL doesn't have AAPI, omit?)?

# This option will retrieve geographic data from the Census
options(tigris_use_cache = TRUE)

# Table #B19013H: White Alone, not Hispanic or Latino
# Table #B19013B: Black or African American Alone
# Table #B19013I: Hispanic or Latino
## Problem: AAPI = #B19013D (Asian Alone) AND #B19013E (Native Hawaiian and Other Pacific Islander Alone)

# load individual income tables by race as one table
load_acs_tables <- function(x){
  get_acs(geography = "state", 
          variables = c(white_alone = "B19013H_001",  
                        black_alone = "B19013B_001",  
                        hispanic = "B19013I_001"), 
          year = x, 
          survey  = "acs1",
          output = "wide") %>% 
    #create year variable
    mutate(year = x)
}


#load all data 2005 to 2023 (excluding 2020)
hhincome <- map_dfr(c(2005:2019, 2021:2023), load_acs_tables)