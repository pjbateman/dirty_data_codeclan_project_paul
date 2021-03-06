# loading in the necessary libraries

library(tidyverse) # loads the tibble package to convert rownmes_to_col
library(janitor) # to use the clean names function
library(readr) # used to read in the raw data file type .rds
library(here) # to enable the file locations to be reproducible

# fetching the raw data file 
decathlon <- read_rds(here("raw_data/decathlon.rds"))

# the first column contains row names and needs to be fixed
decathlon <- decathlon %>% 
  rownames_to_column(var = "athlete")

# cleaning the column names
decathlon_clean_names <- clean_names(decathlon)

# check the column data types make sense
glimpse(decathlon_clean_names)

# The athlete names should feature a consistent format
decathlon_clean_names <- decathlon_clean_names %>% 
  mutate(
    athlete = str_to_title(athlete)
  )

# we need to make the data "tidy", as rank 1 for example, appears twice 
# use the pivot_wider() function
decathlon_wide <- decathlon_clean_names %>%
  pivot_wider(names_from = competition, 
              values_from = rank)


# finally, output the clean data to a new file
write_csv(decathlon_wide, "clean_data/decathlon_clean.csv")
