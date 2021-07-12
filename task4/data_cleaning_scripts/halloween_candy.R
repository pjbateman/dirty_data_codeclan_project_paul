# loading in the necessary libraries

library(tidyverse) # loads the tibble package to convert rownmes_to_col
library(janitor) # to use the clean names function
library(readxl) # to fetch the raw data files

# fetching the raw data files
candy_2015_raw <- read_excel("raw_data/boing-boing-candy-2015.xlsx")
candy_2016_raw <- read_excel("raw_data/boing-boing-candy-2016.xlsx")
candy_2017_raw <- read_excel("raw_data/boing-boing-candy-2017.xlsx")

# thought about how best to join the data
# looked at the questions to see which columns were needed, 
  # decided i needed to keep columns: containing joy, despair or meh; age; going
  # out trick-or-treating or not; timestamp; gender; country.  All other columns
  # could be dropped
# decided to clean the names of each data set first to make combining easier, 
candy_2015_clean_names <- clean_names(candy_2015_raw)
candy_2016_clean_names <- clean_names(candy_2016_raw)
candy_2017_clean_names <- clean_names(candy_2017_raw)

# create and then apply a function to standardise the columns: age, t/t, country
clean_candy_age <- function(candy){
  candy_with_year <- candy %>% 
    # if the column timestamp is present, use it, else make the year 2017
    if ("timestamp" %in% colnames(candy) == TRUE){
      mutate(
        year = str_extract(timestamp,("[0-9]{4}")), .before = timestamp
      )
    } else {
      mutate(year = "2017") # improve later to take year from file name
    } 
  
  output <- candy_with_year %>% 
    rename(
      age = matches("old_are_you|age")
      ) %>% 
    rename(
      age = matches("age1")
    ) %>% rename(
      trick_or_treating = matches("trick_or_treat|going_out")
    )  %>% 
    rename(
      country = matches("country")
    ) %>% 
    rename(
      gender = matches("gender")
    )
  return(output)
}

candy_2015_clean_names <- clean_candy_age(candy_2015_clean_names)
candy_2016_clean_names <- clean_candy_age(candy_2016_clean_names)
candy_2017_clean_names <- clean_candy_age(candy_2017_clean_names)
  # check the data types of the columns (integer, categorical etc)
  # then join the 3 data sets, 
  # then drop the unneeded columns