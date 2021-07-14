# loading in the necessary libraries

library(tidyverse) # loads the tibble package to convert rownmes_to_col
library(janitor) # to use the clean names function
library(readxl) # to fetch the raw data files
library(stringr) # to standardise some of the string data, e.g. country column

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
# and add a year column to each data set
candy_2015_clean_names <- clean_names(candy_2015_raw) %>% 
  mutate(
    year = str_extract(timestamp,("[0-9]{4}")), .before = timestamp
  ) %>%  
  mutate(country = NA, .after = year) %>% # add missing column of NAs
  mutate(gender = NA, .after = year) # add missing column of NAs

candy_2016_clean_names <- clean_names(candy_2016_raw) %>% 
  mutate(
    year = str_extract(timestamp,("[0-9]{4}")), .before = timestamp
  )

candy_2017_clean_names <- clean_names(candy_2017_raw) %>% mutate(year = "2017", .before = internal_id) 

# create and then apply a function to standardise the columns: age, t/t, country
clean_candy_key_columns <- function(candy){
  
  output <- candy %>% 
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
    ) %>% # reorder the key columns for the questions
    relocate(country, .after = year) %>% 
    relocate(age, .after = country) %>% 
    relocate(gender, .after = age) %>% 
    relocate(trick_or_treating, .after = gender)

  return(output)
}
# applied the new function to the data sets to standardise the key columns
candy_2015_clean_names <- clean_candy_key_columns(candy_2015_clean_names)
candy_2016_clean_names <- clean_candy_key_columns(candy_2016_clean_names)
candy_2017_clean_names <- clean_candy_key_columns(candy_2017_clean_names)

# remove columns after trick_or_treating, unless they contain rating data
drop_unwanted_columns <- function(cleaned_names) {
  first_bit <- cleaned_names[1:5]
  second_bit <- cleaned_names[6:ncol(cleaned_names)] %>% 
    select(where(~any(. %in% c("JOY", "DESPAIR", "MEH"), na.rm = TRUE)))  
  output <- bind_cols(first_bit, second_bit)
  return(output)
}
candy_2015_lean <- drop_unwanted_columns(candy_2015_clean_names)
candy_2016_lean <- drop_unwanted_columns(candy_2016_clean_names)
candy_2017_lean <- drop_unwanted_columns(candy_2017_clean_names)


# # investigated the columns to see which ones to keep
# names(candy_2015_clean_names)
# # subset the columsn of 2015 in a hard-coded way (improve later)
# candy_2015_lean <- candy_2015_clean_names[c(1:5,7:99,118)]
# # check to see output is as expected
# names(candy_2015_lean)
# # followed the same process for 2016 and 2017 data
# names(candy_2016_clean_names)
# candy_2016_lean <- candy_2016_clean_names[c(1:5,8:107)]
# names(candy_2016_lean)
# names(candy_2017_clean_names)
# candy_2017_lean <- candy_2017_clean_names[c(1:5,8:110)]
# names(candy_2017_lean)


# with each data set, pivot-longer to have a starting column of candy
candy_2015_longer <- candy_2015_lean %>% 
  pivot_longer(cols = 6:last_col(), names_to = "candy", values_to = "rating", values_drop_na = TRUE) %>% 
  # reorder the key columns for consistency
  relocate(candy, .before = year) %>% 
  relocate(rating, .after = candy)

candy_2016_longer <- candy_2016_lean %>% 
  pivot_longer(cols = 6:last_col(), names_to = "candy", values_to = "rating", values_drop_na = TRUE) %>% 
  # reorder the key columns for consistency
  relocate(candy, .before = year) %>% 
  relocate(rating, .after = candy)

candy_2017_longer <- candy_2017_lean %>% 
  pivot_longer(cols = 6:last_col(), names_to = "candy", names_prefix = "q6_",values_to = "rating", values_drop_na = TRUE) %>% 
  # reorder the key columns for consistency
  relocate(candy, .before = year) %>% 
  relocate(rating, .after = candy)


  # then join the 3 data sets with Rbind, 
candy_bound <- bind_rows(candy_2015_longer, candy_2016_longer, candy_2017_longer)

# prescribe the best data types to the columns. maybe prioritise before pivot
head(candy_bound)
candy_data_types <- candy_bound %>% 
  mutate(
    candy = as.character(candy), 
    rating = as.factor(rating),# could add heirarchy later
    year = as.integer(as.numeric(year)),
    country = as.character(country),
    #age = as.integer(as.numeric(age)) Need to look later
    gender = as.factor(gender))
    #trick_or_treating = as.logical(trick_or_treating)) Need to look later

head(candy_data_types)
uk <- c("england", "endland", "scotland", "uk", "united kindom") 
usa <- c("'merica","ahem....amerca","alaska", "california", 
           "i pretend to be from canada, but i am really from the united states.",
           "merica", "america", "murica", "murrika", "n. america", 'new jersey',
           "new york", "north carolina", "pittsburgh", "sub-canadian north america... 'merica",
           "the best one - usa", "the united states", "the united states of america",
           "the yoo ess of aaayyyyyy", "trumpistan", "u s", "u s a",
           "u.s.", "u.s.a.", "united  states of america", "united states of america", "united sates",
           "united staes", "united state", "united statea", "united stated",
           "united statss", "united stetes", "unites states", "units states",
           "us", "us of a", "usa", "usa (i think but it's an election year so who can really tell)",
           "usa usa usa", "usa usa usa usa", "usa usa usa!!!!", "usa!", 
           "usa! usa!", "usa! usa! usa!", "usa!!!!!!", "usa? hard to tell anymore..",
           "usausausa", "ussa")

# clean up the country column data
candy_countries <- candy_data_types %>% 
  mutate(
      country = str_trim(str_to_lower(country))) %>% 
  mutate(country = 
        case_when(
          country %in% uk ~ "united kingdom",
          country %in% usa ~ "united states",
          country %in% c("can", "canada`", "canae") ~ "canada",
          country %in% c("cascadia", "a tropical island south of the equator", 
          "a", "atlantis", "denial", "earth", "eua", "europe", "fear and loathing", 
          "god's country", "i don't know anymore", "insanity lately",
          "narnia", "neverland", "not the usa or canada", "one of the best ones",
          "see above", "somewhere", "subscribe to dm4uz3 on youtube",
          "the republic of cascadia", "there isn't one for old men", "this one") ~ "unspecified",
          country %in% c("españa") ~ "spain",
          country %in% c("the netherlands") ~ "netherlands",
          country %in% c("uae") ~ "united arab emirates",
          str_detect(string = country, pattern = "[0-9]") ~ "unspecified",
          is.na(country) ~ "unspecified",
          TRUE ~ country
                  )
        ) %>% 
  arrange(country) %>% 
  mutate(country = str_to_title(country))
        
countries <- distinct(candy_countries, country)


# clean up the gender column data
candy_gender <- candy_countries %>% 
  mutate(
    gender = fct_collapse(
      gender, Unspecified = c("Other", "I'd rather not say"))) %>% 
  mutate(
    gender = fct_explicit_na(gender, "Unspecified")
  )

# clean up the age column data
# want to keep values which contain begin two digits, followed by a blank, a 
  # decimal place, or a comma., then extract the number, then convert to integer

candy_age <- candy_gender %>% 
  mutate(
    age = str_extract(string = age, pattern = "[0-9\\.,]+")
  ) %>% 
  mutate(age = as.numeric(age)) %>% 
  arrange(age)
ages <- distinct(candy_age, age)

ages_before <- distinct(candy_gender,age) %>% 
  arrange(age)# 276 observations

# clean up the candy names
# write the cleaned data to a new file
  # write_csv(test, "clean_data/candy_clean.csv")
# move to answering questions in an .Rmd file