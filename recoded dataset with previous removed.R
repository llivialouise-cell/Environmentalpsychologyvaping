library(tidyverse)
library(stringr)

cleaned_dataset <- read_csv("clean_responses.csv")

# recode knowledge 

recoded_dataset <- cleaned_dataset %>%
  mutate(across(
    c(knowledge_1, knowledge_2, knowledge_3, knowledge_6), # true is correct
    ~ case_when(
      trimws(tolower(.)) == "true"  ~ 1,
      trimws(tolower(.)) == "false" ~ 0,
      TRUE ~ NA_real_ 
    )))

recoded_dataset <- recoded_dataset %>% 
  mutate(across(
    c(knowledge_4, knowledge_5, knowledge_7, knowledge_8,
      knowledge_9, knowledge_10, knowledge_11, knowledge_12), # false is correct 
    ~ case_when(
      trimws(tolower(.)) == "false"  ~ 1,    
      trimws(tolower(.)) == "true" ~ 0,
      TRUE ~ NA_real_
    )))

# clean up column responses to be uniform 

recoded_dataset <- recoded_dataset %>%
  mutate(across(
    55:82,
    ~ if (is.character(.x) || is.factor(.x)) {
      .x %>%
        as.character() %>%
        stringr::str_squish() %>%  # trim and remove extra spaces
        tolower()
    } else {
      .x
    }
  ))

# recode all values as they are the same coding 

recoded_dataset <- recoded_dataset %>% 
  mutate(across(all_of(c(
    "egoistic_1", "egoistic_2", "egoistic_3", "egoistic_4", "egoistic_5",
    "hedonistic_1", "hedonistic_2", "hedonistic_3",
    "altruistic_1", "altruistic_2", "altruistic_3", "altruistic_4", "altruistic_5",
    "biospheric_1", "biospheric_2", "biospheric_3", "biospheric_4" )),
    ~ case_when(
      .x == "strongly disagree" ~ 1,
      .x == "disagree" ~ 2,
      .x == "slightly disagree" ~ 3,
      .x == "neither agree nor disagree" ~ 4,
      .x == "slightly agree" ~ 5,
      .x == "agree" ~ 6,
      .x == "strongly agree" ~ 7,
      TRUE ~ NA_real_  )))

# mutate attitude and belief variables that dont need to be recoded

recoded_dataset <- recoded_dataset %>% 
  mutate(across(all_of(c(
    "beliefs_1", "beliefs_3",
    "attitude_4","attitude_5","attitude_6")),
    ~ case_when(
      .x == "i don't know" ~ NA_real_,
      .x == "strongly disagree" ~ 1,
      .x == "disagree" ~ 2,
      .x == "slightly disagree" ~ 3,
      .x == "neither agree nor disagree" ~ 4,
      .x == "slightly agree" ~ 5,
      .x == "agree" ~ 6,
      .x == "strongly agree" ~ 7,
      TRUE ~ NA_real_  )))


# recode attitude and beleif variables that need to be recoded 

recoded_dataset <- recoded_dataset %>% 
  mutate(across(all_of(c(
    "beliefs_2", "beliefs_4",
    "attitude_1","attitude_2","attitude_3")),
    ~ case_when(
      .x == "i don't know" ~ NA_real_,
      .x == "strongly disagree" ~ 7,
      .x == "disagree" ~ 6,
      .x == "slightly disagree" ~ 5,
      .x == "neither agree nor disagree" ~ 4,
      .x == "slightly agree" ~ 3,
      .x == "agree" ~ 2,
      .x == "strongly agree" ~ 1,
      TRUE ~ NA_real_ )))


write_csv(recoded_dataset, "recoded_dataset.csv")

recoded_dataset_prev_removed <- recoded_dataset %>% 
select(-c(19,20, 25,26, 28, 32,34, 37,38, 41,42))

write_csv(recoded_dataset, "recoded_dataset_prev_removed.csv")

