
library(tidyverse)
library(readr)
library(dplyr)
library(stringr)
recoded_dataset <- read_csv("recoded_dataset.csv")

# here i need to understand which participants to remove and remove irrelevant cols 
# start by checking ethics 

test <- recoded_dataset %>%
  group_by(across(1)) %>%
  summarise(count = n())

# find who didnt agree to understanding the study 
checked_responses <- recoded_dataset %>% 
  filter(is.na(understands_study_purpose)) %>% 
  select(id)

# find row numbers 
which(recoded_dataset$id %in% c("pri300", "new300", "puffinmccloud", 
                                "lew161", "ran220"))
#  remove these participants 

final_participants <- recoded_dataset %>% 
  slice(-c(39, 202, 262, 282, 309))

# check next columns 

test <- final_participants %>%
  group_by(across(2:10)) %>%
  summarise(count = n())

# find who disagreed to the study 
checked_responses <- final_participants %>% 
  filter( wishes_to_take_part_in_study == "i disagree") %>% 
  select(id)

# find their column
which(final_participants$id %in% c("nooty", "g"))

# remove them
final_participants <- final_participants %>% 
  slice(-c(148, 149))

# find out who has never vaped and remove this needs to be done 


checked_responses <- final_participants %>%
  filter(
    (tolower(str_squish(as.character(previous_vaping_device_type))) == "other (please specify)" | is.na(previous_vaping_device_type)) &
      (tolower(str_squish(as.character(previous_vaping_device_type_other))) == "other (please specify)" | is.na(previous_vaping_device_type_other)) &
      (tolower(str_squish(as.character(current_vaping_device_type))) == "other (please specify)" | is.na(current_vaping_device_type)) &
      (tolower(str_squish(as.character(current_vaping_device_type_other))) == "other (please specify)" | is.na(current_vaping_device_type_other))
  ) %>%
  select(id, previous_vaping_device_type, previous_vaping_device_type_other,
         current_vaping_device_type, current_vaping_device_type_other)

# find their column  "
which(final_participants$id %in% c("fra060", "smi080", "top010", "cun230",
                                   "kan141", "sa2304","rb19","str201","jon060",
                                   "pun220","ros110","gil061", "dai28/","ho1409", "mca240","cha120",
                                   "ter260","mob130","win070","tur180", "hip125"))

# remove never vapers 

final_participants <- final_participants %>% 
  slice(-c(5, 22, 53, 134, 172, 178, 182, 190,
           197, 198, 208, 210, 214, 238, 268, 300, 327, 330,
           342, 390, 402, 416))

# check attention check 
test <- final_participants %>%
  group_by(attention_check) %>%
  summarise(count = n())

# check who didnt put slightly agree 

checked_responses <- final_participants %>%
  filter(attention_check %in% c("agree", "slightly disagree", "strongly agree")) %>%
  select(id, attention_check)

which(final_participants$id %in% c( "phi170", "man030", "tcf011", "rte",
                                    "jea210", "pal210", "flexible-vaper", "hep140",
                                    "cre070", "lid260", "mos020", "con300", "cin250",
                                    "jon161", "smy072503","raj301", 
                                    "jwt010199","bob010", "bar090", "cum420"))

final_participants <- final_participants %>% 
  slice(-c(18, 29, 44, 50, 74, 102, 105, 
           133, 191, 202, 222, 269, 292, 293,
           325, 380, 403, 405, 422, 262))

# check everyone is appropriate age 

test <- final_participants %>%
  group_by(age) %>%
  summarise(count = n())

which(final_participants$age == 98)

final_participants <- final_participants %>% 
  slice(-c(289))

write_csv(final_participants, "final_participants.csv")




