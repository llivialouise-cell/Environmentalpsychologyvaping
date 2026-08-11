# load in dataset
final_participants <- read_csv('final_participants.csv')

final_dataset <- final_participants

# csv_ loaded the file in to have multiple decimals 
test <- final_dataset %>%           
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# remove ethics  that are not needed anymore 
final_dataset <- final_dataset %>% 
  select(-c(1:10, 12, 14, 18, 68))

 
# Replace "other (please specify)" with the other response 
final_dataset <- final_dataset %>%
  mutate(
    current_vaping_device_type = if_else(
      !is.na(current_vaping_device_type_other) & current_vaping_device_type_other != "" &
        str_detect(current_vaping_device_type, regex("other\\s*\\(please specify\\)", ignore_case = TRUE)),
      current_vaping_device_type_other,  
      current_vaping_device_type))

# check
test <- final_dataset %>% 
  group_by(current_vaping_device_type) %>% 
  summarise(count= n())

# remove the other column 
final_dataset <- final_dataset %>% 
  select(-current_vaping_device_type_other)

#  replace other please specified in nicotine into numeric 
final_dataset <- final_dataset %>%
  mutate(
    current_nicotine_level_other_mg_ml = as.numeric(current_nicotine_level_other_mg_ml),
    current_nicotine_level_mg_ml = as.numeric(current_nicotine_level_mg_ml),
    current_nicotine_level_mg_ml = coalesce(current_nicotine_level_other_mg_ml, current_nicotine_level_mg_ml))

# remove these columns
final_dataset <- final_dataset %>% 
  select(-current_nicotine_level_other_mg_ml)


# change bottle sizes columns to be numeric 
final_dataset <- final_dataset %>% 
  mutate(
    current_eliquid_bottle_size_mls = as.numeric(current_eliquid_bottle_size_mls),
    current_eliquid_bottle_size_other_mls = as.numeric(current_eliquid_bottle_size_other_mls),
    current_eliquid_bottle_size_mls = coalesce(current_eliquid_bottle_size_other_mls, current_eliquid_bottle_size_mls))


# remove these columns 
final_dataset <- final_dataset %>% 
  select(-current_eliquid_bottle_size_other_mls)

final_dataset <- final_dataset %>% 
  select(-eliquid_bottle_size_6_months_ago_other_ml)

# add up averages and sums 
final_dataset<- final_dataset %>%
  rowwise() %>%
  mutate(knowledge_sum = sum(c_across(15:26), na.rm = TRUE)) %>%
  ungroup()

# calculate averages for self transcendence and self enahncement 
final_dataset <- final_dataset %>%
  rowwise() %>%
  mutate(
    self_transcendence = round(mean(c_across(c(biospheric_1, biospheric_2, biospheric_3, biospheric_4,
                                               altruistic_1, altruistic_2, altruistic_3,
                                               altruistic_4, altruistic_5)), na.rm = TRUE),3),
    self_enhancement = round(mean(c_across(c(hedonistic_1, hedonistic_2, hedonistic_3,
                                             egoistic_1, egoistic_2, egoistic_3, 
                                             egoistic_4, egoistic_5)), na.rm = TRUE),3)) %>%
  ungroup()

# calculate average beliefs and attitudes 
final_dataset <- final_dataset %>%
  rowwise() %>%
  mutate(
    beliefs_average = round(mean(c_across(c(beliefs_1, beliefs_2, beliefs_3, beliefs_4)), na.rm = TRUE), 3),
    attitudes_average = round(mean(c_across(c(attitude_1, attitude_2, attitude_3,
                                              attitude_4, attitude_5, attitude_6)), na.rm = TRUE), 3)) %>%
  ungroup()


# calculate purchase frequency
final_dataset <- final_dataset %>% 
  mutate(
    current_eliquid_avg_daily_ml_consumption = current_eliquid_bottle_size_mls / current_time_to_finish_eliquid_bottle_days,
    current_pod_avg_daily_puffs = current_pod_puffs / current_time_taken_to_finish_pod_days,
    current_disposable_avg_daily_puffs = current_disposable_device_puffs / current_time_taken_to_finish_disposable_days)
    
# convert puffs to ml  current_pod_avg_daily_ml_consumption =

final_dataset <- final_dataset %>%
  mutate(
    current_pod_avg_daily_ml_consumption = current_pod_avg_daily_puffs * (1/300),
    current_disposable_avg_daily_ml_consumption = current_disposable_avg_daily_puffs * (1/300)
  )

# convert scientific numbers into whole numbers 
options(scipen = 999) 

# round the dataset to 3 decimals again 
final_dataset <- final_dataset %>%
  mutate(
    across(55:63, ~ round(., 3)))

# i need to check that all of the usage frequencies are right 
test <- final_dataset %>% 
  select(id, current_eliquid_bottle_size_mls, current_time_to_finish_eliquid_bottle_days, current_eliquid_avg_daily_ml_consumption)


test <- final_dataset %>% 
  select(id, current_pod_puffs, current_time_taken_to_finish_pod_days, current_pod_avg_daily_ml_consumption)


test <- final_dataset %>% 
  select(id, current_disposable_device_puffs, current_time_taken_to_finish_disposable_days, current_disposable_avg_daily_ml_consumption )



# move the columns into places that make the most sense 
final_dataset <- final_dataset %>%
  relocate(knowledge_sum, .before = egoistic_1) %>% 
  relocate(self_transcendence, .before = beliefs_1) %>% 
  relocate(self_enhancement, .before = beliefs_1) %>% 
  relocate(beliefs_average, .before = attitude_1) %>%
  relocate (current_disposable_avg_daily_ml_consumption, .before = current_pod_puffs)  %>% 
  relocate (current_pod_avg_daily_ml_consumption, .before = knowledge_1)  %>%
  relocate (current_eliquid_avg_daily_ml_consumption, .before = current_disposable_device_puffs)


write_csv(final_dataset, "final_dataset.csv")




# handling missing data (median imputation)
# because some participants were supposed to have NA, i need to do this case by case 
# start off easy with likert responses 

# knowledge
knowledge_data <- final_dataset %>% 
  select(18:29) 

sum(is.na(knowledge_data)) # # 1 missing data 

knowledge_data[is.na(knowledge_data)] <- 0
sum(is.na(knowledge_data))

final_dataset <- final_dataset %>%
  mutate(across(18:29, ~replace_na(., 0)))

# values 
altruism_data <- final_dataset %>% 
  select(altruistic_1, altruistic_2, altruistic_3,
         altruistic_4, altruistic_5)

sum(is.na(altruism_data)) # no missing data 

biospherism_data <- final_dataset %>% 
  select(biospheric_1, biospheric_2, 
         biospheric_3, biospheric_4)


sum(is.na(biospherism_data)) # no missing data 

hedonism_data <- final_dataset %>% 
  select(hedonistic_1, hedonistic_2, hedonistic_3)

sum(is.na(hedonism_data)) # no missing data 


egoistic_data <- final_dataset %>% 
  select(egoistic_1, egoistic_2,
         egoistic_3, egoistic_4, egoistic_5)

sum(is.na(egoistic_data)) # no missing data 


# beliefs 
beliefs_data <- final_dataset %>% 
  select(50:53)

sum(is.na(beliefs_data))   # 119 missing data points - but these are from "0- i dont know" being mutated so these were therefore not median imputated 


# attitudes 
attitude_data <- final_dataset %>% 
  select(55:60)

sum(is.na(attitude_data))   # no missing 


# HANDLE MISSING DATA 


current_nicotine_missing_data <- final_dataset %>% 
  select(current_nicotine_level_mg_ml, current_vaping_device_type)

# Step 2: impute median nicotine level for individuals who had NA 

current_nicotine_missing_data <- current_nicotine_missing_data %>%
  mutate(
    current_nicotine_level_mg_ml = ifelse(
      is.na(current_nicotine_level_mg_ml), 
      median(current_nicotine_level_mg_ml, na.rm = TRUE),  # Impute the median for NA values
      current_nicotine_level_mg_ml  # Keep the original value if it's not NA or they didn't vape
    )
  )
current_nicotine_missing_data <- current_nicotine_missing_data %>%
  mutate(
    current_nicotine_level_mg_ml = case_when(
      is.na(current_vaping_device_type) ~ NA_real_,  # Use NA_real_ to set to NA for numeric columns
      TRUE ~ current_nicotine_level_mg_ml  # Keep existing values for others
    )
  )

final_dataset$current_nicotine_level_mg_ml <- current_nicotine_missing_data$current_nicotine_level_mg_ml

# current eliquid bottle size missing data 


# Step 1: Calculate the median for current_eliquid_bottle_size_mls for "open pod" and "tank" users
median_eliquid_bottle_size <- final_dataset %>%
  select(current_vaping_device_type, current_eliquid_bottle_size_mls) # 1 missing 

median_eliquid_bottle_size <- median_eliquid_bottle_size %>% 
  mutate(
    current_eliquid_bottle_size_mls = case_when(
      is.na(current_eliquid_bottle_size_mls) & str_detect(current_vaping_device_type, "open pod|tank") ~
        median(current_eliquid_bottle_size_mls[str_detect(current_vaping_device_type, "open pod|tank")], na.rm = TRUE),
      TRUE ~ current_eliquid_bottle_size_mls  # Keep original value if it's not NA or condition is not met
    )
  )

final_dataset$current_eliquid_bottle_size_mls <- median_eliquid_bottle_size$current_eliquid_bottle_size_mls


# check current time to finish eliquid 

curr_time_to_finish_eliquid_missing <- final_dataset %>% 
  select(current_eliquid_bottle_size_mls, current_time_to_finish_eliquid_bottle_days) # 6 missing 


curr_time_to_finish_eliquid_missing <- curr_time_to_finish_eliquid_missing %>%
  mutate(
    current_time_to_finish_eliquid_bottle_days = ifelse(
      is.na(current_time_to_finish_eliquid_bottle_days), 
      median(current_time_to_finish_eliquid_bottle_days, na.rm = TRUE),  # Impute the median for NA values
      current_time_to_finish_eliquid_bottle_days  # Keep the original value if it's not NA or they didn't vape
    )
  )
curr_time_to_finish_eliquid_missing <- curr_time_to_finish_eliquid_missing %>%
  mutate(
    current_time_to_finish_eliquid_bottle_days = case_when(
      is.na(current_eliquid_bottle_size_mls) ~ NA_real_,  # Use NA_real_ to set to NA for numeric columns
      TRUE ~ current_time_to_finish_eliquid_bottle_days  # Keep existing values for others
    )
  )

final_dataset$current_time_to_finish_eliquid_bottle_days <- curr_time_to_finish_eliquid_missing$current_time_to_finish_eliquid_bottle_days




# current 
current_disposable_puff_missing <- final_dataset %>% 
  select(current_disposable_device_puffs, current_vaping_device_type) # 1 missing 

current_disposable_puff_missing <- current_disposable_puff_missing %>% 
  mutate(
    current_disposable_device_puffs = case_when(
      is.na(current_disposable_device_puffs) & str_detect(current_vaping_device_type, "disposable") ~
        median(current_disposable_device_puffs[str_detect(current_vaping_device_type, "disposable")], na.rm = TRUE),
      TRUE ~ current_disposable_device_puffs  # Keep original value if it's not NA or condition is not met
    )
  )
final_dataset$current_disposable_device_puffs <- current_disposable_puff_missing$current_disposable_device_puffs


# sort out missing data for time to finish disposable 

current_time_to_finish_disposable_missing <- final_dataset %>% 
  select(current_disposable_device_puffs, current_time_taken_to_finish_disposable_days) # 3 missing 

current_time_to_finish_disposable_missing <- current_time_to_finish_disposable_missing  %>%
  mutate(
    current_time_taken_to_finish_disposable_days  = ifelse(
      is.na(current_time_taken_to_finish_disposable_days), 
      median(current_time_taken_to_finish_disposable_days, na.rm = TRUE),  # Impute the median for NA values
      current_time_taken_to_finish_disposable_days  # Keep the original value if it's not NA or they didn't vape
    )
  )

current_time_to_finish_disposable_missing <- current_time_to_finish_disposable_missing %>%
  mutate(
    current_time_taken_to_finish_disposable_days = case_when(
      is.na(current_disposable_device_puffs) ~ NA_real_,  # Use NA_real_ to set to NA for numeric columns
      TRUE ~ current_time_taken_to_finish_disposable_days   # Keep existing values for others
    )  )

final_dataset$current_time_taken_to_finish_disposable_days <- current_time_to_finish_disposable_missing$current_time_taken_to_finish_disposable_days



# closed pod puffs current 

current_pod_puff_missing <- final_dataset %>% 
  select(current_pod_puffs, current_vaping_device_type) # 12 missing 

current_pod_puff_missing <- current_pod_puff_missing %>% 
  mutate(
    current_pod_puffs = case_when(
      is.na(current_pod_puffs) & str_detect(current_vaping_device_type, "closed pod") ~
        median(current_pod_puffs[str_detect(current_vaping_device_type, "closed pod")], na.rm = TRUE),
      TRUE ~ current_pod_puffs # Keep original value if it's not NA or condition is not met
    )
  )

final_dataset$current_pod_puffs <- current_pod_puff_missing$current_pod_puffs



# time to finish pods current 
current_time_to_finish_pod_missing <- final_dataset %>% 
  select(current_pod_puffs, current_time_taken_to_finish_pod_days) # 6 missing 

current_time_to_finish_pod_missing <- current_time_to_finish_pod_missing %>%
  mutate(
    current_time_taken_to_finish_pod_days  = ifelse(
      is.na(current_time_taken_to_finish_pod_days), 
      median(current_time_taken_to_finish_pod_days, na.rm = TRUE),  # Impute the median for NA values
      current_time_taken_to_finish_pod_days  # Keep the original value if it's not NA or they didn't vape
    )
  )

current_time_to_finish_pod_missing <- current_time_to_finish_pod_missing %>%
  mutate(
    current_time_taken_to_finish_pod_days = case_when(
      is.na(current_pod_puffs) ~ NA_real_,  # Use NA_real_ to set to NA for numeric columns
      TRUE ~ current_time_taken_to_finish_pod_days  # Keep existing values for others
    )  )

final_dataset$current_time_taken_to_finish_pod_days <- current_time_to_finish_pod_missing$current_time_taken_to_finish_pod_days

# recalculate usage frequencies 

final_dataset <- final_dataset %>% 
  mutate(
    current_eliquid_avg_daily_ml_consumption = current_eliquid_bottle_size_mls / current_time_to_finish_eliquid_bottle_days,
    current_pod_avg_daily_puffs = current_pod_puffs / current_time_taken_to_finish_pod_days,
    current_disposable_avg_daily_puffs = current_disposable_device_puffs / current_time_taken_to_finish_disposable_days)

# convert puffs to ml  current_pod_avg_daily_ml_consumption =

final_dataset <- final_dataset %>%
  mutate(
    current_pod_avg_daily_ml_consumption = current_pod_avg_daily_puffs * (1/300),
    current_disposable_avg_daily_ml_consumption = current_disposable_avg_daily_puffs * (1/300)
  )


final_dataset <- final_dataset %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# calculate averages 
final_dataset <- final_dataset %>%
  rowwise() %>%
  mutate(
    beliefs_average = round(mean(c(beliefs_1, beliefs_2, beliefs_3, beliefs_4), na.rm = TRUE), 2)
  ) %>%
  ungroup()

final_dataset <- final_dataset %>%
  rowwise() %>%
  mutate(
    self_transcendence = round(mean(c_across(c(biospheric_1, biospheric_2, biospheric_3, biospheric_4,
                                               altruistic_1, altruistic_2, altruistic_3,
                                               altruistic_4, altruistic_5)), na.rm = TRUE),2),
    self_enhancement = round(mean(c_across(c(hedonistic_1, hedonistic_2, hedonistic_3,
                                             egoistic_1, egoistic_2, egoistic_3, 
                                             egoistic_4, egoistic_5)), na.rm = TRUE),2)) %>%
  ungroup()

# calculate average beliefs and attitudes 
final_dataset <- final_dataset %>%
  rowwise() %>%
  mutate(
    attitudes_average = round(mean(c_across(c(attitude_1, attitude_2, attitude_3,
                                              attitude_4, attitude_5, attitude_6)), na.rm = TRUE), 2)) %>%
  ungroup()



write.csv(final_dataset, "final_dataset_missing_data_handled.csv")






