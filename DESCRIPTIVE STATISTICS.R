final_dataset <- read.csv("final_dataset_missing_data_handled.csv")



participants_all_missing <- final_dataset %>%
  filter(if_all(
    c(disposable_usage_ml, closedpod_usage_ml, openpod_usage_ml, tank_usage_ml),
    is.na
  ))

model_dataset <- final_dataset %>%
  filter(current_vaping_device_type != "other")

participants_all_missing <- model_dataset %>%
  filter(if_all(
    c(disposable_usage_ml, closedpod_usage_ml, openpod_usage_ml, tank_usage_ml),
    is.na
  ))

library(dplyr)

gender_table <- model_dataset %>%
  count(gender) %>%
  mutate(
    Percent = round(n / sum(n) * 100, 1)
  )

gender_table

location_table <- model_dataset %>%
  count(location) %>%
  mutate(
    Percent = round(n / sum(n) * 100, 1)
  )

location_table

model_dataset %>%
  summarise(
    Min = min(age, na.rm = TRUE),
    Max = max(age, na.rm = TRUE),
    Mean = round(mean(age, na.rm = TRUE), 1),
    SD = round(sd(age, na.rm = TRUE), 1)
  )

values <- final_dataset %>% 
  count(self_transcendence) # nothing missing 

values <- final_dataset %>% 
  count(self_enhancement)

hist((final_dataset$self_transcendence),
     main = "Histogram of Self-transcendence",
     xlab = "Self-transcendence score")

Self_transcendence <- final_dataset %>%
  summarise(
    Min = min(self_transcendence, na.rm = TRUE),
    Max = max(self_transcendence, na.rm = TRUE),
    Mean = round(mean(self_transcendence, na.rm = TRUE), 1),
    SD = round(sd(self_transcendence, na.rm = TRUE), 1)
  )



hist((final_dataset$self_enhancement),
     main = "Histogram of Self-enhancement",
     xlab = "Self-enhancement score")

self_enhancement <- final_dataset %>%
  summarise(
    Min = min(self_enhancement, na.rm = TRUE),
    Max = max(self_enhancement, na.rm = TRUE),
    Mean = round(mean(self_enhancement, na.rm = TRUE), 1),
    SD = round(sd(self_enhancement, na.rm = TRUE), 1)
  )


hist((final_dataset$beliefs_average),
     main = "Histogram of Average Beliefs Score",
     xlab = "Beliefs score")

beliefs_average <- final_dataset %>%
  summarise(
    Min = min(beliefs_average, na.rm = TRUE),
    Max = max(beliefs_average, na.rm = TRUE),
    Mean = round(mean(beliefs_average, na.rm = TRUE), 1),
    SD = round(sd(beliefs_average, na.rm = TRUE), 1)
  )

hist((final_dataset$knowledge_sum),
     main = "Histogram of Knowledge Score",
     xlab = "Knowledge score")

knowledge_sum <- final_dataset %>%
  summarise(
    Min = min(knowledge_sum, na.rm = TRUE),
    Max = max(knowledge_sum, na.rm = TRUE),
    Mean = round(mean(knowledge_sum, na.rm = TRUE), 1),
    SD = round(sd(knowledge_sum, na.rm = TRUE), 1)
  )

hist((final_dataset$attitudes_average),
     main = "Histogram of Average Attitudes Score",
     xlab = "Attitudes score")

attitudes_average<- final_dataset %>%
  summarise(
    Min = min(attitudes_average, na.rm = TRUE),
    Max = max(attitudes_average, na.rm = TRUE),
    Mean = round(mean(attitudes_average, na.rm = TRUE), 1),
    SD = round(sd(attitudes_average, na.rm = TRUE), 1)
  )

hist((final_dataset$current_nicotine_level_mg_ml),
     main = "Histogram of Current Nicotine Levels",
     xlab = "Nicotine levels")

hist(final_dataset$current_nicotine_level_mg_ml,
     main = "Histogram of Current Nicotine Levels",
     xlab = "Nicotine levels",
     xaxt = "n")   # turn off default x‑axis

axis(side = 1,
     at = seq(0,
              max(final_dataset$current_nicotine_level_mg_ml, na.rm = TRUE),
              by = 5))


current_nicotine_level_mg_ml<- final_dataset %>%
  summarise(
    Min = min(current_nicotine_level_mg_ml, na.rm = TRUE),
    Max = max(current_nicotine_level_mg_ml, na.rm = TRUE),
    Mean = round(mean(current_nicotine_level_mg_ml, na.rm = TRUE), 1),
    SD = round(sd(current_nicotine_level_mg_ml, na.rm = TRUE), 1)
  )

write.csv(model_dataset,
          "final_dataset_missing_data_handled.csv",
          row.names = FALSE)

library(psych)
# calculating cronbachs alphas 
belief_items <- final_dataset %>%
  dplyr::select(beliefs_1, beliefs_2, beliefs_3, beliefs_4)

alpha(belief_items)


attitudes_items <- final_dataset %>%
  dplyr::select(attitude_1, attitude_2, attitude_3, attitude_4,
                attitude_5, attitude_6)

alpha(attitudes_items)

knowledge_items <- final_dataset %>%
  dplyr::select(knowledge_1: knowledge_12)

alpha(knowledge_items)


self_transcendence_items <- final_dataset %>%
  dplyr::select(altruistic_1, altruistic_2, altruistic_3,
                altruistic_4, altruistic_5, biospheric_1,
                biospheric_2, biospheric_3, biospheric_4)

alpha(self_transcendence_items)

self_enhancement_items <- final_dataset %>%
  dplyr::select(hedonistic_1, hedonistic_2, hedonistic_3,
                egoistic_1, egoistic_2, egoistic_3, egoistic_4,
                egoistic_5)

alpha(self_enhancement_items)


