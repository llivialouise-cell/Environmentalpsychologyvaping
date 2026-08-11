final_dataset <- read.csv("final_dataset_missing_data_handled.csv")



library(dplyr)
library(tidyr)
library(ggplot2)

device_counts <- final_dataset %>%
  separate_rows(current_vaping_device_type, sep = ",") %>%
  count(current_vaping_device_type) %>%
  filter(current_vaping_device_type != "other")

device_counts

ggplot(device_counts, 
       aes(x = reorder(current_vaping_device_type, -n), 
           y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Current vaping device type",
    y = "Number of users"
  ) +
  theme_classic()
ggplot(device_counts, 
       aes(x = reorder(current_vaping_device_type, -n), 
           y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Current vaping device type",
    y = "Number of users"
  ) +
  theme_classic()


library(ggplot2)
table(final_dataset$current_vaping_device_type)
device_table <- table(final_dataset$current_vaping_device_type)
device_table <- as.data.frame(device_table)

colnames(device_table) <- c("current_vaping_device_type", "n")


library(ggplot2)

ggplot(device_table,
       aes(x = reorder(current_vaping_device_type, n),
           y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, max(device_table$n) +10, by = 10)
  ) +
  labs(
    title = "Current Device Type Combination Frequencies",
    x = "Device type combination",
    y = "Frequency"
  ) +
  theme_classic()
