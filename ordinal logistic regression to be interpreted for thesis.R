# Load in packages 
require(MASS)
require(Hmisc)
require(reshape2)
library(ordinal)
library(VGAM)
library(brant)    # Brant test
library(dplyr)
library(tidyverse)
library(stringr)
install.packages("sjPlot")
library(sjPlot)
detach("package:car", unload = TRUE)
library(tidyr)
library(ggplot2)

# load in final dataset 
final_dataset <- read.csv("final_dataset_missing_data_handled.csv")

# Assign device type based on highest usage
devicetypes <- final_dataset %>%
  mutate(
    device_type = case_when(
      max_usage_ml == disposable_usage_ml ~ 1,
      max_usage_ml == closedpod_usage_ml ~ 2,
      max_usage_ml == openpod_usage_ml ~ 3,
      max_usage_ml == tank_usage_ml ~ 4,
      TRUE ~ NA_real_
    )
  )

# Convert to ordered factor
devicetypes$device_type <- factor(
  devicetypes$device_type,
  levels = c(1, 2, 3, 4),
  labels = c("disposable", "closed pod", "open pod", "tank"),
  ordered = TRUE
)


# Check outcome distribution
table(devicetypes$device_type)

# Create model dataset
olr_data <- devicetypes %>%
  select(
    device_type,
    knowledge_sum,
    beliefs_average,
    attitudes_average,
    self_transcendence,
    self_enhancement,
    current_nicotine_level_mg_ml
  ) %>%
  filter(complete.cases(.))

# Fit ordinal logistic regression
olr_model <- MASS::polr(
  device_type ~ 
    knowledge_sum +
    beliefs_average +
    attitudes_average +
    self_transcendence +
    self_enhancement +
    current_nicotine_level_mg_ml,
  data = olr_data,
  Hess = TRUE
)

# Results
summary(olr_model)

# Proportional odds assumption
brant(olr_model)

# Odds ratios
exp(coef(olr_model))

# Confidence intervals
exp(confint(olr_model))

# Model fit
AIC(olr_model)

# fit ppo model 
ppo_model <- vglm(
  device_type ~
    knowledge_sum +
    beliefs_average +
    attitudes_average +
    self_transcendence +
    self_enhancement +
    current_nicotine_level_mg_ml,
  family = cumulative(
    parallel =
      TRUE ~ beliefs_average +
      attitudes_average +
      self_enhancement,
    reverse = FALSE
  ),
  data = olr_data
)

summary(ppo_model)

# Generate prediction data across the observed range of nicotine strength
newdat <- data.frame(
  knowledge_sum = mean(olr_data$knowledge_sum),
  beliefs_average = mean(olr_data$beliefs_average),
  attitudes_average = mean(olr_data$attitudes_average),
  self_transcendence = mean(olr_data$self_transcendence),
  self_enhancement = mean(olr_data$self_enhancement),
  current_nicotine_level_mg_ml = seq(
    min(olr_data$current_nicotine_level_mg_ml),
    max(olr_data$current_nicotine_level_mg_ml),
    length.out = 200
  )
)
# Generate predicted probabilities from the proportional odds model
pred <- predict(
  ppo_model,
  newdata = newdat,
  type = "response"
)

# Combine prediction data with predicted probabilities
plot_dat <- cbind(newdat, pred)

# Label predicted probability columns by device type
names(plot_dat)[7:10] <- c(
  "Disposable",
  "Closed pod",
  "Open pod",
  "Tank"
)

# Reshape predicted probabilities for plotting
plot_dat <- pivot_longer(
  plot_dat,
  cols = Disposable:Tank,
  names_to = "Device",
  values_to = "Probability"
)

# Plot predicted probability of device type across nicotine strength
ggplot(plot_dat,
       aes(x = current_nicotine_level_mg_ml,
           y = Probability,
           colour = Device)) +
  
  geom_line(linewidth = 1.3) +
  
  scale_colour_manual(values = c(
    "Disposable" = "#E69F00",
    "Closed pod" = "#009E73",
    "Open pod" = "#7570B3",
    "Tank" = "#E7298A"
  )) +
  
  labs(
    title = "Predicted Probability of Current Vaping Device Type\nAcross Nicotine Strength",
    x = "Nicotine Strength (mg/ml)",
    y = "Predicted Probability",
    colour = "Device Type"
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

# Inspect predicted probability columns and device type frequencies
colnames(pred)
table(olr_data$device_type)

# Plot observed proportion of device types across nicotine strength
ggplot(olr_data,
       aes(x = current_nicotine_level_mg_ml,
           fill = device_type)) +
  geom_histogram(binwidth = 2, position = "fill") +
  labs(
    x = "Nicotine strength (mg/ml)",
    y = "Proportion",
    fill = "Device type"
  ) +
  theme_classic()
