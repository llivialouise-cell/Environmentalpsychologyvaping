library(tidyverse)   # read_csv, %>%, select, mutate, filter, ggplot, geom_*
library(lmtest)      # bptest(), coeftest() if used
library(car)         # harvtest(), durbinWatsonTest(), vif()
library(lmtest)      # bptest()
library(mgcv)  # gam(), gam.check()
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
library(ordinal)
library(VGAM)
library(car)      # For VIF
library(brant)    # Brant test
library(dplyr)
library(tidyverse)
library(stringr)


final_dataset <- read.csv("final_dataset_missing_data_handled.csv")

final_dataset %>% 
  group_by(current_vaping_device_type) %>% 
  summarise(n=n())

devicetypes <- final_dataset %>%
  rowwise() %>%
  mutate(
    disposable_usage_ml = ifelse(
      str_detect(current_vaping_device_type, regex("disposable", ignore_case = TRUE)),
      current_disposable_avg_daily_ml_consumption,
      NA_real_
    ),
    
    closedpod_usage_ml = ifelse(
      str_detect(current_vaping_device_type, regex("closed pod", ignore_case = TRUE)),
      current_pod_avg_daily_ml_consumption,
      NA_real_
    ),
    
    # Open pod and tank already in ml → keep as-is
    openpod_usage_ml = ifelse(
      str_detect(current_vaping_device_type, regex("open pod", ignore_case = TRUE)),
      current_eliquid_avg_daily_ml_consumption,
      NA_real_
    ),
    
    tank_usage_ml = ifelse(
      str_detect(current_vaping_device_type, regex("tank", ignore_case = TRUE)),
      current_eliquid_avg_daily_ml_consumption,
      NA_real_
    ),
    
    # Find highest daily use in ml-equivalents
    max_usage_ml = pmax(
      disposable_usage_ml,
      closedpod_usage_ml,
      openpod_usage_ml,
      tank_usage_ml,
      na.rm = TRUE
    ),
    
    # Assign device type based on which usage was highest
    device_type = case_when(
      max_usage_ml == disposable_usage_ml ~ 1,
      max_usage_ml == closedpod_usage_ml ~ 2,
      max_usage_ml == openpod_usage_ml ~ 3,
      max_usage_ml == tank_usage_ml ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()



# Convert numeric device_type to ordered factor for ordinal regression # numerical
devicetypes$device_type <- factor(
  devicetypes$device_type,
  levels = c(1, 2, 3, 4),
  labels = c("disposable", "closed pod", "open pod", "tank")
)

class(devicetypes$device_type)







  

devicetypes %>%
  group_by(device_type) %>%
  summarise(n = n())
  
devicetypes$device_type <- relevel(devicetypes$device_type,
                                   ref = "open pod")

model <- lm(max_usage_ml ~ device_type + knowledge_sum + beliefs_average + attitudes_average, 
              data = devicetypes)
  
summary(model)

levels(devicetypes$device_type)

# checking for outliers 

regression_data <- devicetypes %>%
  filter(complete.cases(max_usage_ml,
                        device_type,
                        knowledge_sum,
                        beliefs_average,
                        attitudes_average)) %>%
  mutate(
    fitted = fitted(model),
    residuals = residuals(model)
  )

ggplot(regression_data, aes(fitted, residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  theme_bw() +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted values",
    y = "Residuals"
  )


harvtest(model) # violated

raintest(model)


hist(residuals(model),
     main = "Histogram of Residuals",
     xlab = "Residuals")

qqnorm(residuals(model))
qqline(residuals(model), col = "red")

shapiro.test(residuals(model))

bptest(model)


vif(model)
cor(regression_data[, c("max_usage_ml",
                        "knowledge_sum",
                        "beliefs_average",
                        "attitudes_average")],
    use = "pairwise.complete.obs")   # does not include device type as this is a factor 


leverages <- hatvalues(model)

# Check the single highest leverage point
high_leverage_point <- which.max(leverages)
print(row.names(regression_data)[high_leverage_point])

# Check which rows in the original data were used in the model
model_rows <- as.numeric(rownames(model$model))

# See if row 240 is included
240  %in% model_rows

# Find the position of row 240 in the model object
pos_in_model <- which(model_rows == 240)

# Leverage of row 240
leverages[pos_in_model]

# Parameters and average leverage
p <- length(coef(model))  # number of parameters
n <- nrow(model$model)    # number of observations used
average_leverage <- p / n

# Plot leverage values
plot(leverages, type = "h", 
     main = "Leverage Values for Each Observation",
     xlab = "Observation", 
     ylab = "Leverage")

# Add threshold lines
abline(h = 2*average_leverage, col = "blue", lty = 2)  # moderate threshold
abline(h = 3*average_leverage, col = "red", lty = 2)   # high threshold

# Highlight high leverage points
high_lev_points <- which(leverages > 3*average_leverage)
points(high_lev_points, leverages[high_lev_points], col = "red", pch = 19)

# Optional: label the high leverage points
text(high_lev_points, leverages[high_lev_points], labels = high_lev_points, pos = 3, col = "red")


# Cook's distance
cooksD <- cooks.distance(model)

# Common rule: points with Cook's D > 4/n might be influential
influential_points <- which(cooksD > 4/n)
influential_points
plot(cooksD, type = "h", main = "Cook's Distance for Each Observation")
abline(h = 4/n, col = "red", lty = 2)

# to see which items need to be removed, plot these both together 

# Step 1: Calculate leverage and Cook's distance
leverages <- hatvalues(model)
cooksD <- cooks.distance(model)
n <- nrow(model$model)          # number of observations
p <- length(coef(model))        # number of predictors including intercept

# Step 2: Identify points with BOTH high leverage AND high Cook's distance
lev_threshold <- 2 * p / n
cook_threshold <- 4 / n
points_to_show <- which(leverages > lev_threshold & cooksD > cook_threshold)

# Step 3: Plot only those points
plot(
  leverages[points_to_show],
  cooksD[points_to_show],
  xlab = "Leverage",
  ylab = "Cook's Distance",
  main = "High Leverage & High Cook's Distance Points",
  pch = 19,
  col = "red",
  xlim = c(min(leverages[points_to_show]) * 0.9, max(leverages[points_to_show]) * 1.1),
  ylim = c(min(cooksD[points_to_show]) * 0.9, max(cooksD[points_to_show]) * 1.1)
)

# Add labels to those points
text(
  x = leverages[points_to_show],
  y = cooksD[points_to_show],
  labels = points_to_show,
  pos = 3,
  col = "darkred"
)

# Optional: add threshold lines (for context)
abline(v = lev_threshold, col = "blue", lty = 2)
abline(h = cook_threshold, col = "blue", lty = 2)

# remove these 
rows_to_remove <- points_to_show

regression2_data <- regression_data[-rows_to_remove, ]


# redo 


model2 <- lm(max_usage_ml~ device_type + knowledge_sum+ attitudes_average+ beliefs_average, data = regression2_data)






regression2_data %>%
  mutate(fitted = fitted(model2)) %>%
  mutate(residuals = residuals(model2)) %>%
  ggplot(aes(fitted, residuals))+
  geom_point()+
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + # horizontal line at
  theme_bw() +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted values",
       y = "Residuals")


harvtest(model2) # violated

harvtest(
  lm(max_usage_ml ~ knowledge_sum +
       attitudes_average +
       beliefs_average,
     data = regression2_data)
)
raintest(model2)


hist(residuals(model2),
     main = "Histogram of Residuals",
     xlab = "Residuals")

qqnorm(residuals(model2))
qqline(residuals(model2), col = "red")

shapiro.test(residuals(model2))

bptest(model2)


vif(model2)
cor(regression2_data[, c("max_usage_ml",
                         "knowledge_sum",
                         "beliefs_average",
                         "attitudes_average")],
    use = "pairwise.complete.obs")


summary(model2)

confint(model2, level =0.95)


final_dataset_with_devices <- devicetypes

write.csv(
  final_dataset_with_devices,
  "final_dataset_missing_data_handled.csv",
  row.names = FALSE
)
