# Load required packages
library(tidyverse)
library(psych)
library(car)
library(sandwich)
library(lm.beta)
library(lmtest)
library(ggplot2)

# Load final dataset
final_dataset <- read.csv("final_dataset_missing_data_handled.csv")

# Prepare device type variable
final_dataset$device_type <- factor(final_dataset$device_type)

final_dataset$device_type <- relevel(
  final_dataset$device_type,
  ref = "open pod"
)

# Select variables for regression analysis
regression_data <- final_dataset %>% 
  dplyr::select(max_usage_ml,
         device_type, self_enhancement,
         self_transcendence,
         current_nicotine_level_mg_ml)

# Remove observations with missing usage data
regression_data <- regression_data %>% 
  filter(!is.na(max_usage_ml))

# Fit initial multiple linear regression model
model<- lm(current_nicotine_level_mg_ml ~ max_usage_ml+ device_type +
             self_transcendence+ self_enhancement , data = regression_data)

plot(model)


# Plot residuals against fitted values
regression_data %>%
  mutate(fitted = fitted(model)) %>%
  mutate(residuals = residuals(model)) %>%
  ggplot(aes(fitted, residuals))+
  geom_point()+
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + # horizontal line at
  theme_bw() +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted values",
       y = "Residuals")

## Assumption checks 
harvtest(model)

raintest(model)

hist(residuals(model),
     main = "Histogram of Residuals",
     xlab = "Residuals")

qqnorm(residuals(model))
qqline(residuals(model), col = "red")

shapiro.test(residuals(model))

bptest(model)

cor(regression_data[, c("current_nicotine_level_mg_ml",
                        "max_usage_ml",
                        "self_transcendence",
                        "self_enhancement")],
    use = "pairwise.complete.obs")

durbinWatsonTest(model)
vif(model)

summary(model)

# Check for outliers 

leverages <- hatvalues(model)

# Check the single highest leverage point
high_leverage_point <- which.max(leverages)
print(row.names(regression_data)[high_leverage_point])

# Check which rows in the original data were used in the model
model_rows <- as.numeric(rownames(model$model))

# See if row 247 is included
247  %in% model_rows

# Find the position of row 301 in the model object
pos_in_model <- which(model_rows == 247)

# Leverage of row 301
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
  col = "darkred",
  cex = 0.5
)

# Optional: add threshold lines (for context)
abline(v = lev_threshold, col = "blue", lty = 2)
abline(h = cook_threshold, col = "blue", lty = 2)
length(points_to_show)

# remove these 
rows_to_remove <- points_to_show

regression2_data <- regression_data[-rows_to_remove, ]

# fit model with outliers removed 
model2<- lm(current_nicotine_level_mg_ml ~ max_usage_ml+ device_type +
              self_transcendence+ self_enhancement , data = regression2_data)
## assumption checks 

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

harvtest(model2)
raintest(model2)

hist(residuals(model2),
     main = "Histogram of Residuals",
     xlab = "Residuals")

qqnorm(residuals(model2))
qqline(residuals(model2), col = "red")

shapiro.test(residuals(model2))

bptest(model2)

durbinWatsonTest(model2)
vif(model2)
cor(regression2_data[, c("current_nicotine_level_mg_ml",
                         "max_usage_ml",
                         "self_transcendence",
                         "self_enhancement")],
    use = "pairwise.complete.obs")

summary(model2)

confint(model2, level = 0.95)


