# Load in packages 
library(dplyr)
library(Hmisc)
install.packages("magrittr")
library(magrittr)
library(car)
install.packages("moments")
library(moments)
install.packages("interactions")
library(interactions)
library(lmtest)

# Read in final dataset 
final_dataset <- read.csv("final_dataset_missing_data_handled.csv")

# Set device type as a factor 
final_dataset$device_type <- factor(final_dataset$device_type)

# Set open-pod devices as a reference point 
final_dataset$device_type <- relevel(
  final_dataset$device_type,
  ref = "open pod"
)

# Select out needed variables 
sem_data <- final_dataset %>%
  dplyr::select(current_nicotine_level_mg_ml, max_usage_ml, device_type, 
         self_transcendence, self_enhancement, knowledge_sum, beliefs_average, attitudes_average) %>% 
  filter(!is.na(max_usage_ml))

# Set the model 
sem_data %$% lm(current_nicotine_level_mg_ml~ attitudes_average + knowledge_sum + beliefs_average + self_transcendence +self_enhancement + max_usage_ml + device_type) %>% summary

# Optional- Test out the models 
m1 <- lm(current_nicotine_level_mg_ml ~  device_type + max_usage_ml + attitudes_average * self_transcendence, data = sem_data)
m2 <- lm(current_nicotine_level_mg_ml ~ device_type + max_usage_ml + beliefs_average * self_transcendence, data = sem_data)
m3 <- lm(current_nicotine_level_mg_ml ~ device_type + max_usage_ml + knowledge_sum * self_transcendence, data = sem_data)
m4 <- lm(current_nicotine_level_mg_ml ~ device_type + max_usage_ml + attitudes_average * self_enhancement, data = sem_data)
m5 <- lm(current_nicotine_level_mg_ml ~ device_type + max_usage_ml + beliefs_average * self_enhancement, data = sem_data)
m6 <- lm(current_nicotine_level_mg_ml ~ device_type + max_usage_ml + knowledge_sum * self_enhancement, data = sem_data)
m7 <- lm(current_nicotine_level_mg_ml~ device_type + attitudes_average * max_usage_ml, data = sem_data)
m8 <- lm(current_nicotine_level_mg_ml ~ device_type + beliefs_average * max_usage_ml, data = sem_data)
m9 <- lm(current_nicotine_level_mg_ml ~ device_type + knowledge_sum * max_usage_ml, data = sem_data)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)
summary(m9)

# Mean centre variables 

sem_data <- sem_data %>%
  mutate(
    attitudes_average_c   = scale(attitudes_average, center = TRUE, scale = FALSE),
    beliefs_average_c     = scale(beliefs_average, center = TRUE, scale = FALSE),
    knowledge_sum_c       = scale(knowledge_sum, center = TRUE, scale = FALSE),
    self_transcendence_c  = scale(self_transcendence, center = TRUE, scale = FALSE),
    self_enhancement_c    = scale(self_enhancement, center = TRUE, scale = FALSE), 
    max_usage_ml_c    = scale(max_usage_ml, center = TRUE, scale = FALSE),
  )

# Set the model with mean centres 
full_model_centred <- lm(current_nicotine_level_mg_ml ~
  device_type +
  max_usage_ml_c +
  attitudes_average_c + 
  beliefs_average_c + 
  knowledge_sum_c + 
  self_transcendence_c + 
  self_enhancement_c +   
  attitudes_average_c * self_transcendence_c +
  beliefs_average_c   * self_transcendence_c +
  knowledge_sum_c     * self_transcendence_c +
  attitudes_average_c * self_enhancement_c +
  beliefs_average_c   * self_enhancement_c +
  knowledge_sum_c     * self_enhancement_c +
  attitudes_average_c * max_usage_ml_c +
  beliefs_average_c   * max_usage_ml_c +
  knowledge_sum_c     * max_usage_ml_c, 
  data = sem_data)

# Check the summary of the model 
summary(full_model_centred)

## Assumption checks 
# Extract residuals
resid_model_centred <- residuals(full_model_centred)

# Histogram of residuals
hist(resid_model_centred, main = "Histogram of Residuals", xlab = "Residuals")

# Q-Q plot of residuals
qqnorm(resid_model_centred)
qqline(resid_model_centred, col = "red")

# residuals vs fitted values 
plot(fitted(full_model_centred), resid_model_centred,
     main = "Residuals vs Fitted",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")

# Normal distribution
shapiro.test(resid_model_centred)

# Breusch-pagan 
bptest(full_model_centred)

# VIF values
vif(full_model_centred)

# Harvey-Collier
harvtest(full_model_centred)

# Correlation matrix 
predictors <- sem_data %>%
  dplyr::select(attitudes_average_c, beliefs_average_c, knowledge_sum_c,
                self_transcendence_c, self_enhancement_c, max_usage_ml_c)

rcorr(as.matrix(predictors))

## Checking for outliers 

cooks <- cooks.distance(full_model_centred)
plot(cooks, type = "h", main = "Cook's Distance")
abline(h = 4/length(cooks), col = "red", lty = 2)
which(cooks > 4/length(cooks))

lev <- hatvalues(full_model_centred)
plot(lev, type="h", main="Leverage (Hat Values)")
abline(h = 2*mean(lev), col="red", lty=2)
which(lev > 2*mean(lev))

# Compute thresholds
cook_thresh <- 4/length(cooks)
lev_thresh  <- 2 * mean(lev)

# Points that violate both
both_bad <- which(cooks > cook_thresh & lev > lev_thresh)

# Plot only the bad points
plot(lev[both_bad], cooks[both_bad],
     xlab = "Leverage",
     ylab = "Cook's Distance",
     main = "Points Violating Both Cooks Distance and Leverage Thresholds",
     pch = 19, col = "blue")

# Input reference lines
abline(v = lev_thresh, col="red", lty=2)
abline(h = cook_thresh, col="red", lty=2)

text(lev[both_bad], cooks[both_bad], labels = both_bad, pos = 3)

length(both_bad)

# Re-fit the model with the outliers removed 
clean_model <- lm(current_nicotine_level_mg_ml ~
                        device_type +
                        max_usage_ml_c +
                        attitudes_average_c + 
                        beliefs_average_c + 
                        knowledge_sum_c + 
                        self_transcendence_c + 
                        self_enhancement_c +   
                        attitudes_average_c * self_transcendence_c +
                        beliefs_average_c   * self_transcendence_c +
                        knowledge_sum_c     * self_transcendence_c +
                        attitudes_average_c * self_enhancement_c +
                        beliefs_average_c   * self_enhancement_c +
                        knowledge_sum_c     * self_enhancement_c +
                        attitudes_average_c * max_usage_ml_c +
                        beliefs_average_c   * max_usage_ml_c +
                        knowledge_sum_c     * max_usage_ml_c, data = sem_data[-both_bad, ])
summary(clean_model)

## Re-check assumptions 

# Histogram of residuals
resid_clean_model <- residuals(clean_model)
hist(resid_clean_model, main = "Histogram of Residuals", xlab = "Residuals")

# Q-Q plot of residuals
qqnorm(resid_clean_model)
qqline(resid_clean_model, col = "red")

# Residuals vs Fitted 

plot(fitted(clean_model), resid_clean_model,
     main = "Residuals vs Fitted",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")

shapiro.test(resid_clean_model)
bptest(clean_model)
summary(clean_model)
harvtest(clean_model)
confint(clean_model)









