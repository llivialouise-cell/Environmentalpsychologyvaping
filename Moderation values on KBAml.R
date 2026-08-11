library(tidyverse)
library(magrittr)
library(car)
library(interactions)
library(Hmisc)
install.packages("modelsummary")
library(modelsummary)
library(broom)
library(dplyr)
install.packages("sandwich")
install.packages("lmtest")

library(sandwich)
library(lmtest)

# Import data
final_dataset <- read.csv("final_dataset_missing_data_handled.csv")


class(final_dataset$device_type)
str(final_dataset$device_type)

final_dataset$device_type <- factor(final_dataset$device_type)

final_dataset$device_type <- relevel(
  final_dataset$device_type,
  ref = "open pod"
)

sem_data <- final_dataset %>%
  dplyr::select(
    attitudes_average,
    max_usage_ml,
    device_type,
    self_transcendence,
    self_enhancement,
    knowledge_sum,
    beliefs_average
  ) %>%
  filter(!is.na(max_usage_ml))


sem_data %$% lm(max_usage_ml~ device_type + attitudes_average + knowledge_sum + beliefs_average + self_transcendence +self_enhancement) %>% summary

m1 <- lm(
  max_usage_ml ~
    device_type +
    attitudes_average * self_transcendence +
    knowledge_sum +
    beliefs_average,
  data = sem_data
)

m2 <- lm(
  max_usage_ml ~
    device_type +
    beliefs_average * self_transcendence +
    attitudes_average +
    knowledge_sum,
  data = sem_data
)

m3 <- lm(
  max_usage_ml ~
    device_type +
    knowledge_sum * self_transcendence +
    attitudes_average +
    beliefs_average,
  data = sem_data
)

m4 <- lm(
  max_usage_ml ~
    device_type +
    attitudes_average * self_enhancement +
    knowledge_sum +
    beliefs_average,
  data = sem_data
)

m5 <- lm(
  max_usage_ml ~
    device_type +
    beliefs_average * self_enhancement +
    attitudes_average +
    knowledge_sum,
  data = sem_data
)

m6 <- lm(
  max_usage_ml ~
    device_type +
    knowledge_sum * self_enhancement +
    attitudes_average +
    beliefs_average,
  data = sem_data
)
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)

# Centering variables
sem_data <- sem_data %>%
  mutate(
    attitudes_average_c   = scale(attitudes_average, center = TRUE, scale = FALSE),
    beliefs_average_c     = scale(beliefs_average, center = TRUE, scale = FALSE),
    knowledge_sum_c       = scale(knowledge_sum, center = TRUE, scale = FALSE),
    self_transcendence_c  = scale(self_transcendence, center = TRUE, scale = FALSE),
    self_enhancement_c    = scale(self_enhancement, center = TRUE, scale = FALSE)
  )

full_model <- lm(
  max_usage_ml ~
    device_type +
    attitudes_average_c * self_transcendence_c +
    beliefs_average_c   * self_transcendence_c +
    knowledge_sum_c     * self_transcendence_c +
    attitudes_average_c * self_enhancement_c +
    beliefs_average_c   * self_enhancement_c +
    knowledge_sum_c     * self_enhancement_c,
  data = sem_data
)



resid_model_centred <- residuals(full_model)

# Histogram of residuals
hist(resid_model_centred, main = "Histogram of Residuals", xlab = "Residuals")

# Q-Q plot of residuals
qqnorm(resid_model_centred)
qqline(resid_model_centred, col = "red")

plot(fitted(full_model), resid_model_centred,
     main = "Residuals vs Fitted",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")

shapiro.test(resid_model_centred)


bptest(full_model)

vif(full_model)

harvtest(full_model)

table(sem_data$device_type)
vif(full_model)

predictors <- sem_data %>%
  dplyr::select(attitudes_average_c, beliefs_average_c, knowledge_sum_c,
                self_transcendence_c, self_enhancement_c)

# Correlation matrix with p-values
rcorr(as.matrix(predictors))




cooks <- cooks.distance(full_model)
plot(cooks, type = "h", main = "Cook's Distance")
abline(h = 4/length(cooks), col = "red", lty = 2)
which(cooks > 4/length(cooks))

lev <- hatvalues(full_model)
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
     main = "Points Violating Cook's Distance and Leverage Thresholds",
     pch = 19, col = "blue")

abline(v = lev_thresh, col="red", lty=2)
abline(h = cook_thresh, col="red", lty=2)

text(lev[both_bad], cooks[both_bad], labels = both_bad, pos = 3)

clean_model <- lm(  max_usage_ml ~
                      device_type +
                      attitudes_average_c * self_transcendence_c +
                      beliefs_average_c   * self_transcendence_c +
                      knowledge_sum_c     * self_transcendence_c +
                      attitudes_average_c * self_enhancement_c +
                      beliefs_average_c   * self_enhancement_c +
                      knowledge_sum_c     * self_enhancement_c, data = sem_data[-both_bad, ])
summary(clean_model)


resid_clean_model <- residuals(clean_model)

# Histogram of residuals
hist(resid_clean_model, main = "Histogram of Residuals", xlab = "Residuals")

# Q-Q plot of residuals
qqnorm(resid_clean_model)
qqline(resid_clean_model, col = "red")

plot(fitted(clean_model), resid_clean_model,
     main = "Residuals vs Fitted",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")

shapiro.test(resid_clean_model)


bptest(clean_model)
summary(clean_model)
harvtest(clean_model)

Confint(clean_model)





