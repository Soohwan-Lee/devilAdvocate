# Install and load necessary packages
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

# Import the data
data <- read.csv("./Desktop/github/devilAdvocate/data/selfReported/selfReportedImmersion.csv")

# Convert necessary columns to factors
data$subjectNum <- as.factor(data$subjectNum)
data$role <- as.factor(data$role)



#### TASK1
# Reshape the data for Task 1
task1_data <- data %>%
  select(subjectNum, role, T1P1, T1P2, T1P3) %>%
  pivot_longer(
    cols = T1P1:T1P3,
    names_to = "Choice",
    names_pattern = "T1P(\\d+)",
    values_to = "score"
  ) %>%
  mutate(Choice = as.factor(Choice))

###########################
# Fit the model for Task 1
task1_model <- lm(score ~ Choice * role, data = task1_data)

# Display summary of the Task 1 model
summary(task1_model)

###########################
# Check model assumptions
residuals <- residuals(task1_model)
qqnorm(residuals)
qqline(residuals)
shapiro.test(residuals)

plot(fitted(task1_model), residuals)
abline(h = 0, col = "red")

###########################
# Post-hoc tests
emm <- emmeans(task1_model, ~ Choice | role)
pairs(emm, adjust = "tukey")

###########################
# For differences between roles across conditions
emm_role <- emmeans(task1_model, ~ role | Choice)
pairs(emm_role, adjust = "tukey")





#### TASK2
# Reshape the data for Task 1
task2_data <- data %>%
  select(subjectNum, role, T2V1, T2V2, T2V3) %>%
  pivot_longer(
    cols = T2V1:T2V3,
    names_to = "Choice",
    names_pattern = "T2V(\\d+)",
    values_to = "score"
  ) %>%
  mutate(Choice = as.factor(Choice))

###########################
# Fit the model for Task 1
task2_model <- lm(score ~ Choice * role, data = task2_data)

# Display summary of the Task 1 model
summary(task2_model)

###########################
# Check model assumptions
residuals <- residuals(task2_model)
qqnorm(residuals)
qqline(residuals)
shapiro.test(residuals)

plot(fitted(task2_model), residuals)
abline(h = 0, col = "red")

###########################
# Post-hoc tests
emm <- emmeans(task2_model, ~ Choice | role)
pairs(emm, adjust = "tukey")

###########################
# For differences between roles across conditions
emm_role <- emmeans(task2_model, ~ role | Choice)
pairs(emm_role, adjust = "tukey")

