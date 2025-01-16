# Install and load necessary packages
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(readr)
library(dplyr)

# Import the data
data <- read.csv("./Desktop/github/devilAdvocate/data/selfReported/selfReported_cleaned.csv")

# Assume 'data' is your full dataset
# Filter data for Conditions B and C
data_paa <- data %>%
  filter(condition %in% c("B", "C"))

# Ensure factors are correctly specified
data_paa <- data_paa %>%
  mutate(
    Condition = factor(condition, levels = c("B", "C")),
    Role = factor(role, levels = c("Junior", "Senior")),
    Task = factor(task),
    Group = factor(groupNum),
    Participant = factor(subjectNum)
  )

#######################
##### PAA1 #####
# Fit the LMM
model <- lm(PAA1 ~ Condition * Role, data = data_paa)

# Summary of the model
summary(model)

#######################
# Check model assumptions
residuals <- residuals(model)
qqnorm(residuals)
qqline(residuals)
shapiro.test(residuals)

plot(fitted(model), residuals)
abline(h = 0, col = "red")

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model)

#######################
# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm, adjust = "tukey")

#######################
# For differences between roles across conditions
emm_role <- emmeans(model, ~ Role | Condition)
pairs(emm_role, adjust = "tukey")



#######################
##### PAA2 #####
# Fit the LMM
model <- lm(PAA2 ~ Condition * Role, data = data_paa)

# Summary of the model
summary(model)

#######################
# Check model assumptions
residuals <- residuals(model)
qqnorm(residuals)
qqline(residuals)
shapiro.test(residuals)

plot(fitted(model), residuals)
abline(h = 0, col = "red")

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model)

#######################
# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm, adjust = "tukey")

#######################
# For differences between roles across conditions
emm_role <- emmeans(model, ~ Role | Condition)
pairs(emm_role, adjust = "tukey")


#######################
##### PAA3 #####
# Fit the LMM
model <- lm(PAA3 ~ Condition * Role, data = data_paa)

# Summary of the model
summary(model)

#######################
# Check model assumptions
residuals <- residuals(model)
qqnorm(residuals)
qqline(residuals)
shapiro.test(residuals)

plot(fitted(model), residuals)
abline(h = 0, col = "red")

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model)

#######################
# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm, adjust = "tukey")

#######################
# For differences between roles across conditions
emm_role <- emmeans(model, ~ Role | Condition)
pairs(emm_role, adjust = "tukey")



#######################
##### PAA4 #####
# Fit the LMM
model <- lm(PAA4 ~ Condition * Role, data = data_paa)

# Summary of the model
summary(model)
anova(model)

#######################
# Check model assumptions
residuals <- residuals(model)
qqnorm(residuals)
qqline(residuals)
shapiro.test(residuals)

plot(fitted(model), residuals)
abline(h = 0, col = "red")

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model)

#######################
# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm, adjust = "tukey")

#######################
# For differences between roles across conditions
emm_role <- emmeans(model, ~ Role | Condition)
pairs(emm_role, adjust = "tukey")
