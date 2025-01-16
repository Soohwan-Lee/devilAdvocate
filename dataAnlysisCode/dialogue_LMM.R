# Install and load necessary packages
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(readr)
library(dplyr)

# Import the data
data <- read.csv("./Desktop/github/devilAdvocate/data/dialogue_calculatedMetric/engagement_scores_changeJunior.csv")

# Data preprocessing
data <- data %>%
  mutate(
    Condition = factor(condition, levels = c("A", "B", "C")),
    Role = factor(role, levels = c("Junior", "Senior")),
    Task = factor(task),
    Group = factor(groupNum),
    Participant = factor(sender)
  )


###########################
##### Message #####
# Fit the LMM
model <- lmer(M ~ Condition * Role + (1 | Participant), data = data)
# Summary of the model
summary(model)

###########################
# Check model assumptions
residuals <- residuals(model)
qqnorm(residuals)
qqline(residuals)
shapiro.test(residuals)

plot(fitted(model), residuals)
abline(h = 0, col = "red")

###########################
# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm, adjust = "tukey")

###########################
# For differences between roles across conditions
emm_role <- emmeans(model, ~ Role | Condition)
pairs(emm_role, adjust = "tukey")



###########################
##### Character #####
# Fit the LMM
model <- lmer(C ~ Condition * Role + (1ㄴParticipant), data = data)
# Summary of the model
summary(model)

###########################
# Check model assumptions
residuals <- residuals(model)
qqnorm(residuals)
qqline(residuals)
shapiro.test(residuals)

plot(fitted(model), residuals)
abline(h = 0, col = "red")

###########################
# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm, adjust = "tukey")

###########################
# For differences between roles across conditions
emm_role <- emmeans(model, ~ Role | Condition)
pairs(emm_role, adjust = "tukey")




###########################
##### NES #####
# Fit the LMM
model <- lmer(NES_m6 ~ Condition * Role + (1 | Participant), data = data)
# Summary of the model
summary(model)

###########################
# Check model assumptions
residuals <- residuals(model)
qqnorm(residuals)
qqline(residuals)
shapiro.test(residuals)

plot(fitted(model), residuals)
abline(h = 0, col = "red")

###########################
# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm, adjust = "tukey")

###########################
# For differences between roles across conditions
emm_role <- emmeans(model, ~ Role | Condition)
pairs(emm_role, adjust = "tukey")
