# Install and load necessary packages
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(readr)
library(dplyr)
library(nparLD)
library(effectsize)
library(MuMIn)
library(ordinal)



# Import the data
data <- read.csv("./Desktop/github/devilAdvocate/data/selfReported/selfReported_cleaned.csv")

# Data preprocessing
data <- data %>%
  mutate(
    Condition = factor(condition, levels = c("A", "B", "C")),
    Role = factor(role, levels = c("Junior", "Senior")),
    Task = factor(task),
    Group = factor(groupNum),
    Participant = factor(subjectNum)
  )



##### Psychological Safety #####
# Fit the LMM for PS
model_PS <- lmer(PS ~ Condition * Role + (1 | Participant), data = data)
# Summary of the model
summary(model_PS)

### Effect size
# 1A. Compute standardized coefficients:
# By default, effectsize::standardize_parameters() uses partial standardization
std_coef <- standardize_parameters(model_PS)
std_coef

# 1B. Compute marginal and conditional R-squared:
#  - Marginal R^2: proportion of variance explained by fixed effects alone
#  - Conditional R^2: proportion of variance explained by both fixed + random effects
r2_vals <- r.squaredGLMM(model_PS)
r2_vals


###########################
# Check model assumptions
residuals_PS <- residuals(model_PS)
qqnorm(residuals_PS)
qqline(residuals_PS)
shapiro.test(residuals_PS)

plot(fitted(model_PS), residuals_PS)
abline(h = 0, col = "red")

###########################
# Post-hoc tests
emm_PS <- emmeans(model_PS, ~ Condition | Role)
pairs(emm_PS, adjust = "tukey")

###########################
# For differences between roles across conditions
emm_PS_role <- emmeans(model_PS, ~ Role | Condition)
pairs(emm_PS_role, adjust = "tukey")



###########################
##### Marginality #####
# Fit the LMM for PS
model <- lmer(M1 ~ Condition * Role + (1 | Participant), data = data)

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
##### PTDP1 #####
# Fit the LMM for PS
model <- lmer(PTDP1 ~ Condition * Role + (1 | Participant), data = data)

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
##### PTDP2 #####
# Fit the LMM for PS
model <- lmer(PTDP2 ~ Condition * Role + (1 | Participant), data = data)

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
##### PTDP3 #####
# Fit the LMM for PS
model <- lmer(PTDP3 ~ Condition * Role + (1 | Participant), data = data)

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
##### PTDP4 #####
# Fit the LMM
model <- lmer(PTDP4 ~ Condition * Role + (1 | Participant), data = data)

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
##### PTDP5 #####
# Fit the LMM
model <- lmer(PTDP5 ~ Condition * Role + (1 | Participant), data = data)

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
##### PDOQ1#####
# Fit the LMM
model <- lmer(PDOQ1 ~ Condition * Role + (1 | Participant), data = data)

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
##### PDOQ2 #####
# Fit the LMM
model <- lmer(PDOQ2 ~ Condition * Role + (1 | Participant), data = data)

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
##### NASA1 #####
# Fit the LMM
model <- lmer(NASA1 ~ Condition * Role + (1 | Participant), data = data)

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
##### NASA2 #####
# Fit the LMM
model <- lmer(NASA2 ~ Condition * Role + (1 | Participant), data = data)

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
##### NASA3 #####
# Fit the LMM
model <- lmer(NASA3 ~ Condition * Role + (1 | Participant), data = data)

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
##### NASA4 #####
# Fit the LMM
model <- lmer(NASA4 ~ Condition * Role + (1 | Participant), data = data)

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
##### NASA5 #####
# Fit the LMM
model <- lmer(NASA5 ~ Condition * Role + (1 | Participant), data = data)


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

