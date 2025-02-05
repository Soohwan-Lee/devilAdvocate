library(robustlmm)
library(emmeans)

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




#### PAA1
model <- rlm(PAA1 ~ Condition * Role, data = data_paa)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)




#### PAA2
model <- rlm(PAA2 ~ Condition * Role, data = data_paa)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)



#### PAA3
model <- rlm(PAA3 ~ Condition * Role, data = data_paa)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)



#### PAA4
model <- rlm(PAA4 ~ Condition * Role, data = data_paa)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)
