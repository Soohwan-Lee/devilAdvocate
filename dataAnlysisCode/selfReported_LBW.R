library(robustlmm)
library(emmeans)
library(readr)
library(dplyr)
library(tidyr)

# Import the data
data <- read.csv("./Desktop/github/devilAdvocate/data/selfReported/selfReported_AB_LBW_mean.csv")

# Data preprocessing
data <- data %>%
  mutate(
    Condition = factor(condition, levels = c("A", "B")),
    Role = factor(role, levels = c("Junior", "Senior")),
    Task = factor(task),
    Group = factor(groupNum),
    Participant = factor(subjectNum)
  )



#### Psychological Safety
model <- rlmer(PS ~ Condition * Role + (1 | Participant), data = data)

summary(model)

# Post-hoc tests
emm_PS <- emmeans(model, ~ Condition | Role)
pairs(emm_PS)

# Post-hoc tests
emm_PS <- emmeans(model, ~ Role | Condition)
pairs(emm_PS)




#### PTDP
model <- rlmer(PTDP ~ Condition * Role + (1 | Participant), data = data)

summary(model)

# Post-hoc tests
emm_PS <- emmeans(model, ~ Condition | Role)
pairs(emm_PS)

# Post-hoc tests
emm_PS <- emmeans(model, ~ Role | Condition)
pairs(emm_PS)





#### PDOQ
model <- rlmer(PDOQ ~ Condition * Role + (1 | Participant), data = data)

summary(model)

# Post-hoc tests
emm_PS <- emmeans(model, ~ Condition | Role)
pairs(emm_PS)

# Post-hoc tests
emm_PS <- emmeans(model, ~ Role | Condition)
pairs(emm_PS)




#### NASA
model <- rlmer(NASA ~ Condition * Role + (1 | Participant), data = data)

summary(model)

# Post-hoc tests
emm_PS <- emmeans(model, ~ Condition | Role)
pairs(emm_PS)

# Post-hoc tests
emm_PS <- emmeans(model, ~ Role | Condition)
pairs(emm_PS)




### PAA
result <- wilcox.test(PAA ~ Role, data = data, alternative = "two.sided")
print(result)
