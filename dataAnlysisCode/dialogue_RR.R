library(robustlmm)
library(emmeans)


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


#### Messages
model <- rlmer(M ~ Condition * Role + (1 | Participant), data = data)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)




#### Character
model <- rlmer(C ~ Condition * Role + (1 | Participant), data = data)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)




#### NES_M(0.4)_C(0.7=6)
model <- rlmer(NES_m4 ~ Condition * Role + (1 | Participant), data = data)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)