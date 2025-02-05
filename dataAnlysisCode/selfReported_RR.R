library(robustlmm)
library(emmeans)

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



#### Psychological Safety
model <- rlmer(PS ~ Condition * Role + (1 | Participant), data = data)

summary(model)

# Post-hoc tests
emm_PS <- emmeans(model, ~ Condition | Role)
pairs(emm_PS)

# Post-hoc tests
emm_PS <- emmeans(model, ~ Role | Condition)
pairs(emm_PS)



#### 
model <- rlmer( ~ Condition * Role + (1 | Participant), data = data)

summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)



#### Marginalization
model <- rlmer(M1 ~ Condition * Role + (1 | Participant), data = data)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)



#### PTDP1
model <- rlmer(PTDP1 ~ Condition * Role + (1 | Participant), data = data)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)



#### PTDP2
model <- rlmer(PTDP2 ~ Condition * Role + (1 | Participant), data = data)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)



#### PTDP3
model <- rlmer(PTDP3 ~ Condition * Role + (1 | Participant), data = data)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)



#### PTDP4
model <- rlmer(PTDP4 ~ Condition * Role + (1 | Participant), data = data)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)



#### PTDP5
model <- rlmer(PTDP5 ~ Condition * Role + (1 | Participant), data = data)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)



#### PDOQ1
model <- rlmer(PDOQ1 ~ Condition * Role + (1 | Participant), data = data)

summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)



#### PDOQ2
model <- rlmer(PDOQ2 ~ Condition * Role + (1 | Participant), data = data)

summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)



#### NASA1
model <- rlmer(NASA1 ~ Condition * Role + (1 | Participant), data = data)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)



#### NASA2
model <- rlmer(NASA2 ~ Condition * Role + (1 | Participant), data = data)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)



#### NASA3
model <- rlmer(NASA3 ~ Condition * Role + (1 | Participant), data = data)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)



#### NASA4
model <- rlmer(NASA4 ~ Condition * Role + (1 | Participant), data = data)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)



#### NASA5
model <- rlmer(NASA5 ~ Condition * Role + (1 | Participant), data = data)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Condition | Role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ Role | Condition)
pairs(emm)