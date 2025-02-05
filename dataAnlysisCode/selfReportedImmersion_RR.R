library(robustlmm)
library(emmeans)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)




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
task1_data$subjectNum = as.factor(task1_data$subjectNum)

#### Task1
model <- rlm(score ~ Choice * role, data = task1_data)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Choice | role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ role | Choice)
pairs(emm)



#########################


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


#### Task2
model <- rlm(score ~ Choice * role, data = task2_data)
summary(model)

# Post-hoc tests
emm <- emmeans(model, ~ Choice | role)
pairs(emm)

# Post-hoc tests
emm <- emmeans(model, ~ role | Choice)
pairs(emm)

