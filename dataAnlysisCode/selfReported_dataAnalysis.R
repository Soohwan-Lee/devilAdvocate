library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
# install.packages()
# Assuming 'Outcome' is your dependent variable and 'your_data' is your dataset


# Data Load
selfReportedData <- read.csv(file = "./Desktop/github/devilAdvocate/sampleDataGenerator/selfReported_results.csv", header=T, fileEncoding="UTF-8-BOM")

# Set Factor
selfReportedData$condition <- as.factor(selfReportedData$condition)
selfReportedData$subjectType <- as.factor(selfReportedData$subjectType)
selfReportedData$task <- as.factor(selfReportedData$task)
selfReportedData$group <- as.factor(selfReportedData$group)
selfReportedData$subject <- as.factor(selfReportedData$subject)


### Reliability Analysis & Factor Analysis
# Fit the linear mixed model
model <- lmer(selfReportedData$PS1 ~ condition * subjectType + (1 | group/subject) + (1 | task), data = selfReportedData)

# Summarize the model
summary(model)

# ANOVA table for fixed effects (Note: type = 3 specifies Type III sums of squares, appropriate when the model includes interactions.)
anova(model)

# Check random effects
ranef(model)

# Plot residuals to check assumptions
plot(model)


### Post-hoc test
# Compute estimated marginal means for the interaction
emms <- emmeans(model, ~ condition * subjectType)

# Pairwise comparisons between Minority and Majority within each Condition
pairwise_by_condition <- contrast(emms, method = "pairwise", by = "condition", adjust = "bonferroni")
print(pairwise_by_condition)

# Pairwise comparisons between Conditions within each SubjectType
pairwise_by_subjectType <- contrast(emms, method = "pairwise", by = "subjectType", adjust = "bonferroni")
print(pairwise_by_subjectType)



### Data Visualization
# ggplot example 
ggplot(selfReportedData, aes(x = condition, y = PS1, color = subjectType)) +
  stat_summary(fun = mean, geom = "line", aes(group = subjectType)) +
  stat_summary(fun = mean, geom = "point") +
  labs(title = "Interaction between Condition and SubjectType on PS1",
       x = "Condition", y = "Mean PS1 Score", color = "Subject Type")

