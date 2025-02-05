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
library(brant)
library(brms)




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


################
### cumulative link mixed model
data$PS_ordinal <- factor(data$PS, 
                          ordered = TRUE, 
                          levels = c(1,2,3,4,5,6,7))
model_ordinal <- clmm(PS_ordinal ~ Condition * Role + (1 | Participant),
                      data = data, 
                      Hess = TRUE)  # Hess=TRUE to get variance-covariance info for summary

summary(model_ordinal)

nominal_test(model_ordinal)

# 고정 효과 모형 적합
model_clm <- clm(PS ~ Condition * Role, data = data)

# 비례 오즈 가정 검정
test_nominal <- nominal_test(model_clm)
print(test_nominal)

# 비비례 오즈 모형 적합
model_nonproportional <- clm(PS_ordinal ~ Condition * Role, nominal = ~ Role, data = data)

summary(model_nonproportional)

# 부분 비례
model_partial <- clmm(
  PS ~ Condition * Role + (1 | Participant),
  data = data,
  parallel = c(TRUE, FALSE) # Assuming 'Role' violates
)

summary(model_partial)

### ordinal post-hoc
# 모형 계수 추출
coef_table <- summary(model)$coefficients

# 오즈비와 신뢰구간 계산
odds_ratios <- exp(coef_table[, "Estimate"])
conf_int <- exp(confint(model))

# 결과 정리
effect_size <- data.frame(
  Estimate = coef_table[, "Estimate"],
  OR = odds_ratios,
  `2.5 %` = conf_int[, 1],
  `97.5 %` = conf_int[, 2],
  `p-value` = coef_table[, "Pr(>|z|)"]
)

print(effect_size)


# For probabilities (cumulative or category-specific), you can try:
emm_ord_resp <- emmeans(model_ordinal, ~ Condition | Role, type = "response")
emm_ord_resp

emm_ord_resp <- emmeans(model_ordinal, ~ Role | Condition, type = "response")
emm_ord_resp







##############
# Aligned Rank Transform을 사용한 분석
library(ARTool)
art_model <- art(PS ~ Condition * Role + (1 | Participant), data = data)
anova(art_model)

# For interaction effects
art.con(art_model, "Condition:Role", adjust = "bonferroni")
art.con(art_model, "Condition:Role")

# For main effects
art.con(art_model, "Condition")
art.con(art_model, "Role")



####################
#### Robust LMM
library(robustlmm)
model <- rlmer(PS ~ Condition * Role + (1 | Participant), data = data)

summary(model)

# Post-hoc tests
emm_PS <- emmeans(model, ~ Condition | Role)
pairs(emm_PS)

# Post-hoc tests
emm_PS <- emmeans(model, ~ Role | Condition)
pairs(emm_PS)




#################
### bayesian analysis

# Fit a Bayesian cumulative ordinal mixed model
model_bayes <- brm(
  formula = PS ~ Condition * Role + (1 | Participant),
  data = data,
  family = cumulative(link = "logit"),
  chains = 4,
  cores = 4,
  iter = 2000
)

# Summary of the model
summary(model_bayes)

# Effect size and posterior intervals
posterior_summary(model_bayes)

# Plotting posterior distributions
plot(model_bayes)

# Posterior predictive checks
pp_check(model_bayes)

# Estimated marginal means (emmeans equivalent)
library(emmeans)
emmeans_bayes <- emmeans(model_bayes, ~ Condition * Role)

# Contrast analysis
contrast_results_bayes <- contrast(emmeans_bayes, method = "pairwise", by = "Condition")
print(contrast_results_bayes)

# Contrast analysis
contrast_results_bayes <- contrast(emmeans_bayes, method = "pairwise", by = "Role")
print(contrast_results_bayes)


