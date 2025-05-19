library(robustlmm)
library(emmeans)
library(broom)
library(ggplot2)
library(ggpubr)   # stat_pvalue_manual()
library(tidyr)   # add this near the other library() calls
library(stringr)   # ← add this with the other library() calls
library(ggsignif)
library(dplyr)
library(MASS)


# Import the data
data <- read.csv("./Desktop/github/devilAdvocate/data/selfReported/selfReported_cleaned.csv")

# Assume 'data' is your full dataset
# Filter data for Conditions B and C
data_paa <- data %>%
  filter(condition %in% c("B", "C"))

# Ensure factors are correctly specified
data_paa <- data_paa %>%
  mutate(
    Condition = factor(condition, levels = c("B", "C"),labels = c("AIGC", "AIMM")),
    Role = factor(role, levels = c("Junior", "Senior")),
    Task = factor(task),
    Group = factor(groupNum),
    Participant = factor(subjectNum)
  )


#### Final Drawing
draw_significance_plot <- function(size_type = 2, y_var = "PAA1") {
  library(emmeans)
  library(ggplot2)
  library(dplyr)
  library(ggsignif)
  
  # Post-hoc test
  emm_all <- emmeans(model, ~ Condition * Role)
  pairs_result <- pairs(emm_all, adjust = "bonferroni")
  pairs_df <- as.data.frame(pairs_result) %>%
    mutate(
      signif = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**", 
        p.value < 0.05 ~ "*",
        TRUE ~ ""  # ns 대신 빈 문자열
      ),
      p_label = case_when(
        p.value < 0.0001 ~ "p<0.0001 ***",
        p.value < 0.001 ~ paste0("p=", sprintf("%.4f", p.value), " ***"),
        p.value < 0.01 ~ paste0("p=", sprintf("%.4f", p.value), " **"),
        p.value < 0.05 ~ paste0("p=", sprintf("%.4f", p.value), " *"),
        TRUE ~ ""  # "p=X.XXXX ns" 대신 빈 문자열로 변경
      ),
      is_significant = p.value < 0.05
    )
  
  # 고정된 y_max 값 설정 - 항상 일정한 여백 유지
  y_max <- 8.6
  
  # y_var 매개변수를 사용하여 변수 동적 선택
  # aes_string 대신 aes 내 !!sym()을 사용
  p_with_signif <- ggplot(data_paa, aes(x = Condition, y = !!sym(y_var), fill = Role)) + 
    geom_boxplot(position = position_dodge(0.65), width = 0.6, outlier.shape = 1, outlier.size = 2) +
    stat_summary(fun = mean, geom = "line", aes(group = Role, color = Role), position = position_dodge(0.65), size = 0.8) +
    scale_fill_manual(name = NULL, values = c(Senior = "#e8f6ff", Junior = "#ffe2f2")) +
    scale_color_manual(name = NULL, values = c(Senior = "#2529c3", Junior = "#ff0090")) +
    scale_y_continuous(breaks = 1:7, limits = c(1, y_max), expand = expansion(mult = c(0, .03))) +
    labs(y = y_axis, x = "System Condition") +  # 외부 정의된 y_axis 변수 사용
    theme_gray(base_size = 12) +
    theme(
      axis.title.x = element_text(size = 14, margin = margin(t = 7)),
      axis.title.y = element_text(size = 14, margin = margin(r = 7)),
      axis.text.x = element_text(size = 13),
      axis.text.y = element_text(size = 13),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.margin = margin(t = -5),
      legend.text = element_text(size = 13)
    )
  
  get_dodge_position <- function(condition_num, role) {
    dodge_width <- 0.65
    if (role == "Junior") return(condition_num - dodge_width / 4)
    else return(condition_num + dodge_width / 4)
  }
  
  # 확실하게 유의미한 경우에만 브라켓 추가하도록 함수 강화
  add_signif_brackets <- function(plot, filter_pattern, condition1, condition2, base_y_pos, color, is_condition_comp = TRUE, role = NULL) {
    # 패턴에 맞고 유의미한(p.value < 0.05) 행만 찾기
    matching_rows <- which(grepl(filter_pattern, pairs_df$contrast) & pairs_df$is_significant)
    
    # 유의미한 결과가 있을 경우에만 브라켓 추가
    if (length(matching_rows) > 0 && pairs_df$is_significant[matching_rows[1]]) {
      if (is_condition_comp && !is.null(role)) {
        xmin <- get_dodge_position(condition1, role)
        xmax <- get_dodge_position(condition2, role)
      } else if (!is_condition_comp) {
        xmin <- condition1 - 0.15
        xmax <- condition1 + 0.15
      } else {
        xmin <- condition1
        xmax <- condition2
      }
      
      # 빈 p_label은 표시하지 않음
      if (pairs_df$p_label[matching_rows[1]] != "") {
        plot <- plot + geom_signif(
          xmin = xmin, 
          xmax = xmax, 
          y_position = base_y_pos, 
          annotations = pairs_df$p_label[matching_rows[1]], 
          tip_length = 0.02, 
          color = color
        )
      }
    }
    return(plot)
  }
  
  within_cond_base_y <- 7.3
  junior_cond_base_y <- 7.9
  senior_cond_base_y <- 7.9
  
  # AIGC와 AIMM 두 조건에 맞게 수정된 유의성 비교 설정
  sig_specs <- list(
    # AIGC 내에서 Junior vs Senior 비교
    list("AIGC.*Junior - AIGC.*Senior", 1, 1, within_cond_base_y, "black", FALSE, NULL),
    # AIMM 내에서 Junior vs Senior 비교
    list("AIMM.*Junior - AIMM.*Senior", 2, 2, within_cond_base_y, "black", FALSE, NULL),
    # Junior: AIGC vs AIMM 비교
    list("AIGC.*Junior - AIMM.*Junior", 1, 2, junior_cond_base_y, "#ff0090", TRUE, "Junior"),
    # Senior: AIGC vs AIMM 비교
    list("AIGC.*Senior - AIMM.*Senior", 1, 2, senior_cond_base_y, "#2529c3", TRUE, "Senior")
  )
  
  # 각 비교 패턴에 대해 브라켓 추가
  for (spec in sig_specs) {
    p_with_signif <- add_signif_brackets(p_with_signif, spec[[1]], spec[[2]], spec[[3]], spec[[4]], spec[[5]], spec[[6]], spec[[7]])
  }
  
  # 유의미한 비교만 출력
  print("유의미한 비교:")
  sig_pairs <- pairs_df[pairs_df$is_significant, c("contrast", "p_label")]
  print(sig_pairs)
  
  # Width/Height 결정
  if (size_type == 1) {
    pdf_width <- 4.3
    pdf_height <- 4
  } else {
    pdf_width <- 3.7
    pdf_height <- 4
  }
  
  output_path <- paste0("./Desktop/github/devilAdvocate/dataVisualization/Final_selfReported/", file_name, ".pdf")
  
  ggsave(filename = output_path, plot = p_with_signif, width = pdf_width, height = pdf_height, device = "pdf", dpi = 300)
  message("PDF 저장 완료: ", output_path)
}


#### PAA1
y_axis <- "Cooperation"
file_name <- "PAA1"
model <- rlm(PAA1 ~ Condition * Role, data = data_paa)
summary(model)
emm_all <- emmeans(model, ~ Condition * Role)
pairs(emm_all, adjust = "bonferroni")
draw_significance_plot(1, "PAA1")



#### PAA2
y_axis <- "Satisfaction"
file_name <- "PAA2"
model <- rlm(PAA2 ~ Condition * Role, data = data_paa)
summary(model)
emm_all <- emmeans(model, ~ Condition * Role)
pairs(emm_all, adjust = "bonferroni")
draw_significance_plot(1, "PAA2")




#### PAA3
y_axis <- "Quality"
file_name <- "PAA3"
model <- rlm(PAA3 ~ Condition * Role, data = data_paa)
summary(model)
emm_all <- emmeans(model, ~ Condition * Role)
pairs(emm_all, adjust = "bonferroni")
draw_significance_plot(1, "PAA3")




#### PAA4
y_axis <- "Fairness"
file_name <- "PAA4"
model <- rlm(PAA4 ~ Condition * Role, data = data_paa)
summary(model)
emm_all <- emmeans(model, ~ Condition * Role)
pairs(emm_all, adjust = "bonferroni")
draw_significance_plot(1, "PAA4")



