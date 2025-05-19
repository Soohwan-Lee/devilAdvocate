library(robustlmm)
library(emmeans)
library(broom)
library(ggplot2)
library(ggpubr)   # stat_pvalue_manual()
library(tidyr)   # add this near the other library() calls
library(stringr)   # ← add this with the other library() calls
library(ggsignif)
library(dplyr)


# Import the data
data <- read.csv("./Desktop/github/devilAdvocate/data/dialogue_calculatedMetric/engagement_scores_changeJunior.csv")

# Data preprocessing
data <- data %>%
  mutate(
    Condition = factor(condition, levels = c("A", "B", "C"),labels = c("Baseline", "AIGC", "AIMM")),
    Role = factor(role, levels = c("Junior", "Senior")),
    Task = factor(task),
    Group = factor(groupNum),
    Participant = factor(sender)
  )



##### Final Drawing


### Function for Drawing Figure
draw_significance_plot <- function(size_type = 2, y_var = "PS") {
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
        TRUE ~ ""
      ),
      p_label = case_when(
        p.value < 0.0001 ~ "p<0.0001 ***",
        p.value < 0.001 ~ paste0("p=", sprintf("%.4f", p.value), " ***"),
        p.value < 0.01 ~ paste0("p=", sprintf("%.4f", p.value), " **"),
        p.value < 0.05 ~ paste0("p=", sprintf("%.4f", p.value), " *"),
        TRUE ~ ""
      ),
      is_significant = p.value < 0.05
    )
  
  # 데이터 범위 자동 계산
  y_min <- floor(min(data[[y_var]], na.rm = TRUE))
  y_max_data <- ceiling(max(data[[y_var]], na.rm = TRUE))
  
  # 여백을 최소화하거나 제거 (outlier가 있다면 살짝만 여백 추가)
  has_outliers <- any(boxplot.stats(data[[y_var]])$out > y_max_data)
  if (has_outliers) {
    # outlier가 있을 경우 아주 작은 여백만 추가
    padding <- (y_max_data - y_min) * 0.02
  } else {
    # outlier가 없을 경우 여백 없음
    padding <- 0
  }
  y_max <- y_max_data + padding
  
  # 변수별 맞춤 y축 눈금 설정
  if (y_var == "M") {
    # 메시지 수는 0부터 50까지 5 단위로
    y_breaks <- seq(0, 50, by = 5)
    y_min <- 0
    y_max <- 50 + padding  # 여백 추가
  } else if (y_var == "C") {
    # 문자 수는 0부터 1750까지 250 단위로
    y_breaks <- seq(0, 1750, by = 250)
    y_min <- 0
    y_max <- 1750 + padding  # 여백 추가
  } else {
    # 다른 변수는 데이터 범위에 따라 자동 설정
    if (y_max_data <= 10) {
      y_breaks <- seq(y_min, y_max_data, by = 1)
    } else if (y_max_data <= 50) {
      y_breaks <- seq(y_min, y_max_data, by = 5)
    } else if (y_max_data <= 200) {
      y_breaks <- seq(y_min, y_max_data, by = 20)
    } else if (y_max_data <= 1000) {
      y_breaks <- seq(y_min, y_max_data, by = 100)
    } else {
      y_breaks <- seq(y_min, y_max_data, by = 200)
    }
  }
  
  # 시각화 생성
  p_with_signif <- ggplot(data, aes(x = Condition, y = !!sym(y_var), fill = Role)) + 
    geom_boxplot(position = position_dodge(0.65), width = 0.6, outlier.shape = 1, outlier.size = 2) +
    stat_summary(fun = mean, geom = "line", aes(group = Role, color = Role), position = position_dodge(0.65), size = 0.8) +
    scale_fill_manual(name = NULL, values = c(Senior = "#e8f6ff", Junior = "#ffe2f2")) +
    scale_color_manual(name = NULL, values = c(Senior = "#2529c3", Junior = "#ff0090")) +
    scale_y_continuous(breaks = y_breaks, limits = c(y_min, y_max), expand = expansion(mult = c(0, 0))) +
    labs(y = y_axis, x = "System Condition") +
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
  
  # 유의성 브라켓 함수
  add_signif_brackets <- function(plot, filter_pattern, condition1, condition2, base_y_pos_factor, color, is_condition_comp = TRUE, role = NULL) {
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
      
      # 데이터 범위에 맞게 y_position 조정
      # base_y_pos_factor는 0과 1 사이의 값으로, 데이터 최대값으로부터의 상대적 위치
      y_position <- y_max_data + (base_y_pos_factor * padding)
      
      # 빈 p_label은 표시하지 않음
      if (pairs_df$p_label[matching_rows[1]] != "") {
        plot <- plot + geom_signif(
          xmin = xmin, 
          xmax = xmax, 
          y_position = y_position, 
          annotations = pairs_df$p_label[matching_rows[1]], 
          tip_length = 0.02, 
          color = color
        )
      }
    }
    return(plot)
  }
  
  # 유의성 브라켓 위치를 데이터 범위에 맞게 조정
  # 이제 상대적인 위치로 계산 (최대값의 일정 비율)
  sig_specs <- list(
    list("Baseline.*Junior - Baseline.*Senior", 1, 1, 0.3, "black", FALSE, NULL),
    list("AIGC.*Junior - AIGC.*Senior", 2, 2, 0.3, "black", FALSE, NULL),
    list("AIMM.*Junior - AIMM.*Senior", 3, 3, 0.3, "black", FALSE, NULL),
    list("Baseline.*Junior - AIGC.*Junior", 1, 2, 0.5, "#ff0090", TRUE, "Junior"),
    list("AIGC.*Junior - AIMM.*Junior", 2, 3, 0.5, "#ff0090", TRUE, "Junior"),
    list("Baseline.*Junior - AIMM.*Junior", 1, 3, 0.7, "#ff0090", TRUE, "Junior"),
    list("Baseline.*Senior - AIGC.*Senior", 1, 2, 0.5, "#2529c3", TRUE, "Senior"),
    list("AIGC.*Senior - AIMM.*Senior", 2, 3, 0.5, "#2529c3", TRUE, "Senior"),
    list("Baseline.*Senior - AIMM.*Senior", 1, 3, 0.7, "#2529c3", TRUE, "Senior")
  )
  
  # 유의성 표시가 필요한지 확인 (유의미한 비교가 있는지)
  has_significant_pairs <- any(pairs_df$is_significant)
  
  # 유의성 표시가 필요한 경우에만 브라켓 추가
  if (has_significant_pairs) {
    # 각 비교 패턴에 대해 브라켓 추가
    for (spec in sig_specs) {
      p_with_signif <- add_signif_brackets(p_with_signif, spec[[1]], spec[[2]], spec[[3]], spec[[4]], spec[[5]], spec[[6]], spec[[7]])
    }
    
    # 유의미한 비교만 출력
    print("유의미한 비교:")
    sig_pairs <- pairs_df[pairs_df$is_significant, c("contrast", "p_label")]
    print(sig_pairs)
  } else {
    print("유의미한 비교가 없습니다.")
  }
  
  # Width/Height 결정
  if (size_type == 1) {
    pdf_width <- 4.3
    pdf_height <- 4
  } else {
    pdf_width <- 3.7
    pdf_height <- 4
  }
  
  output_path <- paste0("./Desktop/github/devilAdvocate/dataVisualization/Final_dialogue/", file_name, ".pdf")
  
  ggsave(filename = output_path, plot = p_with_signif, width = pdf_width, height = pdf_height, device = "pdf", dpi = 300)
  message("PDF 저장 완료: ", output_path)
  
  # 시각화 객체 반환
  return(p_with_signif)
}


#### Messages
y_axis <- "Number of Messages"
file_name <- "M"
model <- rlmer(M ~ Condition * Role + (1 | Participant), data = data)
summary(model)
emm_all <- emmeans(model, ~ Condition * Role)
pairs(emm_all, adjust = "bonferroni")
draw_significance_plot(1, "M")




#### Character
y_axis <- "Number of Characters"
file_name <- "C"
model <- rlmer(C ~ Condition * Role + (1 | Participant), data = data)
summary(model)
emm_all <- emmeans(model, ~ Condition * Role)
pairs(emm_all, adjust = "bonferroni")
draw_significance_plot(1, "C")


# 
# #### NES_M(0.4)_C(0.7=6)
# model <- rlmer(NES_m4 ~ Condition * Role + (1 | Participant), data = data)
# summary(model)
# 
# # Post-hoc tests
# emm <- emmeans(model, ~ Condition | Role)
# pairs(emm)
# 
# # Post-hoc tests
# emm <- emmeans(model, ~ Role | Condition)
# pairs(emm)