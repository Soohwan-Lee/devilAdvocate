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
data <- read.csv("./Desktop/github/devilAdvocate/data/selfReported/selfReported_cleaned.csv")

# Data preprocessing
data <- data %>%
  mutate(
    Condition = factor(condition, levels = c("A", "B", "C"),labels = c("Baseline", "AIGC", "AIMM")),
    Role = factor(role, levels = c("Junior", "Senior")),
    Task = factor(task),
    Group = factor(groupNum),
    Participant = factor(subjectNum)
  )



# # Drawing Graph without line
# p <- ggplot(data, aes(Condition, PS, fill = Role)) + 
#   geom_boxplot(
#     position      = position_dodge(width = 0.65),  # Senior 왼쪽 ‑ Junior 오른쪽
#     width         = 0.6,
#     alpha         = 1,
#     outlier.shape = 1,
#     outlier.size  = 2
#   ) +
#   scale_fill_manual(             # 범례 제목을 없애기 위해 name = NULL
#     name   = NULL,
#     values = c(Senior = "#e8f6ff",
#                Junior = "#ffe2f2")
#   ) +
#   scale_y_continuous(
#     breaks = 1:7,
#     limits = c(1, 7),
#     expand = expansion(mult = c(0, .03))
#   ) +
#   labs(
#     y = "Psychological Safety",
#     x = "System Condition"
#   ) +
#   theme_gray(base_size = 12) +   # 기본 회색 테마
#   theme(
#     # 축 라벨 글꼴 크기·여백 조정
#     axis.title.x = element_text(size = 13, margin = margin(t = 7)),
#     axis.title.y = element_text(size = 13, margin = margin(r = 7)),
#     
#     # 세로 그리드 제거
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor   = element_blank(),
#     
#     legend.position = "bottom",
#     legend.margin = margin(t = -5)
#   )
# 
# print(p)

# #### Drawing graph with interaction effect
# p <- ggplot(data, aes(Condition, PS, fill = Role)) +
#   geom_boxplot(
#     position      = position_dodge(width = 0.65),
#     width         = 0.6,
#     alpha         = 1,
#     outlier.shape = 1,
#     outlier.size  = 2
#   ) +
#   # 중앙값을 잇는 실선만 추가
#   stat_summary(
#     fun       = mean,
#     geom      = "line",
#     aes(group = Role, color = Role),
#     position  = position_dodge(width = 0.65),
#     size      = 0.8
#   ) +
#   scale_fill_manual(
#     name   = NULL,
#     values = c(Senior = "#e8f6ff", Junior = "#ffe2f2")
#   ) +
#   scale_color_manual(
#     name   = NULL,
#     values = c(Senior = "#2529c3", Junior = "#ff0090")
#   ) +
#   scale_y_continuous(
#     breaks = 1:7,
#     limits = c(1, 7),
#     expand = expansion(mult = c(0, .03))
#   ) +
#   labs(
#     y = "Psychological Safety",
#     x = "System Condition"
#   ) +
#   theme_gray(base_size = 12) +
#   theme(
#     axis.title.x       = element_text(size = 13, margin = margin(t = 7)),
#     axis.title.y       = element_text(size = 13, margin = margin(r = 7)),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor   = element_blank(),
#     legend.position    = "bottom",
#     legend.margin      = margin(t = -5)
#   )
# 
# print(p)


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
  p_with_signif <- ggplot(data, aes(x = Condition, y = !!sym(y_var), fill = Role)) + 
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
  
  sig_specs <- list(
    list("Baseline.*Junior - Baseline.*Senior", 1, 1, within_cond_base_y, "black", FALSE, NULL),
    list("AIGC.*Junior - AIGC.*Senior", 2, 2, within_cond_base_y, "black", FALSE, NULL),
    list("AIMM.*Junior - AIMM.*Senior", 3, 3, within_cond_base_y, "black", FALSE, NULL),
    list("Baseline.*Junior - AIGC.*Junior", 1, 2, junior_cond_base_y, "#ff0090", TRUE, "Junior"),
    list("AIGC.*Junior - AIMM.*Junior", 2, 3, junior_cond_base_y, "#ff0090", TRUE, "Junior"),
    list("Baseline.*Junior - AIMM.*Junior", 1, 3, junior_cond_base_y + 0.5, "#ff0090", TRUE, "Junior"),
    list("Baseline.*Senior - AIGC.*Senior", 1, 2, senior_cond_base_y, "#2529c3", TRUE, "Senior"),
    list("AIGC.*Senior - AIMM.*Senior", 2, 3, senior_cond_base_y, "#2529c3", TRUE, "Senior"),
    list("Baseline.*Senior - AIMM.*Senior", 1, 3, senior_cond_base_y + 0.5, "#2529c3", TRUE, "Senior")
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
  
  output_path <- paste0("./Desktop/github/devilAdvocate/dataVisualization/selfReportedFinal/", file_name, ".pdf")
  
  ggsave(filename = output_path, plot = p_with_signif, width = pdf_width, height = pdf_height, device = "pdf", dpi = 300)
  message("PDF 저장 완료: ", output_path)
}


#### Psychological Safety
y_axis <- "Psychological Safety"
file_name <- "PS1"
model <- rlmer(PS ~ Condition * Role + (1 | Participant), data = data)
summary(model)
draw_significance_plot(1, "PS")



#### Marginalization
y_axis <- "Marginalization"
file_name <- "PS2"
model <- rlmer(M1 ~ Condition * Role + (1 | Participant), data = data)
summary(model)
draw_significance_plot(1, "M1")




#### PTDP1
y_axis <- "Overall Experience"
file_name <- "PTDP1"
model <- rlmer(PTDP1 ~ Condition * Role + (1 | Participant), data = data)
summary(model)
draw_significance_plot(2,"PTDP1")



#### PTDP2
y_axis <- "Influence"
file_name <- "PTDP2"
model <- rlmer(PTDP2 ~ Condition * Role + (1 | Participant), data = data)
summary(model)
draw_significance_plot(2,"PTDP2")



#### PTDP3
y_axis <- "Cooperation"
file_name <- "PTDP3"
model <- rlmer(PTDP3 ~ Condition * Role + (1 | Participant), data = data)
summary(model)
draw_significance_plot(2,"PTDP3")



#### PTDP4
y_axis <- "Support from Teammates"
file_name <- "PTDP4"
model <- rlmer(PTDP4 ~ Condition * Role + (1 | Participant), data = data)
summary(model)
draw_significance_plot(2,"PTDP4")



#### PTDP5
y_axis <- "Diversity of Opinion"
file_name <- "PTDP5"
model <- rlmer(PTDP5 ~ Condition * Role + (1 | Participant), data = data)
summary(model)
draw_significance_plot(2,"PTDP5")



#### PDOQ1
y_axis <- "Satisfaction"
file_name <- "PDOQ1"
model <- rlmer(PDOQ1 ~ Condition * Role + (1 | Participant), data = data)
summary(model)
draw_significance_plot(1,"PDOQ1")



#### PDOQ2
y_axis <- "Feasibility"
file_name <- "PDOQ2"
model <- rlmer(PDOQ2 ~ Condition * Role + (1 | Participant), data = data)
summary(model)
draw_significance_plot(1,"PDOQ2")



#### NASA1
y_axis <- "Mental Demand"
file_name <- "NASA1"
model <- rlmer(NASA1 ~ Condition * Role + (1 | Participant), data = data)
summary(model)
# emm_all <- emmeans(model, ~ Condition * Role)
# pairs(emm_all, adjust = "bonferroni")
draw_significance_plot(2,"NASA1")



#### NASA2
y_axis <- "Temporal Demand"
file_name <- "NASA2"
model <- rlmer(NASA2 ~ Condition * Role + (1 | Participant), data = data)
summary(model)
# emm_all <- emmeans(model, ~ Condition * Role)
# pairs(emm_all, adjust = "bonferroni")
draw_significance_plot(2,"NASA2")
# print(p_with_signif)



#### NASA3
y_axis <- "Performance"
file_name <- "NASA3"
model <- rlmer(NASA3 ~ Condition * Role + (1 | Participant), data = data)
summary(model,"NASA3")
draw_significance_plot(2,"NASA3")



#### NASA4
y_axis <- "Effort"
file_name <- "NASA4"
model <- rlmer(NASA4 ~ Condition * Role + (1 | Participant), data = data)
summary(model)
# emm_all <- emmeans(model, ~ Condition * Role)
# pairs(emm_all, adjust = "bonferroni")
draw_significance_plot(2,"NASA4")



#### NASA5
y_axis <- "Frustration"
file_name <- "NASA5"
model <- rlmer(NASA5 ~ Condition * Role + (1 | Participant), data = data)
summary(model)
draw_significance_plot(2,"NASA5")





##########################################
# ### Drawing Final Graph
# # Get the pairwise comparisons with p-values (Post-hoc Test)
# emm_all <- emmeans(model, ~ Condition * Role)
# pairs_result <- pairs(emm_all, adjust = "bonferroni")
# pairs_df <- as.data.frame(pairs_result)
# 
# # Format p-values and add significance stars
# pairs_df <- pairs_df %>%
#   mutate(
#     signif = case_when(
#       p.value < 0.001 ~ "***",
#       p.value < 0.01 ~ "**", 
#       p.value < 0.05 ~ "*",
#       TRUE ~ "ns"
#     ),
#     p_label = case_when(
#       p.value < 0.0001 ~ "p<0.0001 ***",
#       p.value < 0.001 ~ paste0("p=", sprintf("%.4f", p.value), " ***"),
#       p.value < 0.01 ~ paste0("p=", sprintf("%.4f", p.value), " **"),
#       p.value < 0.05 ~ paste0("p=", sprintf("%.4f", p.value), " *"),
#       TRUE ~ paste0("p=", sprintf("%.4f", p.value), " ns")
#     ),
#     # Flag to identify significant comparisons
#     is_significant = p.value < 0.05
#   )
# 
# # 유의미한 비교의 수를 세어 y축 높이를 동적으로 조정
# sig_count <- sum(pairs_df$is_significant)
# y_max <- max(7.5, 6.5 + (sig_count * 0.2))
# 
# # Create base plot with dynamically extended y-axis
# p_with_signif <- ggplot(data, aes(Condition, PS, fill = Role)) + 
#   geom_boxplot(
#     position = position_dodge(width = 0.65),
#     width = 0.6,
#     alpha = 1,
#     outlier.shape = 1,
#     outlier.size = 2
#   ) + 
#   # Mean lines
#   stat_summary(
#     fun = mean, 
#     geom = "line", 
#     aes(group = Role, color = Role),
#     position = position_dodge(width = 0.65),
#     size = 0.8
#   ) + 
#   scale_fill_manual(
#     name = NULL,
#     values = c(Senior = "#e8f6ff", Junior = "#ffe2f2")
#   ) + 
#   scale_color_manual(
#     name = NULL,
#     values = c(Senior = "#2529c3", Junior = "#ff0090")
#   ) + 
#   scale_y_continuous(
#     breaks = 1:7,
#     limits = c(1, y_max),  # 동적으로 조정된 높이
#     expand = expansion(mult = c(0, .03))
#   ) + 
#   labs(
#     y = y_axis,
#     x = "System Condition"
#   ) + 
#   theme_gray(base_size = 12) + 
#   theme(
#     axis.title.x = element_text(size = 13, margin = margin(t = 7)),
#     axis.title.y = element_text(size = 13, margin = margin(r = 7)),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor = element_blank(),
#     legend.position = "bottom",
#     legend.margin = margin(t = -5)
#   )
# 
# # 정확한 위치를 계산하기 위한 함수
# get_dodge_position <- function(condition_num, role) {
#   dodge_width <- 0.65  # boxplot의 position_dodge 값과 동일하게 설정
#   # position_dodge를 고려하여 정확한 x축 위치 계산
#   if(role == "Junior") {
#     return(condition_num - (dodge_width/4))  # Junior boxplot 정중앙 (0.325)
#   } else {
#     return(condition_num + (dodge_width/4))  # Senior boxplot 정중앙 (0.325)
#   }
# }
# 
# # 수정된 significance brackets 추가 함수
# add_signif_brackets <- function(plot, filter_pattern, condition1, condition2, base_y_pos, color, is_condition_comp = TRUE, role = NULL) {
#   matching_rows <- which(grepl(filter_pattern, pairs_df$contrast) & pairs_df$is_significant)
#   
#   if(length(matching_rows) > 0) {
#     # 조건 비교인 경우 (특정 역할 내에서)
#     if(is_condition_comp && !is.null(role)) {
#       # 특정 역할 내에서 조건 비교 (정확히 해당 역할의 boxplot 위에 위치)
#       xmin <- get_dodge_position(condition1, role)
#       xmax <- get_dodge_position(condition2, role)
#       y_pos <- base_y_pos
#     } 
#     # Junior vs Senior 비교 (한 조건 내에서)
#     else if(!is_condition_comp) {
#       # 각 조건의 정중앙에 위치하되 너비를 좁게 설정
#       xmin <- condition1 - 0.15  # 너비를 좁게 조정
#       xmax <- condition1 + 0.15  # 너비를 좁게 조정
#       y_pos <- base_y_pos
#     }
#     # 기타 케이스
#     else {
#       xmin <- condition1
#       xmax <- condition2
#       y_pos <- base_y_pos
#     }
#     
#     return(plot + 
#              geom_signif(
#                xmin = xmin, 
#                xmax = xmax,
#                y_position = y_pos,
#                annotations = pairs_df$p_label[matching_rows[1]],
#                tip_length = 0.02,
#                color = color
#              ))
#   }
#   return(plot)
# }
# 
# # Junior vs Senior 비교 (각 조건 내에서)
# within_cond_base_y <- 7.3
# 
# p_with_signif <- add_signif_brackets(
#   p_with_signif, 
#   "Baseline.*Junior - Baseline.*Senior", 
#   1, 1, 
#   within_cond_base_y, 
#   "black",
#   FALSE
# )
# 
# p_with_signif <- add_signif_brackets(
#   p_with_signif, 
#   "AIGC.*Junior - AIGC.*Senior", 
#   2, 2, 
#   within_cond_base_y, 
#   "black",
#   FALSE
# )
# 
# p_with_signif <- add_signif_brackets(
#   p_with_signif, 
#   "AIMM.*Junior - AIMM.*Senior", 
#   3, 3, 
#   within_cond_base_y, 
#   "black",
#   FALSE
# )
# 
# # Junior 내에서 조건 비교 (정확히 Junior boxplot 위에 위치)
# junior_cond_base_y <- 7.9
# 
# # Baseline vs AIGC for Junior
# p_with_signif <- add_signif_brackets(
#   p_with_signif, 
#   "Baseline.*Junior - AIGC.*Junior", 
#   1, 2, 
#   junior_cond_base_y, 
#   "#ff0090",
#   TRUE,
#   "Junior"
# )
# 
# # AIGC vs AIMM for Junior
# p_with_signif <- add_signif_brackets(
#   p_with_signif, 
#   "AIGC.*Junior - AIMM.*Junior", 
#   2, 3, 
#   junior_cond_base_y, 
#   "#ff0090",
#   TRUE,
#   "Junior"
# )
# 
# # Baseline vs AIMM for Junior (높은 아치)
# p_with_signif <- add_signif_brackets(
#   p_with_signif, 
#   "Baseline.*Junior - AIMM.*Junior", 
#   1, 3, 
#   junior_cond_base_y + 0.5, 
#   "#ff0090",
#   TRUE,
#   "Junior"
# )
# 
# # Senior 내에서 조건 비교 (정확히 Senior boxplot 위에 위치)
# senior_cond_base_y <- 7.9
# 
# # Baseline vs AIGC for Senior
# p_with_signif <- add_signif_brackets(
#   p_with_signif, 
#   "Baseline.*Senior - AIGC.*Senior", 
#   1, 2, 
#   senior_cond_base_y, 
#   "#2529c3",
#   TRUE,
#   "Senior"
# )
# 
# # AIGC vs AIMM for Senior
# p_with_signif <- add_signif_brackets(
#   p_with_signif, 
#   "AIGC.*Senior - AIMM.*Senior", 
#   2, 3, 
#   senior_cond_base_y, 
#   "#2529c3",
#   TRUE,
#   "Senior"
# )
# 
# # Baseline vs AIMM for Senior (높은 아치)
# p_with_signif <- add_signif_brackets(
#   p_with_signif, 
#   "Baseline.*Senior - AIMM.*Senior", 
#   1, 3, 
#   senior_cond_base_y + 0.5, 
#   "#2529c3",
#   TRUE,
#   "Senior"
# )
# 
# # 모든 유의미한 비교가 표시되었는지 확인
# print("유의미한 비교:")
# for(i in 1:nrow(pairs_df)) {
#   if(pairs_df$is_significant[i]) {
#     print(paste(pairs_df$contrast[i], ":", pairs_df$p_label[i]))
#   }
# }
# 
# # 최종 도표 출력
# print(p_with_signif)
# 
# # PDF로 저장하기
# # width와 height는 인치 단위로 지정합니다
# #### PTDP & NASA TLX 제외
# pdf_width <- 4.3  # PDF 너비 (인치)
# pdf_height <- 4  # PDF 높이 (인치)
# 
# #### PTDP & NASA TLX 피규어는 아래 사용
# pdf_width <- 3.9  # PDF 너비 (인치)
# pdf_height <- 4  # PDF 높이 (인치)
# 
# #output_path <- "./Desktop/github/devilAdvocate/dataVisualization/selfReportedFinal/" + file_name  # 저장 경로 지정
# output_path <- paste0("./Desktop/github/devilAdvocate/dataVisualization/selfReportedFinal/", file_name,".pdf")
# 
# 
# 
# # PDF 저장 실행
# ggsave(
#   filename = output_path,
#   plot = p_with_signif,
#   width = pdf_width,
#   height = pdf_height,
#   device = "pdf",
#   dpi = 300  # 해상도 설정
# )

