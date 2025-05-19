# ### 개선된 시각화 함수 -> 기존과 비슷한 스타일로 시도하던것
# draw_preference_plot <- function(data = task1_data, model, size_type = 2, y_var = "score", y_axis = "Preference", file_name = "task1") {
#   library(emmeans)
#   library(ggplot2)
#   library(dplyr)
#   library(ggsignif)
#   
#   # Choice 변수에 레이블 추가 (T1P1~T1P3 -> Profile 1~Profile 3)
#   modified_data <- data %>%
#     mutate(Choice_label = factor(Choice, 
#                                  levels = c("1", "2", "3"), 
#                                  labels = c("Profile 1", "Profile 2", "Profile 3")))
#   
#   # Post-hoc test (본페로니 보정 적용)
#   emm_all <- emmeans(model, ~ Choice * role)
#   
#   # 대비 결과 확인
#   print("EMM 레벨 확인:")
#   print(summary(emm_all))
#   
#   # 직접 대비 설정
#   pairs_result <- pairs(emm_all, adjust = "bonferroni")
#   
#   # 대비 결과 출력 (디버깅용)
#   print("Pairwise comparisons with Bonferroni adjustment:")
#   print(pairs_result)
#   
#   # 분석 결과 데이터프레임 생성
#   pairs_df <- as.data.frame(pairs_result) %>%
#     mutate(
#       signif = case_when(
#         p.value < 0.001 ~ "***",
#         p.value < 0.01 ~ "**", 
#         p.value < 0.05 ~ "*",
#         TRUE ~ ""  # 유의하지 않을 경우 빈 문자열
#       ),
#       p_label = case_when(
#         p.value < 0.0001 ~ "p<0.0001 ***",
#         p.value < 0.001 ~ paste0("p=", sprintf("%.4f", p.value), " ***"),
#         p.value < 0.01 ~ paste0("p=", sprintf("%.4f", p.value), " **"),
#         p.value < 0.05 ~ paste0("p=", sprintf("%.4f", p.value), " *"),
#         TRUE ~ paste0("p=", sprintf("%.4f", p.value))  # 유의하지 않을 경우 p값만 표시
#       ),
#       is_significant = p.value < 0.05
#     )
#   
#   # 색상 설정 (요청대로 변경)
#   profile_colors <- c("Profile 1" = "#ff0090", "Profile 2" = "#2529c3", "Profile 3" = "#00C853")
#   profile_fill_colors <- c("Profile 1" = "#ffe2f2", "Profile 2" = "#e8f6ff", "Profile 3" = "#E8F5E9")
#   
#   # 고정된 y_max 값 설정 - 항상 일정한 여백 유지
#   y_max <- 8.6
#   
#   # 데이터 시각화
#   p_with_signif <- ggplot(modified_data, aes(x = role, y = !!sym(y_var), fill = Choice_label)) + 
#     geom_boxplot(position = position_dodge(0.65), width = 0.6, outlier.shape = 1, outlier.size = 2) +
#     stat_summary(fun = mean, geom = "point", aes(group = Choice_label, color = Choice_label), 
#                  position = position_dodge(0.65), size = 2) +
#     scale_fill_manual(name = NULL, values = profile_fill_colors) +
#     scale_color_manual(name = NULL, values = profile_colors) +
#     scale_y_continuous(breaks = 1:7, limits = c(1, y_max), expand = expansion(mult = c(0, .03))) +
#     labs(y = y_axis, x = "Role") +
#     theme_gray(base_size = 12) +
#     theme(
#       axis.title.x = element_text(size = 14, margin = margin(t = 7)),
#       axis.title.y = element_text(size = 14, margin = margin(r = 7)),
#       axis.text.x = element_text(size = 13),
#       axis.text.y = element_text(size = 13),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor = element_blank(),
#       legend.position = "bottom",
#       legend.margin = margin(t = -5),
#       legend.text = element_text(size = 13),
#       legend.title = element_blank()
#     )
#   
#   # 유의성 브라켓 추가를 위한 위치 계산 함수
#   get_dodge_position <- function(role_name, profile_num) {
#     dodge_width <- 0.65
#     role_pos <- ifelse(role_name == "Senior", 1, 2)  # Senior=1, Junior=2
#     profile_offset <- c(-dodge_width/3, 0, dodge_width/3)  # Profile 1, 2, 3의 오프셋
#     
#     return(role_pos + profile_offset[profile_num])
#   }
#   
#   # 브라켓 위치 설정
#   prof_vs_prof_y <- c(7.5, 7.9, 8.3)  # Profile 1vs2, 2vs3, 1vs3
#   role_vs_role_y <- 7.1
#   
#   # 특정 대비에 대한 p-값과 유의성을 가져오는 함수
#   get_p_value_info <- function(level1, level2) {
#     # pair_df에서 해당 대비 찾기
#     # 정규식 패턴 생성
#     pattern1 <- paste0(level1, " - ", level2)
#     pattern2 <- paste0(level2, " - ", level1)
#     
#     # 해당 대비 찾기
#     row_idx <- grep(paste0("^(", pattern1, "|", pattern2, ")$"), pairs_df$contrast)
#     
#     if (length(row_idx) > 0) {
#       p_value <- pairs_df$p.value[row_idx[1]]
#       is_sig <- p_value < 0.05
#       p_label <- pairs_df$p_label[row_idx[1]]
#     } else {
#       # 대비를 찾을 수 없는 경우 기본값 설정
#       p_value <- 1.0
#       is_sig <- FALSE
#       p_label <- "p=1.0000"
#       warning(paste("대비를 찾을 수 없음:", level1, "-", level2))
#     }
#     
#     return(list(p_value = p_value, is_significant = is_sig, p_label = p_label))
#   }
#   
#   # 유의성 브라켓 추가 함수
#   add_signif_brackets <- function(plot, level1, level2, xmin, xmax, y_pos, color, 
#                                   show_nonsig = TRUE) {
#     # p-값 정보 가져오기
#     p_info <- get_p_value_info(level1, level2)
#     
#     # 유의하거나, 유의하지 않더라도 표시하도록 설정된 경우
#     if (p_info$is_significant || show_nonsig) {
#       plot <- plot + geom_signif(
#         xmin = xmin, 
#         xmax = xmax, 
#         y_position = y_pos, 
#         annotations = p_info$p_label, 
#         tip_length = 0.02, 
#         color = color
#       )
#     }
#     return(plot)
#   }
#   
#   # 브라켓 추가
#   # Senior 내부 비교 (Profile 1 vs 2, 2 vs 3, 1 vs 3)
#   p_with_signif <- add_signif_brackets(
#     p_with_signif, 
#     "choice 1,Senior", "choice 2,Senior",
#     get_dodge_position("Senior", 1), get_dodge_position("Senior", 2), 
#     prof_vs_prof_y[1], profile_colors["Profile 1"]
#   )
#   
#   p_with_signif <- add_signif_brackets(
#     p_with_signif, 
#     "choice 2,Senior", "choice 3,Senior",
#     get_dodge_position("Senior", 2), get_dodge_position("Senior", 3), 
#     prof_vs_prof_y[2], profile_colors["Profile 2"]
#   )
#   
#   p_with_signif <- add_signif_brackets(
#     p_with_signif, 
#     "choice 1,Senior", "choice 3,Senior",
#     get_dodge_position("Senior", 1), get_dodge_position("Senior", 3), 
#     prof_vs_prof_y[3], profile_colors["Profile 1"]
#   )
#   
#   # Junior 내부 비교 (Profile 1 vs 2, 2 vs 3, 1 vs 3)
#   p_with_signif <- add_signif_brackets(
#     p_with_signif, 
#     "choice 1,Junior", "choice 2,Junior",
#     get_dodge_position("Junior", 1), get_dodge_position("Junior", 2), 
#     prof_vs_prof_y[1], profile_colors["Profile 1"]
#   )
#   
#   p_with_signif <- add_signif_brackets(
#     p_with_signif, 
#     "choice 2,Junior", "choice 3,Junior",
#     get_dodge_position("Junior", 2), get_dodge_position("Junior", 3), 
#     prof_vs_prof_y[2], profile_colors["Profile 2"]
#   )
#   
#   p_with_signif <- add_signif_brackets(
#     p_with_signif, 
#     "choice 1,Junior", "choice 3,Junior",
#     get_dodge_position("Junior", 1), get_dodge_position("Junior", 3), 
#     prof_vs_prof_y[3], profile_colors["Profile 1"]
#   )
#   
#   # 같은 Profile에서 Senior vs Junior 비교 (Profile 1, 2, 3)
#   p_with_signif <- add_signif_brackets(
#     p_with_signif, 
#     "choice 1,Senior", "choice 1,Junior",
#     get_dodge_position("Senior", 1), get_dodge_position("Junior", 1), 
#     role_vs_role_y, profile_colors["Profile 1"]
#   )
#   
#   p_with_signif <- add_signif_brackets(
#     p_with_signif, 
#     "choice 2,Senior", "choice 2,Junior",
#     get_dodge_position("Senior", 2), get_dodge_position("Junior", 2), 
#     role_vs_role_y, profile_colors["Profile 2"]
#   )
#   
#   p_with_signif <- add_signif_brackets(
#     p_with_signif, 
#     "choice 3,Senior", "choice 3,Junior",
#     get_dodge_position("Senior", 3), get_dodge_position("Junior", 3), 
#     role_vs_role_y, profile_colors["Profile 3"]
#   )
#   
#   # 유의미한 비교만 출력
#   print("유의미한 비교:")
#   sig_pairs <- pairs_df[pairs_df$is_significant, c("contrast", "p_label")]
#   print(sig_pairs)
#   
#   # Width/Height 결정
#   if (size_type == 1) {
#     pdf_width <- 4.3
#     pdf_height <- 4
#   } else {
#     pdf_width <- 3.7
#     pdf_height <- 4
#   }
#   
#   output_path <- paste0("./Desktop/github/devilAdvocate/dataVisualization/Final_selfReported_immersion/", file_name, ".pdf")
#   
#   ggsave(filename = output_path, plot = p_with_signif, width = pdf_width, height = pdf_height, device = "pdf", dpi = 300)
#   message("PDF 저장 완료: ", output_path)
#   
#   return(p_with_signif)
# }
# 
# # # 실행 코드
# # # 기존 데이터 전처리 코드는 그대로 유지
# # # task1_data는 이미 생성되어 있음
# # y_axis <- "Preference"
# # file_name <- "task1"
# # model <- rlm(score ~ Choice * role, data = task1_data)
# # preference_plot <- draw_preference_plot(task1_data, model, 1, "score", y_axis, file_name)
# 
# # 실행 코드
# # 기존 데이터 전처리 코드는 그대로 유지
# # task1_data는 이미 생성되어 있음
# y_axis <- "Preference"
# file_name <- "task1"
# model <- rlm(score ~ Choice * role, data = task1_data)
# summary(model)
# emm_all <- emmeans(model, ~ Choice * role)
# pairs(emm_all, adjust = "bonferroni")
# draw_preference_plot(task1_data, model, 1, "score", y_axis, file_name)
# 

################## 개선된 코드 for Task 1
library(robustlmm)
library(emmeans)
library(readr)
library(tidyr)
library(ggplot2)
library(broom)
library(ggpubr)   # stat_pvalue_manual()
library(stringr)
library(ggsignif)
library(dplyr)


# Import the data
data <- read.csv("./Desktop/github/devilAdvocate/data/selfReported/selfReportedImmersion.csv")

# Convert necessary columns to factors
data$subjectNum <- as.factor(data$subjectNum)
data$role <- as.factor(data$role)

#### TASK1
# Reshape the data for Task 1
task1_data <- data %>%
  dplyr::select(subjectNum, role, T1P1, T1P2, T1P3) %>%
  pivot_longer(
    cols = T1P1:T1P3,
    names_to = "Choice",
    names_pattern = "T1P(\\d+)",
    values_to = "score"
  ) %>%
  mutate(Choice = as.factor(Choice))
task1_data$subjectNum = as.factor(task1_data$subjectNum)

#### Task1
y_axis <- "Preference"
file_name <- "task1"
model <- rlm(score ~ Choice * role, data = task1_data)
summary(model)

# 사후 검정 및 시각화
# 직접적인 비교 테이블 생성
emm_all <- emmeans(model, ~ Choice * role)
pairs_result <- pairs(emm_all, adjust = "bonferroni")
pairs_df <- as.data.frame(pairs_result)

# 유의성 검정 결과 출력
print("본페로니 보정 후 유의미한 결과:")
significant_pairs <- pairs_df[pairs_df$p.value < 0.05, ]
print(significant_pairs)

# Choice 레이블 변경을 위한 매핑 설정
profile_labels <- c("1" = "Option 1", "2" = "Option 2", "3" = "Option 3")

# 박스플롯 위치 계산 함수 - 더 정확한 위치 지정을 위해 사용
get_dodge_position <- function(choice_num, role) {
  dodge_width <- 0.65
  choice_num <- as.numeric(choice_num)
  if (role == "Junior") return(choice_num - dodge_width / 4)
  else return(choice_num + dodge_width / 4)
}

# 브래킷을 그리기 위한 준비 작업
# 유의미한 비교를 그룹화
profile_comparisons <- list(
  # 각 프로필 내 Junior vs Senior 비교 (Prof1: J vs S, Prof2: J vs S, Prof3: J vs S)
  profile_within = list(
    list(pattern = "Choice1 Junior - Choice1 Senior", choice = "1"),
    list(pattern = "Choice2 Junior - Choice2 Senior", choice = "2"),
    list(pattern = "Choice3 Junior - Choice3 Senior", choice = "3")
  ),
  
  # Junior 내 프로필 간 비교 (Prof1 vs Prof2, Prof2 vs Prof3, Prof1 vs Prof3)
  junior_across = list(
    list(pattern = "Choice1 Junior - Choice2 Junior", choice1 = "1", choice2 = "2"),
    list(pattern = "Choice2 Junior - Choice3 Junior", choice1 = "2", choice2 = "3"),
    list(pattern = "Choice1 Junior - Choice3 Junior", choice1 = "1", choice2 = "3")
  ),
  
  # Senior 내 프로필 간 비교 (Prof1 vs Prof2, Prof2 vs Prof3, Prof1 vs Prof3)
  senior_across = list(
    list(pattern = "Choice1 Senior - Choice2 Senior", choice1 = "1", choice2 = "2"),
    list(pattern = "Choice2 Senior - Choice3 Senior", choice1 = "2", choice2 = "3"),
    list(pattern = "Choice1 Senior - Choice3 Senior", choice1 = "1", choice2 = "3")
  )
)

# 각 그룹별 유의한 비교 찾기
find_significant_comparisons <- function(comparison_group, is_within = FALSE) {
  significant_comparisons <- list()
  for (comp in comparison_group) {
    if (any(grepl(comp$pattern, pairs_df$contrast) & pairs_df$p.value < 0.05)) {
      p_val <- pairs_df$p.value[grepl(comp$pattern, pairs_df$contrast)]
      
      if (is_within) {
        # 프로필 내 Junior vs Senior 비교
        xmin <- get_dodge_position(comp$choice, "Junior")
        xmax <- get_dodge_position(comp$choice, "Senior")
        significant_comparisons <- c(significant_comparisons, list(
          list(
            pattern = comp$pattern,
            xmin = xmin,
            xmax = xmax,
            p_value = p_val[1]
          )
        ))
      } else {
        # 각 role 내 프로필 간 비교
        if (comp$pattern %in% c("Choice1 Junior - Choice2 Junior", "Choice2 Junior - Choice3 Junior", "Choice1 Junior - Choice3 Junior")) {
          role <- "Junior"
        } else {
          role <- "Senior"
        }
        
        xmin <- get_dodge_position(comp$choice1, role)
        xmax <- get_dodge_position(comp$choice2, role)
        significant_comparisons <- c(significant_comparisons, list(
          list(
            pattern = comp$pattern,
            xmin = xmin,
            xmax = xmax,
            p_value = p_val[1]
          )
        ))
      }
    }
  }
  return(significant_comparisons)
}

# 유의한 비교 찾기
sig_profile_within <- find_significant_comparisons(profile_comparisons$profile_within, TRUE)
sig_junior_across <- find_significant_comparisons(profile_comparisons$junior_across)
sig_senior_across <- find_significant_comparisons(profile_comparisons$senior_across)

# 브래킷 높이 계산 (차곡차곡 쌓기 위한 초기값과 간격)
bracket_base_height <- 7.3   # 가장 낮은 브래킷 높이 시작
bracket_spacing <- 0.7       # 브래킷 간 간격
max_bracket_height <- bracket_base_height

# 그래프 기본 생성
p <- ggplot(task1_data, aes(x = Choice, y = score, fill = role)) + 
  geom_boxplot(position = position_dodge(0.65), width = 0.6, outlier.shape = 1, outlier.size = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = role, color = role), 
               position = position_dodge(0.65), size = 0.8) +
  scale_fill_manual(name = NULL, values = c(Senior = "#e8f6ff", Junior = "#ffe2f2")) +
  scale_color_manual(name = NULL, values = c(Senior = "#2529c3", Junior = "#ff0090")) +
  scale_x_discrete(labels = profile_labels) +
  labs(y = y_axis, x = "Profile Choice") +
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

# 1. 프로필 내 Junior vs Senior 비교 브래킷 추가
for (i in 1:length(sig_profile_within)) {
  comp <- sig_profile_within[[i]]
  p <- p + geom_signif(
    xmin = comp$xmin, 
    xmax = comp$xmax,
    y_position = bracket_base_height,
    annotations = paste0("p=", format(comp$p_value, digits = 3, scientific = TRUE)),
    tip_length = 0.02,
    color = "black"
  )
  max_bracket_height <- max(max_bracket_height, bracket_base_height)
}

# 2. Junior 내 프로필 간 비교 브래킷 추가
junior_base_height <- bracket_base_height + bracket_spacing
for (i in 1:length(sig_junior_across)) {
  comp <- sig_junior_across[[i]]
  # 긴 브래킷(1-3 비교)은 위쪽에 배치
  y_pos <- junior_base_height
  if (grepl("Choice1 Junior - Choice3 Junior", comp$pattern)) {
    y_pos <- junior_base_height + bracket_spacing
  }
  
  p <- p + geom_signif(
    xmin = comp$xmin, 
    xmax = comp$xmax,
    y_position = y_pos,
    annotations = paste0("p=", format(comp$p_value, digits = 3, scientific = TRUE)),
    tip_length = 0.02,
    color = "#ff0090"
  )
  max_bracket_height <- max(max_bracket_height, y_pos)
}

# 3. Senior 내 프로필 간 비교 브래킷 추가
senior_base_height <- max_bracket_height + bracket_spacing
for (i in 1:length(sig_senior_across)) {
  comp <- sig_senior_across[[i]]
  # 긴 브래킷(1-3 비교)은 위쪽에 배치
  y_pos <- senior_base_height
  if (grepl("Choice1 Senior - Choice3 Senior", comp$pattern)) {
    y_pos <- senior_base_height + bracket_spacing
  }
  
  p <- p + geom_signif(
    xmin = comp$xmin, 
    xmax = comp$xmax,
    y_position = y_pos,
    annotations = paste0("p=", format(comp$p_value, digits = 3, scientific = TRUE)),
    tip_length = 0.02,
    color = "#2529c3"
  )
  max_bracket_height <- max(max_bracket_height, y_pos)
}

# 그래프 y축 범위 설정 (모든 브래킷이 보이도록)
max_y_limit <- max_bracket_height + bracket_spacing  # 약간의 여유 공간 추가
p <- p + scale_y_continuous(breaks = 1:7, limits = c(1, max_y_limit), expand = expansion(mult = c(0, .03)))

# 최종 그래프 출력
print(p)

# 그래프 저장
output_path <- paste0("./Desktop/github/devilAdvocate/dataVisualization/Final_selfReported_immersion/", file_name, ".pdf")
ggsave(filename = output_path, plot = p, width = 4.3, height = 4, device = "pdf", dpi = 300)
message("PDF 저장 완료: ", output_path)






############## 개선된 코드 for Task 2
#### TASK2
# Reshape the data for Task 1
task2_data <- data %>%
  dplyr::select(subjectNum, role, T2V1, T2V2, T2V3) %>%
  pivot_longer(
    cols = T2V1:T2V3,
    names_to = "Choice",
    names_pattern = "T2V(\\d+)",
    values_to = "score"
  ) %>%
  mutate(Choice = as.factor(Choice))

#### Task2
y_axis <- "Preference"
file_name <- "task2"
model <- rlm(score ~ Choice * role, data = task2_data)
summary(model)

# 사후 검정 및 시각화
# 직접적인 비교 테이블 생성
emm_all <- emmeans(model, ~ Choice * role)
pairs_result <- pairs(emm_all, adjust = "bonferroni")
pairs_df <- as.data.frame(pairs_result)

# 유의성 검정 결과 출력
print("본페로니 보정 후 유의미한 결과:")
significant_pairs <- pairs_df[pairs_df$p.value < 0.05, ]
print(significant_pairs)

# Choice 레이블 변경을 위한 매핑 설정
profile_labels <- c("1" = "Option 1", "2" = "Option 2", "3" = "Option 3")

# 박스플롯 위치 계산 함수 - 더 정확한 위치 지정을 위해 사용
get_dodge_position <- function(choice_num, role) {
  dodge_width <- 0.65
  choice_num <- as.numeric(choice_num)
  if (role == "Junior") return(choice_num - dodge_width / 4)
  else return(choice_num + dodge_width / 4)
}

# 브래킷을 그리기 위한 준비 작업
# 유의미한 비교를 그룹화
profile_comparisons <- list(
  # 각 프로필 내 Junior vs Senior 비교 (Prof1: J vs S, Prof2: J vs S, Prof3: J vs S)
  profile_within = list(
    list(pattern = "Choice1 Junior - Choice1 Senior", choice = "1"),
    list(pattern = "Choice2 Junior - Choice2 Senior", choice = "2"),
    list(pattern = "Choice3 Junior - Choice3 Senior", choice = "3")
  ),
  
  # Junior 내 프로필 간 비교 (Prof1 vs Prof2, Prof2 vs Prof3, Prof1 vs Prof3)
  junior_across = list(
    list(pattern = "Choice1 Junior - Choice2 Junior", choice1 = "1", choice2 = "2"),
    list(pattern = "Choice2 Junior - Choice3 Junior", choice1 = "2", choice2 = "3"),
    list(pattern = "Choice1 Junior - Choice3 Junior", choice1 = "1", choice2 = "3")
  ),
  
  # Senior 내 프로필 간 비교 (Prof1 vs Prof2, Prof2 vs Prof3, Prof1 vs Prof3)
  senior_across = list(
    list(pattern = "Choice1 Senior - Choice2 Senior", choice1 = "1", choice2 = "2"),
    list(pattern = "Choice2 Senior - Choice3 Senior", choice1 = "2", choice2 = "3"),
    list(pattern = "Choice1 Senior - Choice3 Senior", choice1 = "1", choice2 = "3")
  )
)

# 각 그룹별 유의한 비교 찾기
find_significant_comparisons <- function(comparison_group, is_within = FALSE) {
  significant_comparisons <- list()
  for (comp in comparison_group) {
    if (any(grepl(comp$pattern, pairs_df$contrast) & pairs_df$p.value < 0.05)) {
      p_val <- pairs_df$p.value[grepl(comp$pattern, pairs_df$contrast)]
      
      if (is_within) {
        # 프로필 내 Junior vs Senior 비교
        xmin <- get_dodge_position(comp$choice, "Junior")
        xmax <- get_dodge_position(comp$choice, "Senior")
        significant_comparisons <- c(significant_comparisons, list(
          list(
            pattern = comp$pattern,
            xmin = xmin,
            xmax = xmax,
            p_value = p_val[1]
          )
        ))
      } else {
        # 각 role 내 프로필 간 비교
        if (comp$pattern %in% c("Choice1 Junior - Choice2 Junior", "Choice2 Junior - Choice3 Junior", "Choice1 Junior - Choice3 Junior")) {
          role <- "Junior"
        } else {
          role <- "Senior"
        }
        
        xmin <- get_dodge_position(comp$choice1, role)
        xmax <- get_dodge_position(comp$choice2, role)
        significant_comparisons <- c(significant_comparisons, list(
          list(
            pattern = comp$pattern,
            xmin = xmin,
            xmax = xmax,
            p_value = p_val[1]
          )
        ))
      }
    }
  }
  return(significant_comparisons)
}

# 유의한 비교 찾기
sig_profile_within <- find_significant_comparisons(profile_comparisons$profile_within, TRUE)
sig_junior_across <- find_significant_comparisons(profile_comparisons$junior_across)
sig_senior_across <- find_significant_comparisons(profile_comparisons$senior_across)

# 브래킷 높이 계산 (차곡차곡 쌓기 위한 초기값과 간격)
bracket_base_height <- 7.3   # 가장 낮은 브래킷 높이 시작
bracket_spacing <- 0.7       # 브래킷 간 간격
max_bracket_height <- bracket_base_height

# 그래프 기본 생성
p <- ggplot(task2_data, aes(x = Choice, y = score, fill = role)) + 
  geom_boxplot(position = position_dodge(0.65), width = 0.6, outlier.shape = 1, outlier.size = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = role, color = role), 
               position = position_dodge(0.65), size = 0.8) +
  scale_fill_manual(name = NULL, values = c(Senior = "#e8f6ff", Junior = "#ffe2f2")) +
  scale_color_manual(name = NULL, values = c(Senior = "#2529c3", Junior = "#ff0090")) +
  scale_x_discrete(labels = profile_labels) +
  labs(y = y_axis, x = "Profile Choice") +
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

# 1. 프로필 내 Junior vs Senior 비교 브래킷 추가
for (i in 1:length(sig_profile_within)) {
  comp <- sig_profile_within[[i]]
  p <- p + geom_signif(
    xmin = comp$xmin, 
    xmax = comp$xmax,
    y_position = bracket_base_height,
    annotations = paste0("p=", format(comp$p_value, digits = 3, scientific = TRUE)),
    tip_length = 0.02,
    color = "black"
  )
  max_bracket_height <- max(max_bracket_height, bracket_base_height)
}

# 2. Junior 내 프로필 간 비교 브래킷 추가
junior_base_height <- bracket_base_height + bracket_spacing
for (i in 1:length(sig_junior_across)) {
  comp <- sig_junior_across[[i]]
  # 긴 브래킷(1-3 비교)은 위쪽에 배치
  y_pos <- junior_base_height
  if (grepl("Choice1 Junior - Choice3 Junior", comp$pattern)) {
    y_pos <- junior_base_height + bracket_spacing
  }
  
  p <- p + geom_signif(
    xmin = comp$xmin, 
    xmax = comp$xmax,
    y_position = y_pos,
    annotations = paste0("p=", format(comp$p_value, digits = 3, scientific = TRUE)),
    tip_length = 0.02,
    color = "#ff0090"
  )
  max_bracket_height <- max(max_bracket_height, y_pos)
}

# 3. Senior 내 프로필 간 비교 브래킷 추가
senior_base_height <- max_bracket_height + bracket_spacing
for (i in 1:length(sig_senior_across)) {
  comp <- sig_senior_across[[i]]
  # 긴 브래킷(1-3 비교)은 위쪽에 배치
  y_pos <- senior_base_height
  if (grepl("Choice1 Senior - Choice3 Senior", comp$pattern)) {
    y_pos <- senior_base_height + bracket_spacing
  }
  
  p <- p + geom_signif(
    xmin = comp$xmin, 
    xmax = comp$xmax,
    y_position = y_pos,
    annotations = paste0("p=", format(comp$p_value, digits = 3, scientific = TRUE)),
    tip_length = 0.02,
    color = "#2529c3"
  )
  max_bracket_height <- max(max_bracket_height, y_pos)
}

# 그래프 y축 범위 설정 (모든 브래킷이 보이도록)
max_y_limit <- max_bracket_height + bracket_spacing  # 약간의 여유 공간 추가
p <- p + scale_y_continuous(breaks = 1:7, limits = c(1, max_y_limit), expand = expansion(mult = c(0, .03)))

# 최종 그래프 출력
print(p)

# 그래프 저장
output_path <- paste0("./Desktop/github/devilAdvocate/dataVisualization/Final_selfReported_immersion/", file_name, ".pdf")
ggsave(filename = output_path, plot = p, width = 4.3, height = 4, device = "pdf", dpi = 300)
message("PDF 저장 완료: ", output_path)



# 
# 
# ########################################### 기존 코드 ###############################3
# library(robustlmm)
# library(emmeans)
# library(readr)
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# 
# library(robustlmm)
# library(emmeans)
# library(broom)
# library(ggplot2)
# library(ggpubr)   # stat_pvalue_manual()
# library(tidyr)   # add this near the other library() calls
# library(stringr)   # ← add this with the other library() calls
# library(ggsignif)
# library(dplyr)
# 
# 
# 
# 
# # Import the data
# data <- read.csv("./Desktop/github/devilAdvocate/data/selfReported/selfReportedImmersion.csv")
# 
# # Convert necessary columns to factors
# data$subjectNum <- as.factor(data$subjectNum)
# data$role <- as.factor(data$role)
# 
# 
# #### Drawing Function
# ### Function for Drawing Figure
# draw_significance_plot <- function(size_type = 2, y_var = "PS") {
#   library(emmeans)
#   library(ggplot2)
#   library(dplyr)
#   library(ggsignif)
#   
#   # Post-hoc test
#   emm_all <- emmeans(model, ~ Condition * Role)
#   pairs_result <- pairs(emm_all, adjust = "bonferroni")
#   pairs_df <- as.data.frame(pairs_result) %>%
#     mutate(
#       signif = case_when(
#         p.value < 0.001 ~ "***",
#         p.value < 0.01 ~ "**", 
#         p.value < 0.05 ~ "*",
#         TRUE ~ ""  # ns 대신 빈 문자열
#       ),
#       p_label = case_when(
#         p.value < 0.0001 ~ "p<0.0001 ***",
#         p.value < 0.001 ~ paste0("p=", sprintf("%.4f", p.value), " ***"),
#         p.value < 0.01 ~ paste0("p=", sprintf("%.4f", p.value), " **"),
#         p.value < 0.05 ~ paste0("p=", sprintf("%.4f", p.value), " *"),
#         TRUE ~ ""  # "p=X.XXXX ns" 대신 빈 문자열로 변경
#       ),
#       is_significant = p.value < 0.05
#     )
#   
#   # 고정된 y_max 값 설정 - 항상 일정한 여백 유지
#   y_max <- 8.6
#   
#   # y_var 매개변수를 사용하여 변수 동적 선택
#   # aes_string 대신 aes 내 !!sym()을 사용
#   p_with_signif <- ggplot(data, aes(x = Condition, y = !!sym(y_var), fill = Role)) + 
#     geom_boxplot(position = position_dodge(0.65), width = 0.6, outlier.shape = 1, outlier.size = 2) +
#     stat_summary(fun = mean, geom = "line", aes(group = Role, color = Role), position = position_dodge(0.65), size = 0.8) +
#     scale_fill_manual(name = NULL, values = c(Senior = "#e8f6ff", Junior = "#ffe2f2")) +
#     scale_color_manual(name = NULL, values = c(Senior = "#2529c3", Junior = "#ff0090")) +
#     scale_y_continuous(breaks = 1:7, limits = c(1, y_max), expand = expansion(mult = c(0, .03))) +
#     labs(y = y_axis, x = "System Condition") +  # 외부 정의된 y_axis 변수 사용
#     theme_gray(base_size = 12) +
#     theme(
#       axis.title.x = element_text(size = 14, margin = margin(t = 7)),
#       axis.title.y = element_text(size = 14, margin = margin(r = 7)),
#       axis.text.x = element_text(size = 13),
#       axis.text.y = element_text(size = 13),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor = element_blank(),
#       legend.position = "bottom",
#       legend.margin = margin(t = -5),
#       legend.text = element_text(size = 13)
#     )
#   
#   get_dodge_position <- function(condition_num, role) {
#     dodge_width <- 0.65
#     if (role == "Junior") return(condition_num - dodge_width / 4)
#     else return(condition_num + dodge_width / 4)
#   }
#   
#   # 확실하게 유의미한 경우에만 브라켓 추가하도록 함수 강화
#   add_signif_brackets <- function(plot, filter_pattern, condition1, condition2, base_y_pos, color, is_condition_comp = TRUE, role = NULL) {
#     # 패턴에 맞고 유의미한(p.value < 0.05) 행만 찾기
#     matching_rows <- which(grepl(filter_pattern, pairs_df$contrast) & pairs_df$is_significant)
#     
#     # 유의미한 결과가 있을 경우에만 브라켓 추가
#     if (length(matching_rows) > 0 && pairs_df$is_significant[matching_rows[1]]) {
#       if (is_condition_comp && !is.null(role)) {
#         xmin <- get_dodge_position(condition1, role)
#         xmax <- get_dodge_position(condition2, role)
#       } else if (!is_condition_comp) {
#         xmin <- condition1 - 0.15
#         xmax <- condition1 + 0.15
#       } else {
#         xmin <- condition1
#         xmax <- condition2
#       }
#       
#       # 빈 p_label은 표시하지 않음
#       if (pairs_df$p_label[matching_rows[1]] != "") {
#         plot <- plot + geom_signif(
#           xmin = xmin, 
#           xmax = xmax, 
#           y_position = base_y_pos, 
#           annotations = pairs_df$p_label[matching_rows[1]], 
#           tip_length = 0.02, 
#           color = color
#         )
#       }
#     }
#     return(plot)
#   }
#   
#   within_cond_base_y <- 7.3
#   junior_cond_base_y <- 7.9
#   senior_cond_base_y <- 7.9
#   
#   sig_specs <- list(
#     list("Baseline.*Junior - Baseline.*Senior", 1, 1, within_cond_base_y, "black", FALSE, NULL),
#     list("AIGC.*Junior - AIGC.*Senior", 2, 2, within_cond_base_y, "black", FALSE, NULL),
#     list("AIMM.*Junior - AIMM.*Senior", 3, 3, within_cond_base_y, "black", FALSE, NULL),
#     list("Baseline.*Junior - AIGC.*Junior", 1, 2, junior_cond_base_y, "#ff0090", TRUE, "Junior"),
#     list("AIGC.*Junior - AIMM.*Junior", 2, 3, junior_cond_base_y, "#ff0090", TRUE, "Junior"),
#     list("Baseline.*Junior - AIMM.*Junior", 1, 3, junior_cond_base_y + 0.5, "#ff0090", TRUE, "Junior"),
#     list("Baseline.*Senior - AIGC.*Senior", 1, 2, senior_cond_base_y, "#2529c3", TRUE, "Senior"),
#     list("AIGC.*Senior - AIMM.*Senior", 2, 3, senior_cond_base_y, "#2529c3", TRUE, "Senior"),
#     list("Baseline.*Senior - AIMM.*Senior", 1, 3, senior_cond_base_y + 0.5, "#2529c3", TRUE, "Senior")
#   )
#   
#   # 각 비교 패턴에 대해 브라켓 추가
#   for (spec in sig_specs) {
#     p_with_signif <- add_signif_brackets(p_with_signif, spec[[1]], spec[[2]], spec[[3]], spec[[4]], spec[[5]], spec[[6]], spec[[7]])
#   }
#   
#   # 유의미한 비교만 출력
#   print("유의미한 비교:")
#   sig_pairs <- pairs_df[pairs_df$is_significant, c("contrast", "p_label")]
#   print(sig_pairs)
#   
#   # Width/Height 결정
#   if (size_type == 1) {
#     pdf_width <- 4.3
#     pdf_height <- 4
#   } else {
#     pdf_width <- 3.7
#     pdf_height <- 4
#   }
#   
#   output_path <- paste0("./Desktop/github/devilAdvocate/dataVisualization/Final_selfReported_immersion/", file_name, ".pdf")
#   
#   ggsave(filename = output_path, plot = p_with_signif, width = pdf_width, height = pdf_height, device = "pdf", dpi = 300)
#   message("PDF 저장 완료: ", output_path)
# }
# 
# 
# 
# #### TASK1
# # Reshape the data for Task 1
# task1_data <- data %>%
#   select(subjectNum, role, T1P1, T1P2, T1P3) %>%
#   pivot_longer(
#     cols = T1P1:T1P3,
#     names_to = "Choice",
#     names_pattern = "T1P(\\d+)",
#     values_to = "score"
#   ) %>%
#   mutate(Choice = as.factor(Choice))
# task1_data$subjectNum = as.factor(task1_data$subjectNum)
# 
# #### Task1
# y_axis <- "Preference"
# file_name <- "task1"
# model <- rlm(score ~ Choice * role, data = task1_data)
# summary(model)
# emm_all <- emmeans(model, ~ Choice * role)
# pairs(emm_all, adjust = "bonferroni")
# draw_significance_plot(1, "score")
# 
# 
# 
# #########################
# 
# 
# #### TASK2
# # Reshape the data for Task 1
# task2_data <- data %>%
#   select(subjectNum, role, T2V1, T2V2, T2V3) %>%
#   pivot_longer(
#     cols = T2V1:T2V3,
#     names_to = "Choice",
#     names_pattern = "T2V(\\d+)",
#     values_to = "score"
#   ) %>%
#   mutate(Choice = as.factor(Choice))
# 
# 
# #### Task2
# model <- rlm(score ~ Choice * role, data = task2_data)
# summary(model)
# 
# # Post-hoc tests
# emm <- emmeans(model, ~ Choice | role)
# pairs(emm)
# 
# # Post-hoc tests
# emm <- emmeans(model, ~ role | Choice)
# pairs(emm)
# 
