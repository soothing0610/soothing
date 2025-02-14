## Day1 valence*treatment 2w rm anova

read_and_process_data1 <- function(file_path, time) {
  data <- read_excel(file_path)
  colnames(data)[1] <- "...1"  
  data <- data.frame(data)
  data$id <- as.numeric(sub("s(\\d+)_.*", "\\1", data$`...1`))
  data$time <- time
  data$emo <- 'pos'
  data <- data[!is.na(data$pos_unc_avg), ] 
  colnames(data)[colnames(data) == 'pos_con_avg'] <- 'con'
  colnames(data)[colnames(data) == 'pos_unc_avg'] <- 'unc'
  colnames(data)[colnames(data) == 'pos_ctl_avg'] <- 'ctl'
  data <- data[,c("id", "emo", "con", "unc", "ctl")] 
  df <- melt(data, id.vars = c("id", "emo"), measure.vars = c("con", "unc", "ctl")) 
  df <- df %>%
    rename(treatment = variable, score = value)
  df$treatment <- factor(df$treatment, levels = c("con", "unc", "ctl")) 
  return(df)}
read_and_process_data2 <- function(file_path, time) {
  data <- read_excel(file_path)
  colnames(data)[1] <- "...1" 
  data <- data.frame(data)
  data$id <- as.numeric(sub("s(\\d+)_.*", "\\1", data$`...1`))
  data$time <- time
  data$emo <- 'neg'
  data <- data[!is.na(data$neg_unc_avg), ] 
  colnames(data)[colnames(data) == 'neg_con_avg'] <- 'con'
  colnames(data)[colnames(data) == 'neg_unc_avg'] <- 'unc'
  colnames(data)[colnames(data) == 'neg_ctl_avg'] <- 'ctl'
  data <- data[,c("id", "emo", "con", "unc", "ctl")] #
  df <- melt(data, id.vars = c("id", "emo"), measure.vars = c("con", "unc", "ctl")) 
  df <- df %>%
    rename(treatment = variable, score = value)
  df$treatment <- factor(df$treatment, levels = c("con", "unc", "ctl")) 
  return(df)}

df56_p <- read_and_process_data1("C:/Users/GAG01/OneDrive/바탕 화면/DATA_meteor/4일차/긍부정조건별/ai_stand, 4일차 긍부정조건별.xlsx", "2day")
df56_n <- read_and_process_data2("C:/Users/GAG01/OneDrive/바탕 화면/DATA_meteor/4일차/긍부정조건별/ai_stand, 4일차 긍부정조건별.xlsx", "2day")
df56 <- rbind(df56_p, df56_n)
df56
summary(df56)
analyze_rm_anova(df56)
tworm_bxp(df56, "Positive vs Negative [day2]", -2, 2, c3)

# Paired comparison within each timepoint
df_con_p <- subset(df56, emo == 'pos' & treatment == 'con')
df_unc_p <- subset(df56, emo == 'pos' & treatment == 'unc')
df_ctl_p <- subset(df56, emo == 'pos' & treatment == 'ctl')
df_con_n <- subset(df56, emo == 'neg' & treatment == 'con')
df_unc_n <- subset(df56, emo == 'neg' & treatment == 'unc')
df_ctl_n <- subset(df56, emo == 'neg' & treatment == 'ctl')
var.test(df_con_p$score, df_con_n$score)
var.test(df_unc_p$score, df_unc_n$score)
var.test(df_ctl_p$score, df_ctl_n$score)
direction <- "two.sided"
direction <- "less"
t.test(df_con_n$score, df_con_p$score, paired=TRUE, var.equal=FALSE, alternative = direction, p.adjust.method="bonferroni" )
t.test(df_unc_n$score, df_unc_p$score, paired=TRUE, var.equal=FALSE, alternative = direction, p.adjust.method="bonferroni" )
t.test(df_ctl_n$score, df_ctl_p$score, paired=TRUE, var.equal=TRUE, alternative = direction, p.adjust.method="bonferroni" )
df %>% cohens_d(score ~ treatment, paired = TRUE)

direction <- "two.sided"
direction <- "less"  
wilcox.test(df_con_n$score, df_con_p$score, paired=TRUE, alternative = direction)
wilcox.test(df_unc_n$score, df_unc_p$score, paired=TRUE, alternative = direction)
wilcox.test(df_ctl_n$score, df_ctl_p$score, paired=TRUE, alternative = direction)


########functions##############
#0.Summary func #
summary <- function(df) {
  summary_stats <- df %>%
    group_by(treatment, emo) %>%
    summarize(
      median_score = median(score, na.rm = TRUE),mean_score = mean(score, na.rm = TRUE),
      sd_score = sd(score, na.rm = TRUE),
      min_score = min(score, na.rm = TRUE),max_score = max(score, na.rm = TRUE),
      q1_score = quantile(score, probs = 0.25, na.rm = TRUE), q3_score = quantile(score, probs = 0.75, na.rm = TRUE))
  print(summary_stats)
  # Shapiro-Wilk
  shapiro_results <- df %>%
    group_by(treatment, emo) %>%
    summarise(p_value = shapiro.test(score)$p.value)
  print(shapiro_results)
  # outlier check
  outliers <- df %>%
    group_by(treatment, emo) %>%
    rstatix::identify_outliers(score)
  print(outliers)
  # Q-Q Plot
  qqplot <- ggqqplot(df, "score", ggtheme = theme_bw()) +
    facet_grid(emo ~ treatment, labeller = "label_both") +
    labs(title = "Q-Q Plot of Score by Treatment and Emotion",
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  # histogram
  histograms <- ggplot(df, aes(x = score, fill = treatment)) +
    geom_histogram(binwidth = 0.1, position = "dodge") +
    facet_grid(emo ~ treatment, scales = "free_y") +
    labs(title = "Histograms of Score by Treatment and Emotion",
         x = "Score", y = "Frequency") +
    theme_minimal()
  grid.arrange(qqplot, histograms, ncol = 2)}

#1. 2way rm anova
library(rstatix)
analyze_rm_anova <- function(data) {
  res.aov <- anova_test(  
    data = data, dv = score, wid = id,
    within = c(treatment, emo))
  anova_table <- get_anova_table(res.aov)
  print(anova_table)
  # interaction effect
  # 1-1) Effect of treatment at each time point
  one.way <- data %>% group_by(emo) %>%
    anova_test(dv = score, wid = id, within = treatment) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni")
  print(one.way)
  # 1-2) Pairwise comparisons between treatment groups
  pwc <- data %>%
    group_by(emo) %>%
    pairwise_t_test(
      score ~ treatment, paired = TRUE,
      p.adjust.method = "bonferroni")
  print(pwc)
  # 2-1) Effect of time at each level of treatment
  one.way2 <- data %>%
    group_by(treatment) %>%
    anova_test(dv = score, wid = id, within = emo) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni")
  print(one.way2)
  # 2-2) Pairwise comparisons between time points
  pwc2 <- data %>%
    group_by(treatment) %>%
    pairwise_t_test(
      score ~ emo, paired = TRUE,
      p.adjust.method = "bonferroni")
  print(pwc2)
  # interaction effect이 없다면
  # 1) comparisons for treatment variable
  print(data %>%
          pairwise_t_test(
            score ~ treatment, paired = TRUE, 
            p.adjust.method = "bonferroni"))
  
  # 2) comparisons for time variable
  print(data %>%
          pairwise_t_test(
            score ~ emo, paired = TRUE, 
            p.adjust.method = "bonferroni"))}   

#2. bxplo
tworm_bxp <- function(data, title, low, up, c) {
  group_means <- data %>%
    group_by(emo, treatment) %>%
    summarise(mean_score = mean(score)) %>%
    arrange(emo, treatment)  # 시간과 처리 순서대로 정렬
  bxp <- ggboxplot(
    data, x = "emotion", y = "score",
    color = "treatment", palette = c,
    add = "mean_point", mean.color = "black", mean.shape = 21, mean.size = 2,
    fill = "treatment", legend = "top",
    title = title, xlab = "Emotion", ylab = "Score",
    ylim = c(low, up),
    theme = theme_minimal() +
      theme(
        plot.title = element_text(family = "Arial", face = "bold", size = 12, hjust = 0.5),
        axis.title = element_text(family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", size = 10),
        legend.title = element_text(family = "Arial", size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA))) +
    geom_point(data = group_means, aes(x = emo, y = mean_score, color = treatment), 
               size = 2, position = position_dodge(width = 0.8), shape = 21, fill = "white", stroke = 1) +
    scale_color_manual(values = c("conscious" = "#F8766D", "unconscious" = "#00BFC4", "control" = "#F5F5F5"))
  # Paired t-test
  res.aov <- anova_test(  
    data = data, dv = score, wid = id,
    within = c(treatment, emo))
  anova_table <- get_anova_table(res.aov)
  pwc <- data %>%
    group_by(emo) %>%
    pairwise_t_test(
      score ~ treatment, paired = TRUE,
      p.adjust.method = "bonferroni")
  pwc <- pwc %>% add_xy_position(x = "emo")
  bxp + 
    stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
    labs(
      subtitle = get_test_label(res.aov, detailed = TRUE),
      caption = get_pwc_label(pwc))}


