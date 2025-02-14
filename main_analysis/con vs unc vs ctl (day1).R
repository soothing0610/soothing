## Con vs. Unc vs. Ctl Day1 ##

# Load data
read_and_process_data <- function(file_path, time) {
  data <- read_excel(file_path)
  colnames(data)[1] <- "...1"  
  data <- data.frame(data)
  data$id <- as.numeric(sub("s(\\d+)_.*", "\\1", data$`...1`))
  data$time <- time
  data <- data[!is.na(data$con_avg), ]
  data <- data[,c("id", "time", "con_avg", "unc_avg", "ctl_avg")]
  df <- melt(data, id.vars = c("id", "time"), measure.vars = c("con_avg", "unc_avg", "ctl_avg"))
  df <- df %>%
    rename(treatment = variable, score = value)
  df$treatment <- factor(df$treatment, levels = c("con_avg", "unc_avg", "ctl_avg"))
  return(df)}

df1 <- read_and_process_data("C:/Users/GAG01/OneDrive/바탕 화면/soothing_data/Gist/Gist/1일차/조건별/gist_stand, 1일차 조건별.xlsx", "1day")
df2 <- read_and_process_data("C:/Users/GAG01/OneDrive/바탕 화면/soothing_data/Gist/Gist/2일차/조건별/gist_stand, 2일차 조건별.xlsx", "2day")
df3 <- read_and_process_data("C:/Users/GAG01/OneDrive/바탕 화면/soothing_data/Gist/Gist/4일차/조건별/gist_stand, 4일차 조건별.xlsx", "4day")

c <- c("#F8766D", "#00BFC4", "#F5F5F5")
c1 <- c("darkseagreen3", "darkseagreen2", "white") 
c2 <- c("thistle","thistle1","white")

title<-"Similarity score for day 1 (n=56)"
d<-df1
summary(d)
rm_anova(d)
onerm_bxp(d, title, -4,4, c)


# Run Kruskal-Wallis test and Wilcoxon test
KW_bxp(d, title,-4,4,c) 
compare_means(score ~ treatment, data = d, method = "kruskal.test")
compare_means(score ~ treatment, data = d, ref.group="ctl_avg", method="wilcox.test", paired = TRUE, p.adjust.method="bonferroni")
compare_means(score ~ treatment, data = d, method="wilcox.test", paired = TRUE, p.adjust.method="bonferroni")
compare_means(score ~ treatment, data = d, method="wilcox.test", alternative="less", paired = TRUE, p.adjust.method="bonferroni")
compare_means(score ~ treatment, data = d, method="wilcox.test", alternative="greater", paired = TRUE, p.adjust.method="bonferroni")
pairwise.wilcox.test(d$score, d$treatment, p.adjust.method='bonferroni')

# Run Anova and paired t-test  
ANOVA_bxp(d, title,-2,2,c)
compare_means(score ~ treatment, data = d,method='anova', paired=TRUE)
compare_means(score ~ treatment, data = d,method='t.test', ref.group="ctl_avg", paired=TRUE, p.adjust.method="bonferroni") 
compare_means(score ~ treatment, data = d,method='t.test', paired=TRUE, p.adjust.method="bonferroni") 
compare_means(score ~ treatment, data = d,method='t.test', alternative="less", paired=TRUE, p.adjust.method="bonferroni") 
compare_means(score ~ treatment, data = d,method='t.test', alternative="greater", paired=TRUE, p.adjust.method="bonferroni") 

#########################
#0.Summary function #
summary <- function(df) {
  summary_stats <- df %>%
    group_by(treatment) %>%
    summarize(
      median_score = median(score, na.rm = TRUE),mean_score = mean(score, na.rm = TRUE),
      sd_score = sd(score, na.rm = TRUE),
      min_score = min(score, na.rm = TRUE),max_score = max(score, na.rm = TRUE),
      q1_score = quantile(score, probs = 0.25, na.rm = TRUE), q3_score = quantile(score, probs = 0.75, na.rm = TRUE))
  print(summary_stats)
  # Shapiro-Wilk
  shapiro_results <- df %>%
    group_by(treatment) %>%
    summarise(p_value = shapiro.test(score)$p.value)
  print(shapiro_results)
  # Levene's Test
  levene_result <- car::leveneTest(score ~ treatment, data = df)
  print(levene_result)
  # outlier check
  outliers <- df %>%
    group_by(treatment) %>%
    rstatix::identify_outliers(score)
  print(outliers)
  # Q-Q Plot
  qqplot <- ggqqplot(df, "score", ggtheme = theme_bw()) +
    facet_grid(~treatment, labeller = "label_both") +
    labs(title = "Q-Q Plot of Score by Treatment",
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  # histogram
  histograms <- ggplot(df, aes(x = score, fill = treatment)) +
    geom_histogram(binwidth = 0.1, position = "dodge") +
    facet_grid(~treatment, scales = "free_y") +
    labs(title = "Histograms of Score by Treatment",
         x = "Score", y = "Frequency") +
    theme_minimal()
  grid.arrange(qqplot, histograms, ncol = 2)}

#1. one-way repeated anova
rm_anova <- function(df) {
  res.aov <- anova_test(data = df, dv = score, wid = id, within = treatment)
  get_anova_table(res.aov)
  # pairwise comparisons
  pwc <- df %>%
    pairwise_t_test(
      score ~ treatment, paired = TRUE,
      p.adjust.method = "bonferroni")
  return(list(one_way_RMANOVA_result = get_anova_table(res.aov), pairwise_test = pwc))}

#1-1. DRAW BOXPLOT with p-val significance
onerm_bxp <- function(df, title, low, up, c) {
  res.aov <- anova_test(data = df, dv = score, wid = id, within = treatment)
  get_anova_table(res.aov)
  # pairwise comparisons
  pwc <- df %>%
    pairwise_t_test(
      score ~ treatment, paired = TRUE,
      p.adjust.method = "bonferroni")
  # Remove non-significant p-values
  pwc <- pwc %>%
    filter(p.adj < 0.05)  
  group_means <- df %>%
    group_by(treatment) %>%
    summarise(mean_score = mean(score)) %>%
    arrange(treatment)  
  bxp <- ggboxplot(df, x = "treatment", y = "score", fill = "treatment", palette = c) +
    geom_line(data = group_means, aes(x = treatment, y = mean_score, group = 1), 
              color = "black", size = 0.5, inherit.aes = FALSE) +  
    geom_point(data = group_means, aes(x = treatment, y = mean_score), 
               color = "black", fill = "white", shape = 21, size = 2) +
    labs(title = title, y=NULL, x=NULL) +
    theme(legend.title=element_blank(),
          plot.title = element_text(size = 9),  
          legend.text = element_text(size = 8),  
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 9))+
    ylim(low, up)
  pwc <- pwc %>% add_xy_position(x = "treatment")
  if (nrow(pwc) > 0) {
    bxp + 
      stat_pvalue_manual(pwc) +
      labs(
        subtitle = get_test_label(res.aov, detailed = TRUE),
        caption = get_pwc_label(pwc)) +
      theme(
        plot.caption = element_text(size = 9),  
        plot.subtitle = element_text(size = 9))}  
  else {
    bxp + labs(
      subtitle = get_test_label(res.aov, detailed = TRUE),
      caption = "No significant differences found") +
      theme(
        plot.caption = element_text(size = 8.5),  
        plot.subtitle = element_text(size = 8.5))}}  \

#KW boxplot with p-val
KW_bxp <- function(data, title, low, up, c) {
  group_means <- data %>%
    group_by(treatment) %>%
    summarise(mean_score = mean(score)) %>%
    arrange(treatment)  
  compare_means(score ~ treatment, data = data, method = "kruskal.test")
  compare_means(score ~ treatment, data = data, method = "wilcox.test", paired = TRUE) 
  ggboxplot(data, x = "treatment", y = "score", fill = "treatment", palette = c) +
    labs(title = title, y=NULL, x=NULL) +
    geom_line(data = group_means, aes(x = treatment, y = mean_score, group = 1), 
              color = "black", size = 0.5, inherit.aes = FALSE, show.legend = FALSE) +  
    geom_point(data = group_means, aes(x = treatment, y = mean_score), 
               color = "black", fill = "white", shape = 21, size = 2) +
    ylim(low, up) +
    theme(plot.title = element_text(size = 11),
          legend.text = element_text(size = 8),  
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 9))+
    stat_compare_means(method = "kruskal.test", label.y = up) +      
    stat_compare_means(label = "p.signif", method = "wilcox.test", 
                       method.args = list(alternative = "two.sided", paired=TRUE), ref.group="ctl_avg",
                       comparisons = list(c("unc_avg", "ctl_avg"), c("con_avg", "ctl_avg")), p.thresholds = c(0.001, 0.01, 0.05, 0.1), ###바꿔야해해
                       size = 3)}
#Anova boxplot with p-val
ANOVA_bxp <- function(data, title, low, up, c) {
  group_means <- data %>%
    group_by(treatment) %>%
    summarise(mean_score = mean(score)) %>%
    arrange(treatment)  
  anova_result <- compare_means(score ~ treatment, data = data, method = "anova")
  t_test_result <- compare_means(score ~ treatment, data = data, method = "t.test", ref.group = "ctl_avg", paired = TRUE)
  p_val <- round(anova_result$p, 3)
  anova_label <- ifelse(p_val < 0.001, "***", ifelse(p_val < 0.01, "**", ifelse(p_val < 0.05, "*", "")))
  ggboxplot(data, x = "treatment", y = "score", fill = "treatment", palette = c) +
    labs(title = title, y=NULL, x = NULL) + 
    geom_line(data = group_means, aes(x = treatment, y = mean_score, group = 1), 
              color = "black", size = 0.5, inherit.aes = FALSE, show.legend = FALSE) +  
    geom_point(data = group_means, aes(x = treatment, y = mean_score), 
               color = "black", fill = "white", shape = 21, size = 2) +
    ylim(low, up) +
    theme(plot.title = element_text(size = 11),
          legend.text = element_text(size = 8),  
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 9))+
    stat_compare_means(method = "anova", label.y = up) +      
    stat_compare_means(label = "p.signif", method = "t.test", 
                       ref.group = "ctl_avg", method.args = list(alternative = "two.sided"),
                       comparisons = list(c("unc_avg", "ctl_avg"), c("con_avg", "ctl_avg")),
                       hide.ns = TRUE, p.thresholds = c(0.001, 0.01, 0.05), size = 3)}

