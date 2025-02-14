## Paired comparison within each timepoint ##
read_and_process_data1 <- function(file_path, time) {
  data <- read_excel(file_path)
  colnames(data)[1] <- "...1"  
  data <- data.frame(data)
  data$id <- as.numeric(sub("s(\\d+)_.*", "\\1", data$`...1`))
  data$time <- time
  data$emo <- 'pos'
  data <- data[!is.na(data$pos_unc_avg), ] 
  colnames(data)[colnames(data) == 'pos_game_avg'] <- 'game'
  colnames(data)[colnames(data) == 'pos_ctl_avg'] <- 'ctl'
  colnames(data)[colnames(data) == 'pos_con_avg'] <- 'con'
  colnames(data)[colnames(data) == 'pos_unc_avg'] <- 'unc'
  data <- data[,c("id", "emo", "game", "ctl", "con", "unc")] #
  df <- melt(data, id.vars = c("id", "emo"), measure.vars = c("game", "ctl", "con", "unc")) #
  df <- df %>%
    rename(treatment = variable, score = value)
  df$treatment <- factor(df$treatment, levels = c("game", "ctl", "con", "unc")) #
  return(df)}

read_and_process_data2 <- function(file_path, time) {
  data <- read_excel(file_path)
  colnames(data)[1] <- "...1" 
  data <- data.frame(data)
  data$id <- as.numeric(sub("s(\\d+)_.*", "\\1", data$`...1`))
  data$time <- time
  data$emo <- 'neg'
  data <- data[!is.na(data$neg_unc_avg), ] 
  colnames(data)[colnames(data) == 'neg_game_avg'] <- 'game'
  colnames(data)[colnames(data) == 'neg_ctl_avg'] <- 'ctl'
  colnames(data)[colnames(data) == 'neg_con_avg'] <- 'con'
  colnames(data)[colnames(data) == 'neg_unc_avg'] <- 'unc'
  data <- data[,c("id", "emo", "game", "ctl","con", "unc")] 
  df <- melt(data, id.vars = c("id", "emo"), measure.vars = c("game", "ctl","con", "unc")) 
  df <- df %>%
    rename(treatment = variable, score = value)
  df$treatment <- factor(df$treatment, levels = c("game", "ctl", "con", "unc")) 
  return(df)}

df56_p <- read_and_process_data1("C:/Users/GAG01/OneDrive/바탕 화면/DATA_meteor/2일차/긍부정조건별/ai_stand, 2일차 긍부정조건별.xlsx", "2day")
df56_n <- read_and_process_data2("C:/Users/GAG01/OneDrive/바탕 화면/DATA_meteor/2일차/긍부정조건별/ai_stand, 2일차 긍부정조건별.xlsx", "2day")
df56 <- rbind(df56_p, df56_n)
df56
summary(df56)

df_p <- subset(df56, emo=="pos")
df_game_p <- subset(df_p, treatment=="game")
df_con_p <- subset(df_p, treatment=="con")
df_unc_p <- subset(df_p, treatment=="unc")
df_ctl_p <- subset(df_p, treatment=="ctl")
df_n <- subset(df56, emo=="neg")
df_game_n <- subset(df_n, treatment=="game")
df_con_n <- subset(df_n, treatment=="con")
df_unc_n <- subset(df_n, treatment=="unc")
df_ctl_n <- subset(df_n, treatment=="ctl")
##############
A <- df_con_n
B <- df_con_p
var.test(A$score, B$score)
direction <- "two.sided"
t.test(A$score, B$score, paired=TRUE, var.equal=TRUE, alternative=direction, p.adjust.methods = "bonferroni")


################
var.test(df_unc_n$score, df_con_p$score)
direction <- "two.sided"
direction <- "less"
t.test(df_unc_n$score, df_con_p$score, paired=TRUE, var.equal=TRUE, alternative = direction, p.adjust.method="bonferroni" )
df_game %>% cohens_d(score ~ emo, paired = TRUE)
df_ctl %>% cohens_d(score ~ emo, paired = TRUE)

direction <- "two.sided"
direction <- "less"  
wilcox.test(df_unc_n$score, df_con_p$score, paired=TRUE, alternative = direction)
###
var.test(df_con_n$score, df_unc_p$score)
direction <- "two.sided"
direction <- "less"
t.test(df_con_n$score, df_unc_p$score, paired=TRUE, var.equal=TRUE, alternative = direction, p.adjust.method="bonferroni" )
df_game %>% cohens_d(score ~ emo, paired = TRUE)
df_ctl %>% cohens_d(score ~ emo, paired = TRUE)

direction <- "two.sided"
direction <- "less"  
wilcox.test(df_con_n$score, df_unc_p$score, paired=TRUE, alternative = direction)

