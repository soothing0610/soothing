## Treat vs. ctl Day1 ##

# Load  data
read_and_process_data <- function(file_path, time) {
  data <- read_excel(file_path)
  colnames(data)[1] <- "...1" 
  data <- data.frame(data)
  data$id <- as.numeric(sub("s(\\d+)_.*", "\\1", data$`...1`))
  data$time <- time
  data <- data[!is.na(data$game_avg), ] #
  data <- data[,c("id", "time", "game_avg", "ctl_avg")] #
  df <- melt(data, id.vars = c("id", "time"), measure.vars = c("game_avg", "ctl_avg")) #
  df <- df %>%
    rename(treatment = variable, score = value)
  df$treatment <- factor(df$treatment, levels = c("game_avg", "ctl_avg")) #
  return(df)}

df1 <- read_and_process_data("C:/Users/GAG01/OneDrive/바탕 화면/soothing_data/Similarity/Bert_혼합전처리/1일차/조건별/ai_stand, 1일차 조건별.xlsx", "1day")
df2 <- read_and_process_data("C:/Users/GAG01/OneDrive/바탕 화면/soothing_data/Similarity/Bert_혼합전처리/2일차/조건별/ai_stand, 2일차 조건별.xlsx", "2day")
df3 <- read_and_process_data("C:/Users/GAG01/OneDrive/바탕 화면/soothing_data/Similarity/Bert_혼합전처리/4일차/조건별/ai_stand, 4일차 조건별.xlsx", "4day")
###################################################################      
summary(df1)
d<-df1
# Run Paired Tests
compare_means(score ~ treatment, data = d, method="t.test", paired = TRUE)
compare_means(score ~ treatment, data = d, alternative="greater", method="t.test", paired = TRUE)

compare_means(score ~ treatment, data = d, method="wilcox.test", paired = TRUE)
compare_means(score ~ treatment, data = d, alternative="greater", method="wilcox.test", paired = TRUE)
