# ----------------------------------------------
# Script for Data Preparation and DID Analysis
# ----------------------------------------------

# Load required libraries
library(readxl)
library(reshape2)
library(dplyr)

# ----------------------------------------------
# 1. Data Loading and Preprocessing
# ----------------------------------------------

# Function to load and preprocess data
read_and_process_data <- function(file_path, time) {
  # Read Excel file
  data <- read_excel(file_path)
  
  # Rename the first column
  colnames(data)[1] <- "...1"
  
  # Convert data to a data frame and extract participant ID
  data <- data.frame(data)
  data$id <- as.numeric(sub("s(\\d+)_.*", "\\1", data$`...1`))
  
  # Add time label
  data$time <- time
  
  # Remove rows with missing values in 'con_avg' and drop the 'game_avg' column
  data <- data[!is.na(data$con_avg), ]
  data <- subset(data, select = -c(game_avg))
  
  return(data)
}

# ----------------------------------------------
# 2. Load Data Across Time Points
# ----------------------------------------------

# Load life feedback data
df0 <- read_excel("life_feedback.xlsx")

# Load data for each time point (1 day, 2 days, 4 days)
df1 <- read_and_process_data("1day_data.xlsx", "1day")
df2 <- read_and_process_data("2day_data.xlsx", "2day")
df3 <- read_and_process_data("4day_data.xlsx", "4day")

# ----------------------------------------------
# 3. Combine and Reshape Data
# ----------------------------------------------

# Select rows with IDs matching df3 and relevant columns
selected_df0 <- df0[df0$id %in% df3$id, c("id", "life", "feedback")]
selected_df1 <- df1[df1$id %in% df3$id, c("id", "time", "con_avg", "unc_avg", "ctl_avg")]
selected_df2 <- df2[df2$id %in% df3$id, c("id", "time", "con_avg", "unc_avg", "ctl_avg")]
selected_df3 <- df3[df3$id %in% df3$id, c("id", "time", "con_avg", "unc_avg", "ctl_avg")]

# Combine data across time points
df_avg <- rbind(selected_df1, selected_df2, selected_df3)

# Reshape data to long format
df_avg <- melt(df_avg, id.vars = c("id", "time"), measure.vars = c("con_avg", "unc_avg", "ctl_avg"))

# Rename columns and set treatment factor levels
df_avg <- df_avg %>%
  rename(treatment = variable, score = value) %>%
  mutate(treatment = factor(treatment, levels = c("con_avg", "unc_avg", "ctl_avg"))) %>%
  left_join(selected_df0, by = "id")

# ----------------------------------------------
# 4. Add Repetition Counts
# ----------------------------------------------

# Load repetition data for each time point
df_1_r <- read_and_process_data("1day_repetition.xlsx", "1day")
df_2_r <- read_and_process_data("2day_repetition.xlsx", "2day")
df_3_r <- read_and_process_data("4day_repetition.xlsx", "4day")

# Combine repetition data and rename columns
df_r <- rbind(df_1_r, df_2_r, df_3_r) %>%
  rename(con_avg_r = con_avg, unc_avg_r = unc_avg, ctl_avg_r = ctl_avg)

# Merge repetition data with main data and add repetition column
df_avg <- df_avg %>%
  left_join(df_r, by = c("id", "time")) %>%
  mutate(rep = case_when(
    treatment == "con_avg" ~ con_avg_r,
    treatment == "unc_avg" ~ unc_avg_r,
    treatment == "ctl_avg" ~ ctl_avg_r
  )) %>%
  select(id, time, treatment, rep, life, score, feedback)

# ----------------------------------------------
# 5. Difference-in-Differences (DID) Analysis
# ----------------------------------------------

# Add binary treatment indicator
df_avg <- df_avg %>%
  mutate(treat = ifelse(treatment == 'ctl_avg', 0, 1))

# Prepare subsets of data for different time comparisons
df_con_12 <- df_avg %>%
  filter(treatment %in% c('ctl_avg', 'con_avg')) %>%
  filter(time %in% c('1day', '2day')) %>%
  mutate(time01 = ifelse(time == '1day', 0, 1))

df_con_24 <- df_avg %>%
  filter(treatment %in% c('ctl_avg', 'con_avg')) %>%
  filter(time %in% c('2day', '4day')) %>%
  mutate(time01 = ifelse(time == '2day', 0, 1))

df_con_14 <- df_avg %>%
  filter(treatment %in% c('ctl_avg', 'con_avg')) %>%
  filter(time %in% c('1day', '4day')) %>%
  mutate(time01 = ifelse(time == '1day', 0, 1))

df_unc_12 <- df_avg %>%
  filter(treatment %in% c('ctl_avg', 'unc_avg')) %>%
  filter(time %in% c('1day', '2day')) %>%
  mutate(time01 = ifelse(time == '1day', 0, 1))

df_unc_24 <- df_avg %>%
  filter(treatment %in% c('ctl_avg', 'unc_avg')) %>%
  filter(time %in% c('2day', '4day')) %>%
  mutate(time01 = ifelse(time == '2day', 0, 1))

df_unc_14 <- df_avg %>%
  filter(treatment %in% c('ctl_avg', 'unc_avg')) %>%
  filter(time %in% c('1day', '4day')) %>%
  mutate(time01 = ifelse(time == '1day', 0, 1))

# Perform DID analysis for each subset
summary(lm(score ~ treat * time01 + life + feedback + rep, data = df_con_12))
summary(lm(score ~ treat * time01 + life + feedback + rep, data = df_con_24))
summary(lm(score ~ treat * time01 + life + feedback + rep, data = df_con_14))
summary(lm(score ~ treat * time01 + life + feedback + rep, data = df_unc_12))
summary(lm(score ~ treat * time01 + life + feedback + rep, data = df_unc_24))
summary(lm(score ~ treat * time01 + life + feedback + rep, data = df_unc_14))