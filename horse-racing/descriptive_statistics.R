library(tidyverse)
library(ggplot2)
library(corrplot)

data_clean <- read.csv('horse_racing_cleaned.csv')

# 1. Correlation Analysis
betting_relevant_vars <- data_clean %>%
  select(race_id, horse_no, horse_id, result, won, lengths_behind, horse_age, 
         horse_rating, declared_weight, actual_weight, draw, win_odds, place_odds, 
         finish_time, position_sec1, position_sec2, position_sec3, position_sec4, position_sec5, position_sec6, 
         sec_time1, sec_time2, sec_time3, sec_time4, sec_time5, sec_time6, 
         venue, race_no, config, surface, distance, prize, race_class)

# Compute correlation matrix for the selected variables
corr_matrix <- cor(betting_relevant_vars, use = "complete.obs")

# Visualize the correlation matrix
corrplot(corr_matrix, method = "circle")

# 2. Data Visualization

# a. Plot distributions of key variables using histograms or density plots
ggplot(data_clean, aes(x = horse_age)) + geom_histogram(binwidth = 1, fill = 'blue', color = 'black') + labs(title = "Distribution of Horse Ages", x = "Horse Age", y = "Count")
ggplot(data_clean, aes(x = declared_weight)) + geom_histogram(binwidth = 5, fill = 'green', color = 'black') + labs(title = "Distribution of Declared Weight", x = "Declared Weight", y = "Count")
ggplot(data_clean, aes(x = win_odds)) + geom_histogram(binwidth = 0.5, fill = 'red', color = 'black') + labs(title = "Distribution of Win Odds", x = "Win Odds", y = "Count")


# b. Boxplots for finish times or positions by different categories
ggplot(data_clean, aes(x = as.factor(venue_code), y = finish_time)) + geom_boxplot() + labs(title = "Finish Time by Venue", x = "Venue", y = "Finish Time")
ggplot(data_clean, aes(x = as.factor(horse_type_code), y = finish_time)) + geom_boxplot() + labs(title = "Finish Time by Horse Type", x = "Horse Type", y = "Finish Time")
ggplot(data_clean, aes(x = horse_country_code)) +
  geom_histogram(binwidth = 1, fill = 'blue', color = 'black') +
  labs(title = "Distribution of Horse Country Code", x = "Horse Country Code", y = "Count") +
  theme_minimal()

# Histogram for horse_type_code
ggplot(data_clean, aes(x = horse_type_code)) +
  geom_histogram(binwidth = 1, fill = 'green', color = 'black') +
  labs(title = "Distribution of Horse Type Code", x = "Horse Type Code", y = "Count") +
  theme_minimal()

# Histogram for horse_gear_code
ggplot(data_clean, aes(x = horse_gear_code)) +
  geom_histogram(binwidth = 1, fill = 'red', color = 'black') +
  labs(title = "Distribution of Horse Gear Code", x = "Horse Gear Code", y = "Count") +
  theme_minimal()

# c. Scatter plots to examine relationships between continuous variables
ggplot(data_clean, aes(x = declared_weight, y = finish_time)) + geom_point() + geom_smooth(method = lm) + labs(title = "Declared Weight vs Finish Time", x = "Declared Weight", y = "Finish Time")
ggplot(data_clean, aes(x = win_odds, y = finish_time)) + geom_point() + geom_smooth(method = lm) + labs(title = "Win Odds vs Finish Time", x = "Win Odds", y = "Finish Time")
