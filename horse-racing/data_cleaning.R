library(tidyverse)
library(lubridate)
library(ggplot2)
library(corrplot)
library(skimr)
library(fastDummies)

# Load the datasets
races <- read.csv('races.csv', stringsAsFactors = FALSE)
runs <- read.csv('runs.csv', stringsAsFactors = FALSE)

data <- merge(races, runs, by = 'race_id')

# imputing na values to mean
data_clean <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

data_clean$horse_ratings_avg <- apply(data_clean[, 'horse_ratings', drop = FALSE], 1, function(x) {
  ratings <- as.numeric(strsplit(x, "-")[[1]])
  if (length(ratings) == 2) {
    mean(ratings)
  } else {
    NA
  }
})

win_percentage_by_country <- data_clean %>%
  group_by(horse_country) %>%
  summarise(TotalRaces = n(),
            Wins = sum(won, na.rm = TRUE),
            WinPercentage = Wins / TotalRaces * 100)

# 2. Merge the Summary Table with the Original Data
data_clean <- data_clean %>%
  left_join(win_percentage_by_country, by = "horse_country") %>%
  select(-TotalRaces, -Wins) 

win_percentage_by_gear <- data_clean %>%
  group_by(horse_gear) %>%
  summarise(TotalRaces = n(),
            Wins = sum(won, na.rm = TRUE),
            GearWinPercentage = Wins / TotalRaces * 100)

# 2. Merge the Summary Table with the Original Data
data_clean <- data_clean %>%
  left_join(win_percentage_by_gear, by = "horse_gear") %>%
  select(-TotalRaces, -Wins) 

data_clean$horse_country_code <- as.numeric(factor(data_clean$horse_country))
data_clean$horse_gear_code <- as.numeric(factor(data_clean$horse_gear))

country_levels <- levels(factor(data_clean$horse_country))
country_codes <- as.numeric(factor(country_levels))
gear_levels <- levels(factor(data_clean$horse_gear))
gear_codes <- as.numeric(factor(gear_levels))

# Printing the mappings
print(data.frame(Country = country_levels, Code = country_codes))
print(data.frame(Gear = gear_levels, Code = gear_codes))

# Convert 'horse_type' to numeric codes
data_clean$horse_type_code <- as.numeric(factor(data_clean$horse_type))

# Get the levels and codes for horse_type
type_levels <- levels(factor(data_clean$horse_type))
type_codes <- as.numeric(factor(type_levels))

# Print the mapping of horse_type to its corresponding code
horse_type_mapping <- data.frame(Type = type_levels, Code = type_codes)
print(horse_type_mapping)

str(data_clean)

write.csv(data_clean, 'horse_racing_cleaned.csv', row.names = FALSE)
