library(cluster)

data_clean <- read.csv('horse_racing_cleaned.csv')

# Ensure the target variable, 'finish_time', is numeric
data_clean$finish_time <- as.numeric(data_clean$finish_time)

# Select relevant predictor variables
predictors <- c('horse_ratings_avg', 'declared_weight', 'actual_weight', 'win_odds', 
                'place_odds', 'horse_age', 'horse_country_code', 'trainer_id', 'jockey_id',
                'horse_gear_code', 'venue_code', 'position_sec3', 'position_sec4','distance', 
                'surface', 'race_class', 'behind_sec1', 'behind_sec2', 
                'behind_sec3', 'behind_sec4','prize')

#Clustering
data_normalized <- as.data.frame(scale(data_clean[, predictors]))

# Add 'finish_time' to the normalized data
data_normalized$finish_time <- scale(data_clean$finish_time)

data_normalized <- data_normalized %>%
  mutate(across(everything(), ~ifelse(is.na(.) | is.nan(.), mean(., na.rm = TRUE), .)))

# Determine the optimal number of clusters k
set.seed(123)
wss <- sapply(2:15, function(k){kmeans(data_normalized, k, nstart = 20)$tot.withinss})
elbow_plot <- data.frame(k = 2:15, wss = wss)
ggplot(elbow_plot, aes(x = k, y = wss)) + geom_line() + geom_point() + 
  labs(title = "Elbow Method", x = "Number of clusters k", y = "Total within-cluster sum of squares")

# Choose k based on the Elbow plot
optimal_k <- 5  

# Perform K-Means clustering
set.seed(123)
kmeans_result <- kmeans(data_normalized, centers = optimal_k, nstart = 25)
data_clean$cluster <- kmeans_result$cluster

# Visualizing the clusters with a few selected features (choose features that are meaningful)
ggplot(data_clean, aes(x = declared_weight, y = actual_weight, color = as.factor(cluster))) + 
  geom_point(alpha = 0.6) +
  labs(title = "Cluster Visualization with Declared Weight and Actual Weight", x = "Declared Weight", y = "Actual Weight") +
  scale_color_discrete(name = "Cluster")

# Analyzing cluster properties
cluster_summary <- data_clean %>%
  group_by(cluster) %>%
  summarise_at(vars(one_of(predictors, 'finish_time')), list(mean = mean, sd = sd))

print(cluster_summary)