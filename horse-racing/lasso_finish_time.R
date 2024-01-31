library(tidyverse)
library(glmnet)
library(caret)
library(cluster)

# Load the data
data_clean <- read.csv('horse_racing_cleaned.csv')

# Ensure the target variable, 'finish_time', is numeric
data_clean$finish_time <- as.numeric(data_clean$finish_time)

# Select relevant predictor variables
predictors <- c('horse_ratings_avg', 'declared_weight', 'actual_weight', 'win_odds', 
                'place_odds', 'horse_age', 'horse_country_code', 'trainer_id', 'jockey_id',
                'horse_gear_code', 'position_sec3', 'position_sec4','distance', 
                'surface', 'race_class', 'behind_sec1', 'behind_sec2', 
                'behind_sec3', 'behind_sec4','prize')

# Prepare the data for training and testing
set.seed(123)  # for reproducibility
training_index <- sample(1:nrow(data_clean), 0.7 * nrow(data_clean))

training_data <- data_clean[training_index, c(predictors, 'finish_time')]
testing_data <- data_clean[-training_index, c(predictors, 'finish_time')]

# Impute missing values in numeric predictors with mean
for (predictor in predictors) {
  if (is.numeric(training_data[[predictor]])) {
    training_data[[predictor]][is.na(training_data[[predictor]])] <- mean(training_data[[predictor]], na.rm = TRUE)
    testing_data[[predictor]][is.na(testing_data[[predictor]])] <- mean(testing_data[[predictor]], na.rm = TRUE)
  }
}

# Update matrices after imputation
x_train <- as.matrix(training_data[, predictors])
y_train <- training_data$finish_time
x_test <- as.matrix(testing_data[, predictors])
y_test <- testing_data$finish_time



# Set alpha = 1 for lasso regression
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1, family = "gaussian")

# Best lambda
best_lambda <- lasso_model$lambda.min


# Make predictions
predictions <- predict(lasso_model, s = best_lambda, newx = x_test)

# Calculate RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((y_test - predictions)^2))
print(paste("RMSE:", rmse))

# R-squared
rsq <- cor(y_test, predictions)^2
print(paste("R-squared:", rsq))


# Coefficients
lasso_coef <- coef(lasso_model, s = best_lambda)
print(lasso_coef)

# Convert the sparse matrix to a regular matrix
lasso_coef_matrix <- as.matrix(lasso_coef)

var_imp <- data.frame(
  Feature = rownames(lasso_coef_matrix)[-1],  # Exclude the first row which is the intercept
  Coefficient = lasso_coef_matrix[-1, 1]       # Exclude the intercept coefficient
)

# Removing zero coefficients
var_imp <- var_imp[var_imp$Coefficient != 0,]

# Absolute value of coefficients for importance
var_imp$Importance <- abs(var_imp$Coefficient)

# Ordering the results by importance
var_imp <- var_imp[order(-var_imp$Importance),]

# Top 20 features
top_features <- head(var_imp, 20)
ggplot(top_features, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = 'dodgerblue') +
  coord_flip() +
  labs(title = "Top 20 Important Features (Lasso Regression)", x = "Features", y = "Importance (Absolute Coefficient)") +
  theme_minimal()
