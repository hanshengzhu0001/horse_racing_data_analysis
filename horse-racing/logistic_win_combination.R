library(tidyverse)
library(caret)
library(nnet)  # for multinomial logistic regression
library(pROC)

# Load the data
data_clean <- read.csv('horse_racing_cleaned.csv')

# Ensure the target variable is a factor
data_clean$win_combination1 <- as.factor(data_clean$win_combination1)

predictors <- c('horse_ratings_avg', 'declared_weight', 'actual_weight', 'win_odds', 
                'place_odds', 'horse_age', 'horse_country_code', 'trainer_id', 'jockey_id',
                'horse_gear_code', 'venue_code', 'position_sec3', 'position_sec4','distance', 
                'surface', 'race_class', 'behind_sec1', 'behind_sec2', 
                'behind_sec3', 'behind_sec4','prize')

# Prepare the data for training and testing
set.seed(123)
training_index <- sample(1:nrow(data_clean), 0.7 * nrow(data_clean))
training_data <- data_clean[training_index, ]
testing_data <- data_clean[-training_index, ]

# Remove columns with NA values
cols_with_na <- sapply(training_data, function(x) any(is.na(x)))
training_data_clean <- training_data[, !cols_with_na]
testing_data_clean <- testing_data[, !cols_with_na]

# Ensure that predictors are available in the training data
predictors <- predictors[predictors %in% names(training_data_clean)]

# Train the multinomial logistic regression model using nnet
multinom_model <- multinom(win_combination1 ~ ., data = training_data_clean[, c(predictors, 'win_combination1')])

# Summary of the model
summary(multinom_model)

# Predict on the test data
test_probabilities <- predict(multinom_model, testing_data_clean[, predictors], type = "probs")

# Convert probabilities to predicted classes
# Get the class with the maximum probability
test_predicted_classes <- colnames(test_probabilities)[max.col(test_probabilities, ties.method = "first")]

# Calculate accuracy and other performance metrics
confusion_matrix <- table(Predicted = test_predicted_classes, Actual = testing_data_clean$win_combination1)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

# Initialize an empty list to store ROC curves
roc_curves <- list()
auc_values <- numeric(length(levels(testing_data_clean$win_combination1)))

# Loop through each class and calculate ROC curve and AUC
for(i in seq_along(levels(testing_data_clean$win_combination1))) {
  # Binary classification: current class against all others
  actual_binary <- ifelse(testing_data_clean$win_combination1 == levels(testing_data_clean$win_combination1)[i], 1, 0)
  pred_prob <- test_probabilities[, i]
  
  # ROC curve
  roc_curves[[i]] <- roc(actual_binary, pred_prob)
  
  # AUC
  auc_values[i] <- auc(roc_curves[[i]])
  cat("AUC for", levels(testing_data_clean$win_combination1)[i], ": ", auc_values[i], "\n")
}

# Calculate mean AUC across all classes for overall performance
mean_auc <- mean(auc_values)
cat("\nMean AUC: ", mean_auc, "\n")

# ROC curves
par(mfrow = c(ceiling(sqrt(length(roc_curves))), ceiling(sqrt(length(roc_curves)))))
for (i in seq_along(roc_curves)) {
  plot(roc_curves[[i]], main = paste("ROC Curve for", levels(testing_data_clean$win_combination1)[i]))
}


# Variable Importance
# Extract coefficients from the multinom model
coef_multinom <- coefficients(multinom_model)



