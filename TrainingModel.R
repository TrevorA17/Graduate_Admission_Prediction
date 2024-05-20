# Load dataset
admission_data <- read.csv("data/Admission_Predict.csv", colClasses = c(
  GRE_Score = "numeric",
  TOEFL_Score = "numeric",
  University_Rating = "factor",
  SOP = "numeric",
  LOR = "numeric",
  CGPA = "numeric",
  Research = "factor",
  Chance_of_Admit = "numeric"
))

# Display the structure of the dataset
str(admission_data)

# View the first few rows of the dataset
head(admission_data)

# View the dataset in a separate viewer window
View(admission_data)

# Load necessary library
library(caret)

# Set seed for reproducibility
set.seed(123)

# Split the data into training and testing sets (80% training, 20% testing)
trainIndex <- createDataPartition(admission_data$Chance_of_Admit, p = 0.8, list = FALSE)
train_data <- admission_data[trainIndex, ]
test_data <- admission_data[-trainIndex, ]

# Display the number of observations in each set
cat("Number of observations in training set:", nrow(train_data), "\n")
cat("Number of observations in testing set:", nrow(test_data), "\n")

# Load necessary library
library(boot)

# Define a function to compute the statistic of interest (mean in this case)
boot_mean <- function(data, indices) {
  return(mean(data[indices]))
}

# Apply bootstrapping on the Chance_of_Admit column
set.seed(123)
bootstrap_results <- boot(data = train_data$Chance_of_Admit, statistic = boot_mean, R = 1000)

# Display the bootstrapping results
print(bootstrap_results)

# Load necessary library
library(caret)

# Define the control using a basic cross-validation (10-fold)
ctrl <- trainControl(method = "cv", number = 10)

# Train a model using cross-validation (e.g., linear model)
set.seed(123)
cv_model <- train(Chance_of_Admit ~ ., data = train_data, method = "lm", trControl = ctrl)

# Display the cross-validation results
print(cv_model)

# Load necessary libraries
library(caret)
library(rpart)
library(randomForest)

# Set seed for reproducibility
set.seed(123)

# Train a Linear Regression model
lm_model <- train(Chance_of_Admit ~ ., data = train_data, method = "lm")
print("Linear Regression Model:")
print(lm_model)

# Train a Decision Tree model
dt_model <- train(Chance_of_Admit ~ ., data = train_data, method = "rpart")
print("Decision Tree Model:")
print(dt_model)

# Train a Random Forest model
rf_model <- train(Chance_of_Admit ~ ., data = train_data, method = "rf")
print("Random Forest Model:")
print(rf_model)

# Make predictions on the test set for each model
lm_predictions <- predict(lm_model, newdata = test_data)
dt_predictions <- predict(dt_model, newdata = test_data)
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate model performance
lm_rmse <- sqrt(mean((lm_predictions - test_data$Chance_of_Admit)^2))
dt_rmse <- sqrt(mean((dt_predictions - test_data$Chance_of_Admit)^2))
rf_rmse <- sqrt(mean((rf_predictions - test_data$Chance_of_Admit)^2))

cat("RMSE for Linear Regression:", lm_rmse, "\n")
cat("RMSE for Decision Tree:", dt_rmse, "\n")
cat("RMSE for Random Forest:", rf_rmse, "\n")

# Collect resamples for model comparison
results <- resamples(list(
  Linear_Regression = lm_model,
  Decision_Tree = dt_model,
  Random_Forest = rf_model
))

# Summary of the resamples
summary(results)

# Boxplots of the resamples
bwplot(results, main = "Model Performance Comparison")

# Dot plots of the resamples
dotplot(results, main = "Model Performance Comparison")
