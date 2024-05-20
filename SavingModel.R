# Load necessary libraries
library(caret)

# Set seed for reproducibility
set.seed(123)

# Split the data into training and testing sets (80% training, 20% testing)
trainIndex <- createDataPartition(admission_data$Chance_of_Admit, p = 0.8, list = FALSE)
train_data <- admission_data[trainIndex, ]
test_data <- admission_data[-trainIndex, ]

# Train a Linear Regression model
lm_model <- train(Chance_of_Admit ~ ., data = train_data, method = "lm")

# Create a directory named "models" if it doesn't exist
if (!file.exists("./models")) {
  dir.create("./models")
}

# Save the linear regression model
saveRDS(lm_model, file = "./models/linear_regression_model.rds")

# Load the saved linear regression model
loaded_linear_regression_model <- readRDS("./models/linear_regression_model.rds")

# Prepare new data for prediction (example data similar to the admission dataset structure)
new_data <- data.frame(
  GRE_Score = c(330, 320, 310),  # Example GRE_Score values
  TOEFL_Score = c(115, 110, 105),  # Example TOEFL_Score values
  University_Rating = factor(c(5, 4, 3), levels = c(1, 2, 3, 4, 5)),  # Example University_Rating values
  SOP = c(4.5, 4.0, 3.5),  # Example SOP values
  LOR = c(4.5, 4.0, 3.5),  # Example LOR values
  CGPA = c(9.5, 8.5, 7.5),  # Example CGPA values
  Research = factor(c(1, 0, 1), levels = c(0, 1))  # Example Research values
)

# Use the loaded model to make predictions for new data
predictions_loaded_model <- predict(loaded_linear_regression_model, newdata = new_data)

# Print predictions
print(predictions_loaded_model)
