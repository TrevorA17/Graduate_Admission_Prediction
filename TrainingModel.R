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

