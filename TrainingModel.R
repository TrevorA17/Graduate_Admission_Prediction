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