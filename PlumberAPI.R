# Load necessary libraries
library(plumber)

# Load the saved linear regression model
loaded_linear_regression_model <- readRDS("./models/linear_regression_model.rds")

#* @apiTitle Admission Chance Prediction Model API
#* @apiDescription Used to predict the chance of admission using a linear regression model.

#* @post /predict_admission_chance
#* @param GRE_Score Numeric: GRE score (out of 340)
#* @param TOEFL_Score Numeric: TOEFL score (out of 120)
#* @param University_Rating Factor: University rating (out of 5)
#* @param SOP Numeric: Strength of Statement of Purpose (out of 5)
#* @param LOR Numeric: Strength of Letter of Recommendation (out of 5)
#* @param CGPA Numeric: Undergraduate GPA (out of 10)
#* @param Research Factor: Research experience (0 or 1)
#* @serializer unboxedJSON
predict_admission_chance <- function(GRE_Score, TOEFL_Score, University_Rating, SOP, LOR, CGPA, Research) {
  # Prepare the input data
  new_data <- data.frame(
    GRE_Score = as.numeric(GRE_Score),
    TOEFL_Score = as.numeric(TOEFL_Score),
    University_Rating = factor(as.integer(University_Rating), levels = c(1, 2, 3, 4, 5)),
    SOP = as.numeric(SOP),
    LOR = as.numeric(LOR),
    CGPA = as.numeric(CGPA),
    Research = factor(as.integer(Research), levels = c(0, 1))
  )
  
  # Use the loaded model to make predictions
  prediction <- predict(loaded_linear_regression_model, newdata = new_data)
  
  # Return the prediction
  return(as.character(prediction))
}
