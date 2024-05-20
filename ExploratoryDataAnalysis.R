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

# Measures of Frequency
# Count of research experience
research_count <- table(admission_data$Research)
print("Frequency of Research Experience:")
print(research_count)

# Measures of Central Tendency
# Mean, Median, and Mode of GRE Score
gre_mean <- mean(admission_data$GRE_Score)
gre_median <- median(admission_data$GRE_Score)
gre_mode <- as.numeric(names(sort(table(admission_data$GRE_Score), decreasing = TRUE)[1]))
print("Measures of Central Tendency for GRE Score:")
print(paste("Mean:", gre_mean))
print(paste("Median:", gre_median))
print(paste("Mode:", gre_mode))

# Measures of Distribution
# Range and Standard Deviation of CGPA
cgpa_range <- range(admission_data$CGPA)
cgpa_sd <- sd(admission_data$CGPA)
print("Measures of Distribution for CGPA:")
print(paste("Range:", cgpa_range))
print(paste("Standard Deviation:", cgpa_sd))

# Measures of Relationship
# Correlation between TOEFL Score and Chance of Admit
toefl_chance_cor <- cor(admission_data$TOEFL_Score, admission_data$Chance_of_Admit)
print("Correlation between TOEFL Score and Chance of Admit:")
print(toefl_chance_cor)

# Perform ANOVA
# Considering University Rating as a factor variable
anova_result <- aov(Chance_of_Admit ~ University_Rating, data = admission_data)

# Summary of ANOVA results
summary(anova_result)

