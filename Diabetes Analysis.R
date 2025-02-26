rm(list = ls())
cat("\014")

#Import Libraries
library(readxl)
library(readr)
library(caret)
library(ggplot2)
library(e1071)      # For SVM
library(class)      # For KNN
library(tidyverse)
library(xgboost)      # For XGBoost
library(randomForest) # For Random Forest
library(ggplot2)      # For visualization
library(dplyr)

#Load Diabetes Dataset
diabetes_df <- read_csv("/Users/keriwilkins/Downloads/diabetes.csv.xls")

#Data Inspeation + Pre-processing
#Intial Data Inspection
head(diabetes_df, 5)
colnames(diabetes_df)
str(diabetes_df)
colSums(is.na(diabetes_df))
summary(diabetes_df)

#Data Analysis Part I: Descriptive Statisticss
#Pregnacies Column
mean(diabetes_df$Pregnancies)
median(diabetes_df$Pregnancies)
sd(diabetes_df$Pregnancies)

#Glucose Column
mean(diabetes_df$Glucose)
median(diabetes_df$Glucose)
sd(diabetes_df$Glucose)

#Blood Pressure Column
mean(diabetes_df$BloodPressure)
median(diabetes_df$BloodPressure)
sd(diabetes_df$BloodPressure)

#SkinThickness Column
mean(diabetes_df$SkinThickness)
median(diabetes_df$SkinThickness)
sd(diabetes_df$SkinThickness)

#Insulin Column
mean(diabetes_df$Insulin)
median(diabetes_df$Insulin)
sd(diabetes_df$Insulin)

#BMI Column
mean(diabetes_df$BMI)
median(diabetes_df$BMI)
sd(diabetes_df$BMI)

#DiabetesPedigreeFunction Column
mean(diabetes_df$DiabetesPedigreeFunction)
median(diabetes_df$DiabetesPedigreeFunction)
sd(diabetes_df$DiabetesPedigreeFunction)

#Age Column
mean(diabetes_df$Age)
median(diabetes_df$Age)
sd(diabetes_df$Age)

#Outcome Column
mean(diabetes_df$Outcome)
median(diabetes_df$Outcome)
sd(diabetes_df$Outcome)

#Data Inspection
#Rows where values are 0
diabetes_df %>% filter(BloodPressure == 0)
diabetes_df %>% filter(Insulin == 0)
diabetes_df %>% filter(SkinThickness == 0)
diabetes_df %>% filter(BMI == 0)

#Data Cleaning
#Replace 0's with Median
median_blood_pressure <- median(diabetes_df$BloodPressure, na.rm = TRUE)
median_insulin <- median(diabetes_df$Insulin, na.rm = TRUE)
median_bmi <- median(diabetes_df$BMI, na.rm = TRUE)
diabetes_df$BloodPressure[diabetes_df$BloodPressure == 0] <- median_blood_pressure
diabetes_df$Insulin[diabetes_df$Insulin == 0] <- median_insulin
diabetes_df$BMI[diabetes_df$BMI == 0] <- median_bmi
median_skin_thickness <- median(diabetes_df$SkinThickness, na.rm = TRUE)
diabetes_df$SkinThickness[diabetes_df$SkinThickness == 0] <- median_skin_thickness

summary(diabetes_df)
head(diabetes_df, 10)


# Boxplot of `BMI` vs `Outcome`
ggplot(diabetes_df, aes(x = factor(Outcome), y = BMI)) +
  geom_boxplot(color = "darkgrey", fill = "grey") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  labs(title = "BMI by Diabetes Outcome", x = "Outcome", y = "BMI")


# Violin of `Glucose` vs `Outcome`
ggplot(diabetes_df, aes(x = factor(Outcome), y = Glucose)) +
  geom_violin(fill = "lightgray", color = "black") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  labs(title = "Glucose Levels by Diabetes Outcome", x = "Outcome", y = "Glucose")


#Pregnancies Vs. Outcome
ggplot(diabetes_df, aes(x = Pregnancies, fill = factor(Outcome))) +
  geom_histogram(position = "stack", bins = 30, color = "black", alpha = 0.7) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  labs(title = "Stacked Histogram of Pregnancies by Outcome", x = "Number of Pregnancies", y = "Frequency") +
  scale_fill_manual(values = c("lightgray", "black"))  # Customize the colors



#Data Analysis Part II: Inferential Statistics and Machine Learning
#Aim 1.Inferential Statistics
#Test relationship between DiabetesPedigreeFunction and Outcome)
model <- glm(Outcome ~ DiabetesPedigreeFunction, data = diabetes_df, family = "binomial")
summary(model)
ggplot(diabetes_df, aes(x = DiabetesPedigreeFunction, fill = factor(Outcome))) +
  geom_density(alpha = 0.5, adjust = 1.5) +  # Increase adjust parameter to smooth the plot more
  geom_density(aes(color = factor(Outcome)), size = 1) +  # Add density lines for each outcome
  labs(
    x = "Diabetes Pedigree Function",
    title = "Density of DiabetesPedigreeFunction by Outcome",
    y = "Density",
    fill = "Outcome",
    color = "Outcome"  # For density line legend
  ) +
  scale_fill_manual(values = c("darkgrey", "lightgrey")) +  # Use more contrasting colors
  scale_color_manual(values = c("black", "black")) +  # Matching colors for lines
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    legend.position = "top"  # Move the legend to the top for better visibility
  )

# Set seed for reproducibility
set.seed(123)

# Convert Outcome to a factor (binary classification)
diabetes_df$Outcome <- factor(diabetes_df$Outcome, levels = c(0, 1))

# Split the data into training and test sets (80% train, 20% test)
trainIndex <- createDataPartition(diabetes_df$Outcome, p = 0.8, list = FALSE)
train_data <- diabetes_df[trainIndex, ]
test_data <- diabetes_df[-trainIndex, ]

# Normalize the numeric features (KNN and SVM are sensitive to feature scaling)
preprocess_params <- preProcess(train_data[, -9], method = "scale")
train_scaled <- predict(preprocess_params, train_data[, -9])
test_scaled <- predict(preprocess_params, test_data[, -9])

# Add Outcome back to the scaled data
train_scaled$Outcome <- train_data$Outcome
test_scaled$Outcome <- test_data$Outcome

### 1. Feature Selection Using XGBoost ###
# Prepare the data for XGBoost (exclude Outcome for feature selection)
X_train_xgb <- as.matrix(train_scaled %>% select(-Outcome))
X_test_xgb <- as.matrix(test_scaled %>% select(-Outcome))
y_train_xgb <- train_scaled$Outcome
y_test_xgb <- test_scaled$Outcome

# Convert outcome to numeric (0 and 1 format for XGBoost)
y_train_xgb <- as.numeric(y_train_xgb) - 1
y_test_xgb <- as.numeric(y_test_xgb) - 1

# Create DMatrix (XGBoost internal format)
dtrain <- xgb.DMatrix(data = X_train_xgb, label = y_train_xgb)
dtest <- xgb.DMatrix(data = X_test_xgb, label = y_test_xgb)

# Set parameters for the XGBoost model (for feature selection)
params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 6,
  eta = 0.1,
  nthread = parallel::detectCores() - 1
)

# Train the XGBoost model
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, test = dtest),
  verbose = 1
)
# Get the feature importance from XGBoost
importance_matrix <- xgb.importance(feature_names = colnames(X_train_xgb), model = xgb_model)
# Print feature importance matrix
print(importance_matrix)
# Plot feature importance
xgb.plot.importance(importance_matrix)


# Boxplot of `BMI` vs `Outcome`
ggplot(diabetes_df, aes(x = factor(Outcome), y = BMI)) +
  geom_boxplot(color = "darkgrey", fill = "grey") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  labs(title = "BMI by Diabetes Outcome", x = "Outcome", y = "BMI")





