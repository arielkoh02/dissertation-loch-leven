#26/3/25
#random forest regression

rm(list=ls())

#install.packages("randomForest")   
#install.packages("caret")          
library(randomForest)
library(caret)
library(tidyverse)

#load data 

alldata<-read.csv("data/csv/alldata-2004-2016v2.csv")
alldata<-alldata %>% filter(!is.na(Total.Biovolume)) %>% 
  select(-YearMonth,-X,-Month) %>% 
  mutate(Year=as.numeric(Year),Season=as.factor(Season))
View(alldata)
summary(alldata)  
colSums(is.na(alldata)) 
str(alldata)

data_impute<-rfImpute(Total.Biovolume~.,data=alldata,ntree=500)
View(data_impute)


# simplest random forest regression - without training 
rf <-randomForest(Total.Biovolume~.,data=data_impute, ntree=500) 
print(rf)


#dividing data into training and testing data
trainIndex <- createDataPartition(data_impute$Total.Biovolume, p = 0.7, list = FALSE)
trainData <- data_impute[trainIndex, ]  # 70% training data
testData <- data_impute[-trainIndex, ]  # 30% testing data

rf_model <- randomForest(
  Total.Biovolume ~ .,  # Formula: Predict Biovolume using all other variables
  data = trainData,  
  ntree = 500,  # Number of trees (default is 500)
  mtry = sqrt(ncol(trainData) - 1),  # Number of features randomly selected at each split
  importance = TRUE  # Compute variable importance
)

print(rf_model)
importance(rf_model)  # Importance scores
varImpPlot(rf_model)  # Plot variable importance
plot(rf_model)

predictions <- predict(rf_model, testData)

# Compute RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((predictions - testData$Total.Biovolume)^2))

# Compute R²
r2 <- cor(predictions, testData$Total.Biovolume)^2

# Print results
cat("R²:", r2, "\nRMSE:", rmse)

# tuning model
tuned_model <- tuneRF(
  trainData[, -which(names(trainData) == "Total.Biovolume")],  # Predictors
  trainData$Total.Biovolume,  # Response variable
  stepFactor = 1.5,  # Search range multiplier
  improve = 0.01,  # Minimum improvement needed
  ntreeTry = 500,  # Number of trees tested
  trace = TRUE  # Show progress
)

#updated/tuned model
rf_model1 <- randomForest(
  Total.Biovolume ~ .,  # Formula: Predict Biovolume using all other variables
  data = trainData,  
  ntree = 500,  # Number of trees (default is 500)
  mtry = 17,  # Number of features randomly selected at each split
  importance = TRUE  # Compute variable importance
)

# Plot OOB error vs. number of trees
print(rf_model1)
importance(rf_model1)  # Importance scores
varImpPlot(rf_model1)  # Plot variable importance
plot(rf_model1)

predictions1 <- predict(rf_model1, testData)

# Compute RMSE (Root Mean Squared Error)
rmse1 <- sqrt(mean((predictions1 - testData$Total.Biovolume)^2))

# Compute R²
r21 <- cor(predictions1, testData$Total.Biovolume)^2

# Print results
cat("R²:", r21, "\nRMSE:", rmse1)

