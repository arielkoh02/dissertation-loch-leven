#26/3/25
#random forest regression

rm(list=ls())

#install.packages("randomForest")   
#install.packages("caret")          
library(randomForest)
library(caret)
library(tidyverse)

#load data 

set.seed(888)

alldata<-read.csv("data/csv/alldata-2004-2016v2.csv")
alldata<-alldata %>% filter(!is.na(Total.Biovolume)) %>% 
  select(-YearMonth,-X,-Month,-Cryto.Biovolume,-Cyano.Biovolume,-Diatoms.Biovolume,-Greens.Biovolume,-RB5.ChlA) %>% 
  mutate(Year=as.numeric(Year),Season=as.factor(Season)) 
View(alldata)
summary(alldata)  
colSums(is.na(alldata)) 
str(alldata)


#with imputed data
data_impute<-rfImpute(Total.Biovolume~.,data=alldata,ntree=500)
View(data_impute)

#dividing data into training and testing data
trainIndex1 <- createDataPartition(data_impute$Total.Biovolume, p = 0.7, list = FALSE)
trainData1 <- data_impute[trainIndex1, ]  # 70% training data
testData1 <- data_impute[-trainIndex1, ]  # 30% testing data

rfmodel1 <- randomForest(
  Total.Biovolume ~ .,  # Formula: Predict Biovolume using all other variables
  data = trainData1,  
  ntree = 500,  # Number of trees (default is 500)
  mtry = sqrt(ncol(trainData1) - 1),  # Number of features randomly selected at each split
  importance = TRUE  # Compute variable importance
)

print(rfmodel1)
rfmodel1$importance # Importance scores
varImpPlot(rfmodel1)  # Plot variable importance
plot(rfmodel1)

predictions <- predict(rfmodel1, testData1)

# Compute RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((predictions - testData1$Total.Biovolume)^2))

# Compute R²
pearsonscorrcoef1 <- cor(predictions, testData1$Total.Biovolume)^2

# Print results
cat("Pearson's Correlation Coefficient:", pearsonscorrcoef1, "\nRMSE:", rmse)

# tuning model
tuned_model <- tuneRF(
  trainData1[, -which(names(trainData1) == "Total.Biovolume")],  # Predictors
  trainData1$Total.Biovolume,  # Response variable
  stepFactor = 1.5,  # Search range multiplier
  improve = 0.01,  # Minimum improvement needed
  ntreeTry = 500,  # Number of trees tested
  trace = TRUE  # Show progress
)

#updated/tuned model
rfmodel1opt <- randomForest(
  Total.Biovolume ~ .,  # Formula: Predict Biovolume using all other variables
  data = trainData1,  
  ntree = 500,  # Number of trees (default is 500)
  mtry = 6,  # Number of features randomly selected at each split
  importance = TRUE  # Compute variable importance
)

# Plot OOB error vs. number of trees
print(rfmodel1opt)
rfmodel1opt$importance # Importance scores
varImpPlot(rfmodel1opt)  # Plot variable importance
plot(rfmodel1opt)

predictions1opt <- predict(rfmodel1opt, testData1)

# Compute RMSE (Root Mean Squared Error)
rmse1opt <- sqrt(mean((predictions1opt - testData1$Total.Biovolume)^2))

# Compute R²
pearsonscorrcoef1opt <- cor(predictions1opt, testData1$Total.Biovolume)^2

# Print results
cat("Pearson's Correlation Coefficient:", pearsonscorrcoef1opt, "\nRMSE:", rmse1opt)

# Extract importance values for rfmodel1opt
imp1 <- importance(rfmodel1opt, type = 1)  # type = 1 for %IncMSE
imp1_df <- as.data.frame(imp1)
imp1_df$vars <- rownames(imp1_df)

# Sort by importance
imp1_df <- imp1_df[order(imp1_df$`%IncMSE`), ]

# Plot dotchart
dotchart(
  imp1_df$`%IncMSE`, 
  labels = imp1_df$vars,
  xlim = c(0, max(imp1_df$`%IncMSE`, na.rm = TRUE) * 1.1),
  pch = 1,
  xlab = "% Increase in MSE", 
  ylab="Predictors"
)


#set.seed for NA data 
set.seed(40)

#removing NAs
View(alldata)

noNAdata<-alldata %>% drop_na() 

View(noNAdata)

#dividing data into training and testing data
trainIndex2 <- createDataPartition(noNAdata$Total.Biovolume, p = 0.7, list = FALSE)
trainData2 <- noNAdata[trainIndex2, ]  # 70% training data
testData2 <- noNAdata[-trainIndex2, ]  # 30% testing data

rfmodel2 <- randomForest(
  Total.Biovolume ~ .,  # Formula: Predict Biovolume using all other variables
  data = trainData2,  
  ntree = 500,  # Number of trees (default is 500)
  mtry = sqrt(ncol(trainData2) - 1),  # Number of features randomly selected at each split
  importance = TRUE  # Compute variable importance
)

print(rfmodel2)
importance(rfmodel2)  # Importance scores
varImpPlot(rfmodel2)  # Plot variable importance
plot(rfmodel2)

predictions2 <- predict(rfmodel2, testData2)

# Compute RMSE (Root Mean Squared Error)
rmse2 <- sqrt(mean((predictions2 - testData2$Total.Biovolume)^2))

# Compute R²
pearsonscorrcoef2 <- cor(predictions2, testData2$Total.Biovolume)^2

# Print results
cat("R²:", pearsonscorrcoef2, "\nRMSE:", rmse2)

# tuning model
tuned_model2 <- tuneRF(
  trainData2[, -which(names(trainData2) == "Total.Biovolume")],  # Predictors
  trainData2$Total.Biovolume,  # Response variable
  stepFactor = 1.5,  # Search range multiplier
  improve = 0.01,  # Minimum improvement needed
  ntreeTry = 500,  # Number of trees tested
  trace = TRUE  # Show progress
)

#updated/tuned model
rfmodel2opt <- randomForest(
  Total.Biovolume ~ .,  # Formula: Predict Biovolume using all other variables
  data = trainData2,  
  ntree = 500,  # Number of trees (default is 500)
  mtry = 4,  # Number of features randomly selected at each split
  importance = TRUE  # Compute variable importance
)

# Plot OOB error vs. number of trees
print(rfmodel2opt)
importance(rfmodel2opt)  # Importance scores
varImpPlot(rfmodel2opt)  # Plot variable importance
plot(rfmodel2opt)

predictions2opt <- predict(rfmodel2, testData2)

# Compute RMSE (Root Mean Squared Error)
rmse2opt <- sqrt(mean((predictions2opt - testData2$Total.Biovolume)^2))

# Compute R²
pearsonscorrcoef2opt <- cor(predictions2opt, testData2$Total.Biovolume)^2

# Print results
cat("Pearsons Correlation Coefficient:", pearsonscorrcoef2opt, "\nRMSE:", rmse2opt)

# Extract importance values
imp <- importance(rfmodel2opt, type = 1)  # type = 1 gives %IncMSE
imp_df <- as.data.frame(imp)
imp_df$vars <- rownames(imp_df)

# Sort variables by importance
imp_df <- imp_df[order(imp_df$`%IncMSE`), ]  # or use MeanDecreaseAccuracy if your column is named that

# Plot with dotchart
dotchart(
  imp_df$`%IncMSE`, 
  labels = imp_df$vars,
  xlim = c(-2, max(imp_df$`%IncMSE`, na.rm = TRUE) * 1.1),
  pch = 1,
  xlab = "% Increase in MSE", 
  ylab="Predictors"
)
