

# ************************************************
# COM3018 PRACTICAL BUSINESS ANALYTICS ASSESSMENT
# RandomForest Model
# Based on real-world dataset
#
# DATE: 1st December 2020
# VERSION: v1.05
# AUTHOR: Tarek Elkelani
#
# UPDATE
# 1.00 25/11/2020 Initial Version
# 1.01 26/11/2020 Initial RandomForest Regressor
# 1.02 28/11/2020 Added Cross Validation from Caret
# 1.03 30/11/2020 Added Metric display
# 1.04 30/11/2020 Added Outlier removal
# 1.05 01/12/2020 Added different modes (A,B,C,D)
# 1.05 01/12/2020 Added main function
# 1.06 01/12/2020 Added Cross Validation Display
# ************************************************

library(caret)
library(randomForest)
source("lab4DataPrepNew.R")
source("visualize.R")
set.seed(123)
# ************************************************
# RfRegressor() :
#  
# Use randomforest model as a regression model to 
# predict the student's final G3 grade
#
# INPUT  :  -dataframe - dataset - The dataset to use for model
#        :  -character - mode - The mode describes the variables to use as predictors
#                               A : All variables apart from G3 used as predictors
#                               B : All variables apart from G3 and G2
#                               C : All variables apart from G3, G2 and G1
#                               D : All variables apart from G3, G2 and G1 + removes 
#                                   redundant variables

#
# OUTPUT : No return values - prints out training and testing error with metrics
#                           - MAE, MSE, RMSE
# ************************************************
RfRegressor = function(dataset,mode){
  
  #Remove 0 G3, these students have not taken the final test so are not relevant
  dataset = dataset[-which(dataset$G3==0),]
  
  # Removing outliers here from absences 
  # a lot of outliers shown from boxplots
  # Removing this affects accuracy (should we get a median??)
  # boxplots = skewed_vars(dataset)
  # dataset = dataset[-which(dataset$absences %in% boxplots$out[boxplots$group == 3]),]
  
  # Separate numeric and categorical fields to put in redundant field function
  # Redundant field gets rid of collinearity only for numeric fields
  formula_string = "G3 ~ ."
  if (mode == 'D'){
    RSQUARED_THRESH = 0.6 #If two variables have an R^2 value >= 0.6, one of the variables is removed
    #0.6 indicates a medium-strong correlation
    numeric_fields = Filter(is.numeric,dataset)
    numeric_fields = NPREPROCESSING_redundantFields(numeric_fields,RSQUARED_THRESH)
    categorical_fields = Filter(is.character,dataset)
    dataset = cbind(categorical_fields,numeric_fields)
  }
  else if (mode == 'C'){
    formula_string = "G3 ~.-G1-G2"
  }
  else if (mode == 'B'){
    formula_string = "G3 ~.-G1"
  }
  formular = as.formula(formula_string)

  
  ##Setup training and testing data (we will see if CV is needed)
  #Setting training and testing records
  training_records = sample(1:nrow(dataset), size=0.75*nrow(dataset))
  training_data = dataset[training_records,]
  testing_data = dataset[-training_records,]
  
  # Random Forest Training + Cross Validation
  crossVal = trainControl(method="cv", number=10) #10-fold Cv
  
  rf = train(formular, data=training_data, method="rf",
             ntree=500, trControl=crossVal, importance=TRUE)
  
  # If you want to compare OOB with CV use this model
  #rfOOB = randomForest(formular, data=training_data, mtry=rf$finalModel$mtry)
  #print(rfOOB)
  
  ##Display CV Results
  print(formattable(rf$resample))
  ##Predicting outcome
  prediction = predict(rfOOB,testing_data)
  
  error = prediction - testing_data$G3
  mae = mean(abs(error))
  mse = mean((error^2))
  rmse = round(sqrt(mse),2)
  
  ##Training error
  
  RMSE_TRAINING = round(min(rf$results$RMSE),2)
  chosen_model = which(rf$results$RMSE == min(rf$results$RMSE))
  MAE_TRAINING = round(rf$results$MAE[chosen_model],2)
  
  # Displaying Results
  testing_results = data.frame(
    MAE = c(round(mae,2),MAE_TRAINING),
    MSE = c(round(mse,2),round(RMSE_TRAINING^2,2)),
    RMSE = c(rmse,RMSE_TRAINING),
    row.names=c("Testing Error","Training Error")
  )
  print(formattable(testing_results,"Test Metrics"))
  
  #Importance plots
  varImpPlot(rf$finalModel, n.var=10, main="Importance of Variables in Random Forest Regressor")
  
}

main = function(){
  dataset_math = read.csv("student-mat.csv",sep=";")
  # RfRegressor(dataset_math,"A")
  # RfRegressor(dataset_math,"B")
  # RfRegressor(dataset_math,"C")
  # RfRegressor(dataset_math,"D")
  dataset_por=read.csv("student-por.csv",sep=";")
}

main()
