

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
# 1.06 01/12/2020 Added MAPE metric
# 1.07 01/12/2020 Added Classifier w/ROC curve plot
# ************************************************

library(caret)
library(randomForest)
source("lab4DataPrepNew.R")
source("visualize.R")
set.seed(123)

TRAIN_TEST_RATIO = 0.75 #75% of dataset will be training data, 25% will be testing data
RSQUARED_THRESH = 0.6 #If two variables have an R^2 value >= 0.6, one of the variables is removed

# ************************************************
# errorCatching()
# Function that is called at the beginning of models (rfRegressor & rfClassifier) to validate parameter input
# 
# INPUT  :  -dataframe - dataset - The dataset to use for model
#        :  -character - mode - The mode describes the variables to use as predictors
#                               A : All variables apart from G3 used as predictors
#                               B : All variables apart from G3 and G2
#                               C : All variables apart from G3, G2 and G1
#                               D : All variables apart from G3, G2 and G1 + removes 
#                                   redundant variables 
# OUTPUT: No return value - Will terminate program with error specified as strings
#                         - If doesn't return error, none of the stop() functions are called 
#                         - meaning the parameters are valid
errorCatching = function(data,mode){
  if (missing(data)){
    stop("Where is the data?")
  }
  if (missing(mode)){
    stop("Which predictors do you want me to use?")
  }
  if (!(is.character(mode)) || !(is.data.frame(data))){
    stop("Stop trying to break the code!
         Specify dataframe for dataset
         Specify A,B,C or D for mode")
  }
  
  if (!(mode %in% c("A","B","C","D"))){
    stop("Please specify either A, B, C or D for the second
         parameter.
         Read the comments on the top of the function for reference")
  }
}

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
# OUTPUT : none 
#
# - prints out training and testing error with metrics
# - MAE, MSE, RMSE
# ************************************************
RfRegressor = function(dataset,mode){
  #Run error Catcher before code is executed
  errorCatching(dataset,mode)
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
    formula_string = "G3 ~.-G2"
  }
  formular = as.formula(formula_string)
  print(formula_string)
  
  ##Setup training and testing data (we will see if CV is needed)
  #Setting training and testing records
  training_records = sample(1:nrow(dataset), size=TRAIN_TEST_RATIO*nrow(dataset))
  training_data = dataset[training_records,]
  testing_data = dataset[-training_records,]
  
  # Random Forest Training + Cross Validation
  crossVal = trainControl(method="cv", number=10) #10-fold Cv
  
  rf = train(formular, data=training_data, method="rf",
             ntree=500, trControl=crossVal, importance=TRUE)
  
  # If you want to compare OOB with CV use this model
  #rf = randomForest(formular, data=training_data, mtry=rf$finalModel$mtry)
  #print(rfOOB)
  
  ##Display CV Results
  #print(formattable(rf$resample))
  ##Predicting outcome
  prediction = predict(rf,testing_data)
  
  error = prediction - testing_data$G3
  mae = mean(abs(error))
  mse = mean((error^2))
  rmse = round(sqrt(mse),2)
  mape = mean(abs(testing_data$G3 - prediction)/testing_data$G3)*100
  
  ##Training error
  
  RMSE_TRAINING = round(min(rf$results$RMSE),2)
  chosen_model = which(rf$results$RMSE == min(rf$results$RMSE))
  MAE_TRAINING = round(rf$results$MAE[chosen_model],2)
  MAPE_TRAINING = mean(abs((training_data$G3-rf$finalModel$predicted)/training_data$G3)) * 100
  
  # Displaying Results
  testing_results = data.frame(
    MAE = c(round(mae,2),MAE_TRAINING),
    MSE = c(round(mse,2),round(RMSE_TRAINING^2,2)),
    RMSE = c(rmse,RMSE_TRAINING),
    MAPE = c(round(mape,2),round(MAPE_TRAINING,2)),
    row.names=c("Testing Error","Training Error")
  )
  print(formattable(testing_results,"Test Metrics"))
  print(rf$finalModel)
  #Importance plots
  varImpPlot(rf$finalModel, n.var=10, main="Importance of Variables in Random Forest Regressor")
  
}

# ************************************************
# toFactor() :
#
# Converts categorical fields to factors, to pre-process for the SMOTE balancing function
#
# INPUT : dataframe - fields - the dataset with just the categorical fields
#
# OUTPUT: dataframe - fields - the same dataset but with all the fields as factors
# ************************************************
toFactor = function(fields){
  for (n in 1:ncol(fields)){
    fields[,n] = as.factor(fields[,n])
  }
  return (fields)
}

# ************************************************
# RfClassifier() :
#  
# Use randomforest model as a classifier to 
# predict whether a student will pass the year or not
# A student has passed if they obtained >=10 on their final exam (G3)
#
#
# INPUT  :  -dataframe - dataset - The dataset to use for model
#        :  -character - mode - The mode describes the variables to use as predictors
#                               A : All variables apart from G3 used as predictors
#                               B : All variables apart from G3 and G2
#                               C : All variables apart from G3, G2 and G1
#                               D : All variables apart from G3, G2 and G1 + removes 
#                                   redundant variables

#
# OUTPUT : none 
#
# prints out Confusion Matrix, plots ROC curve and prints AUC
# prints specificity and sensitivity, kappa, accuracy
# ************************************************
RFClassifier = function(data, mode){
  errorCatching(data,mode)
  #Remove 0 final grade as it is not relevant
  dataset = dataset[-which(dataset$G3==0),]
  PASSING_GRADE = 10

  OUTPUT_FIELD = "passed" #Creating field for binary classifier
  
  #D mode removes G1,G2 and redundant variables
  if (mode == 'D'){
    formular = as.formula("passed ~ .")
    numeric_fields = Filter(is.numeric,data)
    numeric_fields = NPREPROCESSING_redundantFields(numeric_fields,0.6)
    categorical_fields = Filter(is.character,data)
    #Must convert categorical fields to factors for SMOTE
    categorical_fields = toFactor(categorical_fields)
    data = cbind(categorical_fields,numeric_fields)
    
  }
  
  else {
    # Formulas for SMOTE to balance dataset
    # Must remove predictor variables based on mode selected in parameter
    
    if (mode == 'C'){
      formular = as.formula("passed ~. -G1-G2")
    }
    else if (mode == 'B'){
      formular = as.formula("passed ~. -G2")
    }
    else{
      formular = as.formula("passed ~ .")
    }
    
    numeric_fields = Filter(is.numeric,data)
    categorical_fields = Filter(is.character,data)
    categorical_fields = toFactor(categorical_fields)
    data = cbind(categorical_fields,numeric_fields)
  }
  
  #Create the OUTPUT field, whether someone has passed or not
  data$passed = ifelse(data$G3 >= PASSING_GRADE, "Yes","No")
  data$passed = as.factor(data$passed)
  data$G3 = NULL #Delete G3 as it was used for the passed variable
  
  #Set training and test data
  training_records = sample(1:nrow(data),size=TRAIN_TEST_RATIO*nrow(data))
  train_data = data[training_records,]
  test_data = data[-training_records,]
  
  library(e1071)
  library(DMwR)
  #Balance the dataset, by creating synthetic records to balance the failures and the successes
  #Best results were Maths: perc.over=94,perc.under=740 , Por: perc.over=100,perc.under=210
  balanced = DMwR::SMOTE(formular, data=train_data, perc.over=94,perc.under=740)
  #Cross Validation
  indexforCV = createFolds(balanced$passed,k=10,returnTrain = T)
  
  # Must be separated like this because train() function doesn't work with formula input
  output = balanced[,OUTPUT_FIELD]
  if (mode == 'B'){
    input = balanced[,!(names(balanced) %in% c(OUTPUT_FIELD,"G2"))]
  }
  else if (mode == 'C'){
    input = balanced[,!(names(balanced) %in% c(OUTPUT_FIELD,"G2","G1"))]
  }
  else{ #D already removed all the redundant fields (including G1 and G2)
    input = balanced[,names(balanced)!=OUTPUT_FIELD]
  }
  
  
  
  # RandomForest Model
  tC = trainControl(index=indexforCV,method="cv", number="10")
  rfCrossVal = train(input,output,data=train_data,trControl=tC, importance=TRUE, proximity=TRUE,method="rf")
  
  finalModel=rfCrossVal$finalModel
  
  # Prediction and output confusion matrix
  pred = predict(rfCrossVal,test_data)
  print(confusionMatrix(pred,test_data$passed, positive="Yes"))
  
  #ROC curve w/AUC
  library(ROSE)
  AUC = roc.curve(test_data$passed,pred)
  print(AUC)
}
# ************************************************
# main()
# 
# Loads datasets and runs all the code by calling the regressor and classifier functions
#
# INPUT : none
# OUTPUT : none
# ************************************************
main = function(){
  dataset_math = read.csv("student-mat.csv",sep=";")
  dataset_por=read.csv("student-por.csv",sep=";")
  MODE = 'C'

  RfRegressor(dataset_math,MODE)
  RfRegressor(dataset_por,MODE)
  RFClassifier(dataset_math,MODE)
  RFClassifier(dataset_por,MODE)
}

main()
