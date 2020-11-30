##RandomForest w/ Cross Validation (Math Dataset)
library(caret)
library(randomForest)
source("lab4DataPrepNew.R")
source("visualize.R")
set.seed(123)

##Setup training and testing data (we will see if CV is needed)
dataset = read.csv("student-mat.csv", sep=";")
#Remove 0 G3, these students have not taken the final test so are not relevant
dataset = dataset[-which(dataset$G3==0),]

##Removing outliers here from absences 
##a lot of outliers shown from boxplots
boxplots = skewed_vars(dataset)
dataset = dataset[-which(dataset$absences %in% boxplots$out[boxplots$group == 3]),]

#Separate numeric and categorical fields to put in redundant field function
#Redundant field gets rid of collinearity
numeric_fields = Filter(is.numeric,dataset)
numeric_fields = NPREPROCESSING_redundantFields(numeric_fields,0.6)
categorical_fields = Filter(is.character,dataset)
dataset = cbind(categorical_fields,numeric_fields)

#Setting training and testing records
training_records = sample(1:nrow(dataset), size=0.7*nrow(dataset))
training_data = dataset[training_records,]
testing_data = dataset[-training_records,]

##Random Forest Training + Cross Validation
crossVal = trainControl(method="cv", number=10) #10-fold Cv
rf = train(G3 ~ ., data=training_data, method="rf",
             ntree=500, trControl=crossVal, importance=TRUE)

##Predicting outcome
prediction = predict(rf,testing_data)

error = prediction - testing_data$G3
mae = mean(abs(error))
mse = mean((error^2))
rmse = round(sqrt(mse),2)
print(rmse)

##Training error

RMSE_TRAINING = round(min(rf$results$RMSE),2)
chosen_model = which(rf$results$RMSE == min(rf$results$RMSE))
MAE_TRAINING = round(rf$results$MAE[chosen_model],2)

##Displaying Results
testing_results = data.frame(
  MAE = c(round(mae,2),MAE_TRAINING),
  MSE = c(round(mse,2),round(RMSE_TRAINING^2,2)),
  RMSE = c(rmse,RMSE_TRAINING),
  row.names=c("Testing Error","Training Error")
)
print(formattable(testing_results,"Test Metrics"))

#Importance plots
varImpPlot(rf$finalModel, n.var=10, main="Importance of Variables in Random Forest Regressor")

