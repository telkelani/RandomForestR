set.seed(123)
library(randomForest) 
source("preprocessing.R")
math_data = read.csv("student-mat.csv", sep=";")
math_train = getTrainTestData(math_data)[[1]]
math_test = getTrainTestData(math_data)[[2]]
rf_math_A = randomForest(G3 ~ ., data=math_train, mtry=10)
rf_math_B = randomForest(G3 ~ .-G2, data=math_train, mtry=10)
rf_math_C = randomForest(G3 ~ .-G2-G1, data=math_train, do.trace=T)
varImpPlot(rf_math_C)
print(rf_math_C)
rmse_oob_A = sqrt(rf_math_A$mse[rf_math_A$ntree])
rmse_oob_B = sqrt(rf_math_B$mse[rf_math_B$ntree])
rmse_oob_C = sqrt(rf_math_C$mse[rf_math_C$ntree])
#print(c("rmse oob",rmse_oob))
df = data.frame(
  rmseA = round(rmse_oob_A,2),
  rmseB = round(rmse_oob_B,2),
  rmseC = round(rmse_oob_C,2))
rownames(df)= "model"

df = rbind(df, "paper" = c(1.75,2.46,3.90))
View(df)



por_data = read.csv("student-por.csv",sep=';')
por_train = getTrainTestData(por_data)[[1]]
por_test = getTrainTestData(por_data)[[2]]

# print(rf_math)
# print(sqrt(rf$mse))
#importance(rf)
#rf_por = randomForest(G3 ~ .-G2-G1,data=por_train, importance =TRUE)
#print(rf_por)


#test_input = math_test[,names(math_test)!=c("G3","school")]
math_prediction_A = predict(rf_math_A, math_test)
math_prediction_B = predict(rf_math_B, math_test)
math_prediction_C = predict(rf_math_C,math_test)
##Mean Squared Error
error_A = math_prediction_A - math_test$G3
error_B = math_prediction_B - math_test$G3
error_C = math_prediction_C - math_test$G3

getMaes = function(){
  mae_A = mean(abs(error_A))
  mae_B = mean(abs(error_B))
  mae_C = mean(abs(error_C))
  return (c(round(mae_A,2),round(mae_B,2),round(mae_C,2)))
}

getMses = function(){
  mse_A = mean((error_A)^2)
  mse_B = mean((error_B)^2)
  mse_C = mean((error_C)^2)
  return (c(round(mse_A,2),round(mse_B,2),round(mse_C,2)))
}

getRMSES = function(){
  rmse_A = sqrt(mean(error_A^2))
  rmse_B = sqrt(mean(error_B^2))
  rmse_C = sqrt(mean(error_C^2))
  return (c(round(rmse_A,2),round(rmse_B,2),round(rmse_C,2)))
}

metrics = data.frame(
  Models = c("A","B","C"),
  mae = getMaes(),
  mse = getMses(),
  rmse = getRMSES(),
  mtry = c(rf_math_A$mtry,rf_math_B$mtry,rf_math_C$mtry)
)
View(metrics)

#print(math_prediction - math_test$G3)
#print(mean(abs(math_prediction-math_test$G3)))


# library(caret)
# library(e1071)
#prediction = round(predict(rf_por, por_test))
#View(prediction)
# print(test$G3 - prediction)
# plot(rf)
# rmse = sqrt(mean(rf$mse))
# print(rmse)