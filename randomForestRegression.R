library(randomForest) 
d1=read.table("student-mat.csv",sep=";",header=TRUE)
#d1 = d1[,-which(d1[,"G3"] == 0)]
View(d1)
d2=read.table("student-por.csv",sep=";",header=TRUE)
#d2 = d2[,-which(d2[,"G3"] == 0)]
View(d2)
dataset = rbind(d1,d2)

HOLDOUT = 0.7
training_records = 1:round(nrow(dataset)*HOLDOUT)
test_records = -training_records
train = sample(dataset[training_records,])
test = sample(dataset[test_records,])
View(train)
rf = randomForest(G3 ~ ., data=train, importance = TRUE,na.action = na.omit)
print(rf)
print(sqrt(rf$mse))
importance(rf)
#varImpPlot(rf, sort=TRUE)
library(caret)
library(e1071)
prediction = round(predict(rf, test),2)
View(prediction)
print(test$G3 - prediction)
plot(rf)
rmse = sqrt(mean(rf$mse))
print(rmse)