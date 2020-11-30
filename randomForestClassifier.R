##Read data

set.seed(123)
# Load libraries #
library(stats)
library(dplyr)
library(randomForest)

#Combine math and portuguese datasets
d1=read.table("student-mat.csv",sep=";",header=TRUE)
#d1 = d1[,-which(d1[,"G3"] == 0)]
#View(d1)
d2=read.table("student-por.csv",sep=";",header=TRUE)
#d2 = d2[,-which(d2[,"G3"] == 0)]
#View(d2)
#dataset = rbind(d1,d2)
dataset=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

#G3 is a numeric value. We do not care about how well the student is doing 
#We are assuming a student that is performing well has passed in their final grade

dataset$passed="passed" #Make a new categorical field for target value

#For each row in the dataset, if the student obtained a final
for (n in 1:nrow(dataset)){
  #print(dataset$G3)
  if (dataset[n,"G3.x"] >= 9.5 && dataset[n,"G3.y"]){
    dataset[n,"passed"] = "Yes"
  }
  else{
    dataset[n,"passed"] = "No"
  }
}

good_students = dataset[which(dataset$passed == "Yes"),]
View(good_students)
print(nrow(dataset)-nrow(good_students))
# for (n in 1:ncol(dataset)){
#   dataset[,n] = as.factor(dataset[,n])
# }
#columns = c("Pstatus","Medu","Fedu","Mjob","Fjob","guardian","famsup","famrel","passed")
#columns = names(dataset)
HOLDOUT = 0.8
training_records = 1:round(nrow(dataset)*HOLDOUT)
test_records = -training_records
train = sample(dataset[training_records,])

test = sample(dataset[test_records,])
train_output = train[,"passed"]
train_input = subset(train, select=-c(passed,G3.x,G3.y,G2.x,G2.y,G1.x,G1.y))
print(names(train_input))

FOREST_SIZE = 1000
#rf = randomForest::randomForest(train_input,as.factor(train_output),ntree=FOREST_SIZE,
                                #importance=TRUE,
                                #mtry=sqrt(ncol(train_input)))

rf = randomForest(as.factor(passed) ~ .-G3.x-G3.y-G2.x-G2.y-G1.x-G1.y, data=train)
#print(rf)
#varImpPlot(rf, sort=TRUE)
library(caret)
library(e1071)
library(DMwR)
#rf = randomForest(as.factor(train_output) ~ names(train_input), data=train)
balanced = DMwR::SMOTE(passed ~ .-G3.x-G3.y-G2.x-G2.y-G1.x-G1.y, data=train, perc.over=100,perc.under=150)
#perc.over = how much extra data will be oversampled in minority class
#100% more data in minority class 
#(failures were 21% of data now they are 30%)
#perc.under = how much extra data will be undersampled in majority class
#450% less data in minority class
#(passes were 78% now they are 69%)
#print(table(balanced$passed))
#Do the randomforest on balanced data now
balancedRf = randomForest(as.factor(passed) ~ .-G3.x-G3.y-G2.x-G2.y-G1.x-G1.y, data=balanced,ntree=1000)
print(balancedRf)
#training_inputs = dplyr::select(train,-c("passed","G3","G2","G1"))
#training_output = train[,"passed"]
#print(class(training_inputs))
# View(training_inputs)
# View(training_output)
# rfModel = rfcv(training_inputs, training_output, 
#                cv.fold=5,
#                scale="log",
#                recursive=TRUE)
# print(ncol(training_inputs))
# print(rfModel)
#print(confusionMatrix(predict(rfModel,test), test$passed))
#predict on test set and print confusion matrix
print(confusionMatrix(predict(balancedRf,test), test$passed))
#Plot the importance variables. (absence, past class failures and age are most contributing factors)
varImpPlot(balancedRf, sort=TRUE)


# myTitle<-(paste("Preprocessed Dataset. Random Forest=",FOREST_SIZE,"trees"))
# importance<-randomForest::importance(balancedRf,scale=TRUE,type=1)
# importance<-importance[order(importance,decreasing=TRUE),,drop=FALSE]
# colnames(importance)<-"Strength"
# barplot(t(importance),las=2, border = 0,
#        cex.names =0.7,
#        main=myTitle)
# print(formattable::formattable(data.frame(importance)))


