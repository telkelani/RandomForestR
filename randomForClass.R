
RFClassifier = function(data){
  library(caret)
  source("lab4DataPrepNew.R")
  source("4labFunctions.R")
  set.seed(123)
  numeric_fields = Filter(is.numeric,data)
  numeric_fields = NPREPROCESSING_redundantFields(numeric_fields,0.6)
  categorical_fields = Filter(is.character,data)
  for (n in 1:ncol(categorical_fields)){
    categorical_fields[,n] = as.factor(categorical_fields[,n])
  }
  data = cbind(categorical_fields,numeric_fields)
  data$passed = ifelse(data$G3 >= 10, "Yes","No")
  data$passed = as.factor(data$passed)
  data$G3 = NULL
  training_records = sample(1:nrow(data),size=0.75*nrow(data))
  
  OUTPUT_FIELD = "passed"
  train_data = data[training_records,]
  test_data = data[-training_records,]
  
  library(e1071)
  library(DMwR)
  balanced = DMwR::SMOTE(passed ~ ., data=train_data, perc.over=100)
  indexforCV = createFolds(balanced$passed,k=10,returnTrain = T)
  output = balanced[,OUTPUT_FIELD]
  input = balanced[,names(balanced)!=OUTPUT_FIELD]
  
  
  
  tC = trainControl(index=indexforCV,method="cv", number="10")
  rfCrossVal = train(input,output,trControl=tC, importance=TRUE, proximity=TRUE,method="rf")
  
  finalModel=rfCrossVal$finalModel
  
  #tree=getTree(finalModel,500)
  #rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
  
  
  # library(inTrees)
  # treeList = RF2List(finalModel)
  # exec = extractRules(treeList,input)
  # ruleMetric = getRuleMetric(exec,input,output)
  # readRules = presentRules(ruleMetric,colnames(input))
  # View(readRules)
  
  # rfOOB = randomForest(passed ~ .-G2-G1, data=train_data, importance=TRUE)
  # print(rfOOB)
  # print(rfOOB)
  # trainx = train_data[,names(train_data)!="passed"]
  # trainy = train_data[,"passed"]
  # rfCV = rfcv(trainx,trainy, cv.fold=10)
  #test_inputs = test_data[,!(names(test_data) %in% c("passed","G1","G2"))]
  #test_inputs = test_data[,names(test_data)!="passed"]
  #prediction = predict(finalModel,test_inputs, type="prob")
  prediction = predict(finalModel,test_data)
  print(confusionMatrix(prediction,test_data$passed, positive="Yes"))
}


main = function(){
  dataset_math = read.csv("student-mat.csv", sep=";")
  RFClassifier(dataset_math)
  
  dataset_por = read.csv("student-por.csv", sep=";")
  RFClassifier(dataset_por)
}