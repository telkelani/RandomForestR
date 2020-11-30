library(caret)
source("lab4DataPrepNew.R")
source("4labFunctions.R")
set.seed(123)
data = read.csv("student-mat.csv",sep=";")

numeric_to_nominal = function(column){
  nominal_fields = c("none","primary","5th_to_9th","secondary","higher")
  for (n in 1:nrow(data)){
    as_int = as.integer(data[n,column])
    as_int = as_int + 1
    if (as_int == 5){ 
      as_int = as_int - 1 
    }
    as_int = nominal_fields[as_int]
    
    data[n,column] = as_int
  }
  return (data)
}

data = numeric_to_nominal("Medu")
data = numeric_to_nominal("Fedu")
View(data)
numeric_fields = Filter(is.numeric,data)
numeric_fields = NPREPROCESSING_redundantFields(numeric_fields,0.6)
categorical_fields = Filter(is.character,data)
View(numeric_fields)
data = cbind(categorical_fields,numeric_fields)
View(data)
data$passed = ifelse(data$G3 >= 10, "Yes","No")
data$passed = as.factor(data$passed)
data$G3 = NULL
View(data)
training_records = sample(1:nrow(data),size=0.8*nrow(data))
train_data = data[training_records,]
test_data = data[-training_records,]
indexforCV = createFolds(train_data$passed,10,returnTrain = T)
output = train_data[,"passed"]
#input = train_data[,!(names(train_data) %in% c("passed","G2","G1"))]
input = train_data[,names(train_data)!="passed"]
View(input)
OUTPUT_FIELD = "passed"

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
test_inputs = test_data[,names(test_data)!="passed"]
prediction = predict(finalModel,test_inputs, type="prob")
View(prediction)
