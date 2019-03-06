
library(caret)
library(e1071)

## Read in data table
library(foreign)
df <- read.dta("C:/Users/sroberts/Downloads/hsbdemo.dta", convert.dates=TRUE, convert.factors=TRUE)

## Partitioning data
set.seed(7267166)
trainIndex = createDataPartition(df$prog, p=0.7)$Resample1
train = df[trainIndex, ]
test = df[-trainIndex, ]
glimpse(train)
glimpse(test)
# check the balance
print(table(df$prog))
print(table(train$prog))

## Naive Bayes Classifier (using e1071 package)
NBclassfier = naiveBayes(prog ~ science + socst, data=train)
print(NBclassfier)

## Naive Bayes Classifier (using naivebayes package, with Kernal-based density)
library(naivebayes)
newNBclassifier = naive_bayes(prog ~ ses + science + socst, usekernel=T, data=train)


## Generate Predictions
printALL = function(model){
  trainPred = predict(model, newdata=train, type="class")
  trainTable = table(train$prog, trainPred)
  testPred = predict(NBclassfier, newdata=test, type="class")
  testTable = table(test$prog, testPred)
  trainAcc = (trainTable[1,1] + trainTable[2,2] + trainTable[3,3]) / sum(trainTable)
  testAcc = (testTable[1,1] + testTable[2,2] + testTable[3,3]) / sum(testTable)
  message("Contingency Table for Training Data")
  print(trainTable)
  message("Contingency Table for Test Data")
  print(testTable)
  message("Accuracy")
  print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc), 3))
}
printALL(NBclassfier)
printALL(newNBclassifier)
