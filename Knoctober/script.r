setwd("D:/Kaggle/Analyticsvaidya/Knoctober")
train <- read.csv("train.csv", stringsAsFactors = F)
first <- read.csv("First_Health_Camp_Attended.csv", stringsAsFactors = F)
second <- read.csv("Second_Health_Camp_Attended.csv", stringsAsFactors = F)
third <- read.csv("Third_Health_Camp_Attended.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)
test1 <- read.csv("test.csv", stringsAsFactors = F)

library(Amelia)
missmap(train, col = c("yellow", "blue"))
missmap(first)
missmap(second)
missmap(third)

#no data missing in either train, first or second
#the aim is now to check what drives the people to attend any of the camps

train <- train[order(train$Patient_ID),]

first <- first[order(first$Patient_ID),]
second <- second[order(second$Patient_ID),]
third <- third[order(third$Patient_ID),]

first <- first[,-5]
first <- first[,-3]


train1 <- train
train1 <- train1[,-4:-8]

train1$Health_Camp <- rep(0,nrow(train1))
train1$Outcome <- rep(0,nrow(train1))

for (i in 1:nrow(train1)) {
  
  z1 <- subset(first, first$Patient_ID==train1$Patient_ID[i] & first$Health_Camp_ID==train1$Health_Camp_ID[i])
  if (nrow(z1)!= 0) {
    train1$Health_Camp[i] <- 1
    train1$Outcome[i] <- 1
  }
}

for (i in 1:nrow(train1)) {
z2 <- subset(second, second$Patient_ID==train1$Patient_ID[i] & second$Health_Camp_ID==train1$Health_Camp_ID[i])
if (nrow(z2)!=0) {
  train1$Health_Camp[i] <- 2
  train1$Outcome[i] <- 1
  }
}

for (i in 1:nrow(train1)) {
  z3 <- subset(third, third$Patient_ID==train1$Patient_ID[i] & third$Health_Camp_ID==train1$Health_Camp_ID[i])
  if (nrow(z3)!=0) {
    train1$Health_Camp[i] <- 3
    if (z3$Number_of_stall_visited > 0)
    {train1$Outcome[i] <- 1}
  }
}

table(train$Patient_ID==train1$Patient_ID & train$Health_Camp_ID == train1$Health_Camp_ID)

train$Health_Camp <- train1$Health_Camp
train$Outcome <- train1$Outcome

train2 <- train2[,-c(1,2,3,9)]

library(randomForest)
library(caret)
library(gbm)


train2$Outcome <- make.names(train2$Outcome)
train2$Outcome <- as.factor(train2$Outcome)

test <- test1[,-c(1:3)]

objControl <- trainControl(method='cv', 
                           number=50,
                           repeats = 5,
                           returnResamp='none',
                           allowParallel = TRUE,
                           summaryFunction = twoClassSummary, 
                           classProbs = T)

ctrl <- trainControl(method = "CV",
                     number=10,
                     classProbs = TRUE,
                     allowParallel = TRUE,
                     summaryFunction = twoClassSummary)
set.seed(100)



objModel <- train(Outcome ~ ., 
                  data = train2,
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale")
                  )



prediction <- predict(object=objModel, newdata = test, type = 'prob')
str(prediction)
prediction <- prediction[,2]
solution <- data.frame(Patient_ID = test1$Patient_ID, Health_Camp_ID=test1$Health_Camp_ID, Outcome = prediction)
write.csv(solution, "gbm.solution.csv")


rf.model <- randomForest(Outcome~., metric = "ROC", data = train2, importance = T, ntree = 500, preProc = c("center", "scale"))
prediction <- predict(rf.model, newdata = test, type = 'prob')
prediction <- prediction[,2]
solution <- data.frame(Patient_ID = test1$Patient_ID, Health_Camp_ID=test1$Health_Camp_ID, Outcome = prediction)
write.csv(solution, "rf.solution.csv")



library(xgboost)

trainx <- train2[,1:5]
trainx <- as.matrix(trainx)
label <- train$Outcome

testx <- as.matrix(test)

xgb.model <- xgboost(data = trainx, label = label,
                 nrounds = 30, objective = "binary:logistic")

test$Var1 <- as.numeric(test$Var1)
test$Var2 <- as.numeric(test$Var2)
test$Var3 <- as.numeric(test$Var3)
test$Var4 <- as.numeric(test$Var4)
test$Var5 <- as.numeric(test$Var5)

prediction <- predict(xgb.model, testx)
solution <- data.frame(Patient_ID = test1$Patient_ID, Health_Camp_ID=test1$Health_Camp_ID, Outcome = prediction)
write.csv(solution, "xgb.solution.csv")




cv.res <- xgb.cv(data = trainx, label = label, nfold = 10,
                 nrounds = 100, objective = "binary:logistic")

prediction <- predict(cv.res, testx)
solution <- data.frame(Patient_ID = test1$Patient_ID, Health_Camp_ID=test1$Health_Camp_ID, Outcome = prediction)
write.csv(solution, "cvxgb.solution.csv")

