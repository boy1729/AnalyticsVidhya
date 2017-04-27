setwd("D:/kaggle/AnalyticsVaidya/Experiments_with_data")
train <- read.csv("finaltrain.csv")
test <- read.csv("finaltest.csv")

train <- train[,-1]
test <- test[,-1]

levels(test$Native.Country) <- levels(train$Native.Country)

objControl <- trainControl(method='cv', number=15, repeats = 13, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
objModel <- train(df.train[,predictorNames], df.train[,outcomeName],
method='gbm',
trControl=objControl,
metric = "ROC",
preProc = c("center", "scale"))

train.pred <- predict(object = objModel, df.train[,predictorNames], type = 'raw')
predictions <- predict(object = objModel, df.test[,predictorNames], type = 'raw')
test.pred <- as.data.frame(predictions)
summary(test.pred)
str(train$Income.Group)
test.pred <- ifelse(test.pred$predictions == "rich", ">50K", "<=50K")
write.csv(test.pred, "gbm1.csv")


model <- xgboost(data = train.matrix, label = outcome.matrix,
nrounds = 49, maximize = T, objective = "binary:logistic")
preds <- predict(model, test.matrix)
xgb.pred <- ifelse(preds > 0.5, ">50K", "<=50K")
write.csv(xgb.pred, "xgb.csv")

