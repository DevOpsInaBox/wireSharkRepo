######Smote Analysis############

print(table(wireshark$Target))
print(prop.table(table(wireshark$Target)))

print("##################A quick peek to see where we are:######################")
head(wireshark,2)

print("#######The data is riddled with characters need to conver to factor###########")
ind <- sapply(wireshark, is.factor)
wireshark[ind] <- lapply(wireshark[ind], as.character)
wireshark[ind] <- lapply(wireshark[ind], as.numeric)
repalceNAsWithMean <- function(x) {replace(x, is.na(x), mean(x[!is.na(x)]))}
wireshark <- repalceNAsWithMean(wireshark)
print(("######We randomly split the data into two equal portions#########"))
library(caret)
set.seed(1234)
splitIndex <- createDataPartition(wireshark$Target, p = .50,
                                  list = FALSE,
                                  times = 1)
trainSplit <- wireshark[ splitIndex,]
testSplit <- wireshark[-splitIndex,]

prop.table(table(trainSplit$Target))
prop.table(table(testSplit$Target))
print("#########Bagging Model###############3")
ctrl <- trainControl(method = "cv", number = 5)
tbmodel <- train(Target ~ ., data = trainSplit, method = "treebag",
                 trControl = ctrl)
predictors <- names(trainSplit)[names(trainSplit) != 'Target']
pred <- predict(tbmodel$finalModel, testSplit[,predictors])

print("##########AUC Score & Plot#############")
library(pROC)
auc <- roc(testSplit$Target, pred)
print(auc)
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)
print("######Smote Analysis############")
library(DMwR)
trainSplit$Target <- as.factor(trainSplit$Target)
trainSplit <- SMOTE(Target ~ ., trainSplit, perc.over = 100, perc.under=200)
trainSplit$Target <- as.numeric(trainSplit$Target)
prop.table(table(trainSplit$Target))

print("###########Bagging############")
tbmodel <- train(Target ~ ., data = trainSplit, method = "treebag",
                 trControl = ctrl)
predictors <- names(trainSplit)[names(trainSplit) != 'Target']
pred <- predict(tbmodel$finalModel, testSplit[,predictors])

print("##########AUC Score & Plot#############")
auc <- roc(testSplit$Target, pred)
print(auc)
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)


