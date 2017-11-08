library(data.table)
library(psych)
library(mvtnorm)
library(ggplot2)
library(pROC)
library(h2o)
library(lattice)
library(caret)
library(PRROC)
library(randomForest)

print("**********************Loading Wireshark Data**************************")
wireshark<-read.csv("wireshar_1.csv",header = T)
print("************************Response Variable to Factor form**********************")
wireshark$Source<-as.numeric(wireshark$Source)
wireshark$Destination<-as.numeric(wireshark$Destination)
wireshark$Protocol<-as.numeric(wireshark$Protocol)
wireshark$Length<-as.numeric(wireshark$Length)
wireshark$Info<-as.numeric(wireshark$Info)
# We don't need the ID field
wireshark <- wireshark[,-1]
print("********************************************Summary of Wireshark Data*****************************************************")
summary(wireshark)
set.seed(1234)
random_splits <- runif(nrow(wireshark))
print("****************************The original dataset is split into Training set and Testing/Validating set*********************************")
train_df <- wireshark[random_splits < .5,]
print("******************************************Rows and Columns in the training dataset***************************************************")
dim(train_df)
print("*******************************************Rows and Columns in the testing dataset***************************************************")
validate_df <- wireshark[random_splits >=.5,]
dim(validate_df)
str(wireshark)

# Get benchmark score

outcome_name <- 'Target'
feature_names <- setdiff(names(wireshark), outcome_name)
set.seed(1234)
print("**************************Random Forest Prediction Model is built with Capsule column as the output/predictor************************")
rf_model <- randomForest(x=train_df[,feature_names],
                         y=as.factor(train_df[,outcome_name]),
                         importance=TRUE, ntree=20, mtry = 3)
validate_predictions <- predict(rf_model, newdata=validate_df[,feature_names], type="prob")

auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=validate_predictions[,2])
print("****************************Receiver Operating Curve(ROC) is plotted with the model for checking Model Fit**************************")
plot(auc_rf, print.thres = "best", main=paste('AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')

print(paste0("******************************Area Under Curve is found to be :",round(auc_rf$auc[[1]],3),"********************************"))
# build autoencoder model
print("*******************Since AUC is not close to 1, H2O Deep Learning with Autoencoder is used to detect Anomalies**********************")
library(h2o)
localH2O = h2o.init()
wireshark.hex<-as.h2o(train_df, destination_frame="train.hex")
colnames(train_df) <- iconv(colnames(train_df), to='ASCII', sub='')
head(wireshark.hex)
wireshark.di = h2o.deeplearning(x = feature_names, training_frame = wireshark.hex,
                               autoencoder = TRUE,
                               reproducible = T,
                               activation = "Tanh",
                               seed = 1234,
                               hidden = c(6,5,6), epochs = 50)


wireshark.anon = h2o.anomaly(wireshark.di, wireshark.hex, per_feature=FALSE)
print("**************************************First few rows of Error values from H2O Deep Learning ******************************************")
head(wireshark.anon)
err <- as.data.frame(wireshark.anon)
head(err)
plot(sort(err$Reconstruction.MSE))

row_outliers <- which(err > 0.05)


print("********************Get the Anomaly Detected Rows*********************")
wireshark[row_outliers,]
anom<-wireshark[row_outliers,]
anom
print("*************Number of rows detected with Anomaly*************")
nrow(anom)
print(paste0("******Number of Rows detected as Anomaly:",nrow(anom),"**********"))
train_df_auto <- train_df[err$Reconstruction.MSE < 0.05,]
set.seed(1234)
rf_model <- randomForest(x=train_df_auto[,feature_names],
                         y=as.factor(train_df_auto[,outcome_name]),
                         importance=TRUE, ntree=20, mtry = 3)




validate_predictions_known <- predict(rf_model, newdata=validate_df[,feature_names], type="prob")

auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=validate_predictions_known[,2])
print(paste0("******************ROC curve is plotted based on new predictions and AUC is found to be ",round(auc_rf$auc[[1]],3),"**********************"))
plot(auc_rf, print.thres = "best", main=paste('AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')

train_df_auto <- train_df[err$Reconstruction.MSE < 0.04,]
set.seed(1234)
rf_model <- randomForest(x=train_df_auto[,feature_names],
                         y=as.factor(train_df_auto[,outcome_name]),
                         importance=TRUE, ntree=20, mtry = 3)

validate_predictions_unknown <- predict(rf_model, newdata=validate_df[,feature_names], type="prob")
auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=validate_predictions_unknown[,2])
print(paste0("******************ROC curve is plotted based on new predictions and AUC is found to be ",round(auc_rf$auc[[1]],3),"**********************"))
plot(auc_rf, print.thres = "best", main=paste('AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')

valid_all <- (validate_predictions_known[,2] + validate_predictions_unknown[,2]) / 2
auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=valid_all)
print(paste0("******************ROC curve is plotted using predictions from Bagging and AUC is found to be ",round(auc_rf$auc[[1]],3),"**********************"))
plot(auc_rf, print.thres = "best", main=paste('AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')
prostate.anon = h2o.anomaly(wireshark.di, wireshark.hex, per_feature=TRUE)


















