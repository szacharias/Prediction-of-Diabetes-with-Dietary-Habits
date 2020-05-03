###########################################################################
## For CSE574 Project 2
## XGBoost
###########################################################################

rm(list = ls())

setwd(
  'C:/Users/Administrator/iCloudDrive/Documents/UBSpring2020/CSE574/Project/Project'
)

###install and loard packages
# install.packages('xgboost')
# install.packages('readr')
# install.packages('stringr')
# install.packages('caret')
# install.packages('car')
# install.packages("ROSE")

library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(ROSE)
library(pROC)
###Load datasets
load('dataset/preprocessed_dataset.Rdata')
load('dataset/diet_have.Rdata')
load('dataset/diet_nothave.Rdata')
raw_data = dataset

#######Divide train and test
set.seed(123)

train_have <- sample(1:nrow(diet_have), .80*nrow(diet_have))
train_nothave <- sample(1:nrow(diet_nothave), .80*nrow(diet_nothave))

data_train <- rbind(diet_have[train_have, -c(2)], diet_nothave[train_nothave, -c(2)])
data_test <- rbind(diet_have[-train_have, -c(2)], diet_nothave[-train_nothave, -c(2)])

y_test <- as.numeric(data_test$diab)


####### Over sampling
data.rose <- ROSE(diab ~ ., data = data_train, seed = 1)$data
m_data <- data.matrix(data.rose[, -c(1)], rownames.force = NA)
y_train <- as.numeric(data.rose$diab)

xgb <- xgboost(data = m_data,label = y_train, max_depth=6, eta=0.5,  
        objective='binary:logistic', nround=25)

importance <- xgb.importance(model = xgb)
head(importance)
graphics.off()
xgb.ggplot.importance(importance)
m_test <- data.matrix(data_test[, -c(1)])

pre_xgb = round(predict(xgb, m_test))
table(y_test, pre_xgb)

xgboost_roc <- roc(y_test, pre_xgb)
plot(xgboost_roc, print.auc=TRUE, auc.polygon=TRUE, 
     grid=c(0.1, 0.2),grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,auc.polygon.col="skyblue", 
     print.thres=TRUE,main='ROC curve')
