###########################################################################
## For CSE574 Project 2
## CART model and oversampling
###########################################################################

rm(list = ls())

setwd(
  'C:/Users/Administrator/iCloudDrive/Documents/UBSpring2020/CSE574/Project/Project'
)

###install and loard packages
# install.packages("ROSE")
# install.packages("DMwR")
library(rpart)
library(ROSE)
library(DMwR)

###Load datasets
load('dataset/preprocessed_dataset.Rdata')
load('dataset/diet_have.Rdata')
load('dataset/diet_nothave.Rdata')
raw_data = dataset

############ Functions #############

#pre:prediction
#y:actual
f1_fun = function(pre,y){
  class = sort(unique(y))
  tp=NA
  fp=NA
  fn=NA
  for(i in 1:length(class)){
    tp[i] = sum(pre==class[i] & y==class[i])
    fp[i] = sum(pre==class[i] & y!=class[i])
    fn[i] = sum(pre!=class[i] & y==class[i])
  }
  f1 = 2*tp/(2*tp+fp+fn)
  names(f1) = class
  print(table(pre,y))
  print('-------------f1--------------------')
  print(f1)
  print('--------------mean(f1)-------------------')
  print(mean(f1))
  return(f1)
}

#######Divide train and test
set.seed(123)

train_have <- sample(1:nrow(diet_have), .80*nrow(diet_have))
train_nothave <- sample(1:nrow(diet_nothave), .80*nrow(diet_nothave))

data_train <- rbind(diet_have[train_have, -c(2)], diet_nothave[train_nothave, -c(2)])
data_test <- rbind(diet_have[-train_have, -c(2)], diet_nothave[-train_nothave, -c(2)])

###########################################
#######ROSE resampling             ########
###########################################

table(data_train$diab)
data.rose <- ROSE(diab ~ ., data = data_train, seed = 1)$data

D
model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
fit <- rpart(diab~., data=data.rose, method = "class", control = model.control)
# summary(fit)
predictions <- predict(fit, data_test, type="class")
table(predictions, data_test$diab)

f1_fun(predictions, data_test$diab)

accuracy.meas(data_test$diab, predictions)
roc.curve(data_test$diab, predictions)

###########################################
######      SMOTE resampling       ########
###########################################
table(raw_data$diab)
data_train$diab <- as.factor(data_train$diab)

data.smote <- SMOTE(diab ~ ., data = data_train,k = 5, perc.over = 500)
table(data.smote$diab)

model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
fit <- rpart(diab~., data=data.smote, method = "class", control = model.control)
# summary(fit)
predictions <- predict(fit, data_test, type="class")
table(predictions, data_test$diab)

f1_fun(predictions, data_test$diab)

accuracy.meas(data_test$diab, predictions)
roc.curve(data_test$diab, predictions)

