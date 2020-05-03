###########################################################################
## For CSE574 Project 2
## All models
###########################################################################

rm(list = ls())

setwd('C:/Users/Administrator/iCloudDrive/Documents/UBSpring2020/CSE574/Project/Project')

############ install the package ############
# install.packages('leaps')
# install.packages('lda')
# install.packages("ROSE")
# install.packages("DMwR")
# install.packages('class')

############ load the package ############
library(leaps)
library(MASS)
library(ROSE)
library(DMwR)
library(class)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(pROC)
library(rpart)
library(ggplot2)

###Load datasets
load('dataset/preprocessed_dataset.Rdata')
load('dataset/diet_have.Rdata')
load('dataset/diet_nothave.Rdata')
raw_data = dataset

############ Functions #############

#pre:prediction
#y:actual
f1_fun = function(pre, y) {
  class = sort(unique(y))
  tp = NA
  fp = NA
  fn = NA
  for (i in 1:length(class)) {
    tp[i] = sum(pre == class[i] & y == class[i])
    fp[i] = sum(pre == class[i] & y != class[i])
    fn[i] = sum(pre != class[i] & y == class[i])
  }
  f1 = 2 * tp / (2 * tp + fp + fn)
  names(f1) = class
  # print(table(pre,y))
  # print('-------------f1--------------------')
  # print(f1)
  # print('--------------mean(f1)-------------------')
  # print(mean(f1))
  return(f1[2])
}

#######Divide train and test

set.seed(123)

train_have <- sample(1:nrow(diet_have), .80 * nrow(diet_have))
train_nothave <-
  sample(1:nrow(diet_nothave), .80 * nrow(diet_nothave))

data_train <-
  rbind(diet_have[train_have,-c(2)], diet_nothave[train_nothave,-c(2)])
data_test <-
  rbind(diet_have[-train_have,-c(2)], diet_nothave[-train_nothave,-c(2)])

y_test <- as.numeric(data_test$diab)

########### Oversampling data ########################

data.rose <- ROSE(diab ~ ., data = data_train, seed = 1)$data
data_train$diab <- as.factor(data_train$diab)
data.smote <- SMOTE(diab ~ ., data = data_train,k = 5, perc.over = 500)

y_train <- as.numeric(data.rose$diab)


################################################################
########        Logistics Regression                  ##########
################################################################

########### eploy forward subset selection ##########
regfit.for <- regsubsets(diab~. - diab, data = data.rose, nbest = 1, nvmax = 70, method = "forward")
my_sum <- summary(regfit.for)
summary(regfit.for)

select = summary(regfit.for)$outmat

########### calculate error for each iteration
lr_train_err <- NULL # store training error
lr_test_err <- NULL  # store test error
lr_f1_train <- NULL # store training f1 score
lr_f1_test <- NULL # store testing f1 score
lr_AUC_train <- NULL
lr_AUC_test <- NULL
graphics.off()
for (i in 1:69) {
  temp <- which(select[i,] == "*")
  temp <- temp + 1
  
  red.training <- data.rose[, c(1, temp)]
  red.testing <- data_test[, c(1, temp)]
  
  red.fit <-
    glm(diab ~ . , data = red.training, family = "binomial")
  
  pred.train = round(predict(red.fit, newdata = red.training, type = "response"))
  pred.test = round(predict(red.fit, newdata = red.testing, type = "response"))
  
  lr_train_err[i] <-  sum(abs(pred.train - y_train)) / length(y_train)
  lr_test_err[i]  <-  sum(abs(pred.test - y_test)) / length(y_test)
  lr_f1_train[i] <- f1_fun(pred.train, y_train)
  lr_f1_test[i] <- f1_fun(pred.test, y_test)
  lr_AUC_train[i] <- roc.curve(data.rose$diab, pred.train)$auc
  lr_AUC_test[i] <- roc.curve(data_test$diab, pred.test)$auc
  
}

############# plot out ##################
graphics.off()
par(mfrow=c(1,2))
plot(lr_train_err, type="l", 
     main ="LR Training error - K predictors(Forward)", 
     ylab ="Mean Absolute Error",
     xlab = "Number of Predictors",
     xlim = c(0, 69))
plot(lr_test_err, type="l",
     main ="LR Test error - K predictors(Forward)", 
     ylab ="Mean Absolute Error",
     xlab = "Number of Predictors",
     xlim = c(0, 69))

graphics.off()
par(mfrow=c(1,2))
plot(lr_f1_train, type="l", 
     main ="LR Training F1 score - K predictors(Forward)", 
     ylab ="F1-score",
     xlab = "Number of Predictors",
     xlim = c(0, 69))
plot(lr_f1_test, type="l",
     main ="LR Test F1 score- K predictors(Forward)", 
     ylab ="F1-score",
     xlab = "Number of Predictors",
     xlim = c(0, 69))

graphics.off()
par(mfrow=c(1,2))
plot(lr_AUC_train, type="l", 
     main ="LR Trianing AUC - K predictors(Forward)", 
     ylab ="F1-score",
     xlab = "Number of Predictors",
     xlim = c(0, 69))
plot(lr_AUC_test, type="l", 
     main ="LR Testing AUC - K predictors(Forward)", 
     ylab ="F1-score",
     xlab = "Number of Predictors",
     xlim = c(0, 69))

which(lr_train_err==min(lr_train_err)) 
which(lr_test_err==min(lr_test_err))
which(lr_f1_train==max(lr_f1_train)) 
max(lr_f1_test)
max(lr_AUC_test)

################################################################
########      Linear Discriminant Analysis (LDA)      ##########
################################################################

lda_train_err <- NULL # store training error
lda_test_err <- NULL  # store test error
lda_f1_train <- NULL # store training f1 score
lda_f1_test <- NULL # store testing f1 score
lda_AUC_train <- NULL
lda_AUC_test <- NULL

for (i in 1:69) {
  temp <- which(select[i,] == "*")
  temp <- temp + 1
  
  red.training <- data.rose[, c(1, temp)]
  red.testing <- data_test[, c(1, temp)]
  
  red.fit <- lda(diab ~., data = red.training)
  
  pred.train = predict(red.fit, newdata = red.training, type = "response")
  pred.test = predict(red.fit, newdata = red.testing, type = "response")
  
  y_hat_train <- as.numeric(pred.train$class) -1
  y_hat_test <- as.numeric(pred.test$class) -1
  
  lda_train_err[i] <-  sum(abs(y_hat_train - y_train)) / length(y_train)
  lda_test_err[i]  <-  sum(abs(y_hat_test - y_test)) / length(y_test)
  
  lda_f1_train[i] <- f1_fun(y_hat_train, y_train)
  lda_f1_test[i] <- f1_fun(y_hat_test, y_test)
  lda_AUC_train[i] <- roc.curve(data.rose$diab, y_hat_train)$auc
  lda_AUC_test[i] <- roc.curve(data_test$diab, y_hat_test)$auc
}

############# plot out ##################
graphics.off()
par(mfrow=c(1,2))
plot(lda_train_err, type="l", 
     main ="LDA Training error - K predictors(Forward)", 
     ylab ="Mean Absolute Error",
     xlab = "Number of Predictors",
     xlim = c(0, 69))
plot(lda_test_err, type="l",
     main ="LDA Test error - K predictors(Forward)", 
     ylab ="Mean Absolute Error",
     xlab = "Number of Predictors",
     xlim = c(0, 69))

graphics.off()
par(mfrow=c(1,2))
plot(lda_f1_train, type="l", 
     main ="LDA training F1 score - K predictors(Forward)", 
     ylab ="F1-score",
     xlab = "Number of Predictors",
     xlim = c(0, 69))
plot(lda_f1_test, type="l",
     main ="LDA Test F1 score- K predictors(Forward)", 
     ylab ="F1-score",
     xlab = "Number of Predictors",
     xlim = c(0, 69))

graphics.off()
par(mfrow=c(1,2))
plot(lda_AUC_train, type="l", 
     main ="LDA Training AUC - K predictors(Forward)", 
     ylab ="F1-score",
     xlab = "Number of Predictors",
     xlim = c(0, 69))
plot(lda_AUC_test, type="l", 
     main ="LDA Testing AUC - K predictors(Forward)", 
     ylab ="F1-score",
     xlab = "Number of Predictors",
     xlim = c(0, 69))
which(lda_f1_test==max(lda_f1_test))
max(lda_f1_test)
max(lda_AUC_test)

################################################################
########        kNN                                   ##########
################################################################
knn.train <- data.rose
train.label <- knn.train$diab
errors_knn <- c()
knn_f1 <- NULL
knn_AUC <- NULL
test.label <- data_test$diab


for (i in c(1:20)){
  prediction.knn <- knn(knn.train, data_test, train.label, k = i)
  errors_knn <- c(errors_knn, 1-length(which(test.label == prediction.knn)) / length(test.label))
  knn_AUC[i] <- roc.curve(data_test$diab, as.numeric(prediction.knn) - 1)$auc
  knn_f1[i] <- f1_fun(as.numeric(prediction.knn) - 1, data_test$diab)
}

k = 1:20
plot(k,errors_knn,type = "l",main="knn errors--number of K",xlab="k values",ylab='k-nn error')
plot(k,knn_AUC,type = "l",main="knn AUC--number of K",xlab="k values",ylab='k-nn error', ylim = c(0.45, 0.6))
plot(k,knn_f1,type = "l",main="knn F1 score--number of K",xlab="k values",ylab='k-nn error')


###########################################
#######           Xgboost          ########
###########################################


b4_rose <- as.numeric(table(data_train$diab))
after_rose <- as.numeric(table(data.rose$diab))

labels <- c('Do not hava', 'Have')

graphics.off()
par(mfrow = c(1,2))
pie(b4_rose,labels, main = 'Before oversampling with ROSE')
pie(after_rose,labels, main = 'After oversampling with ROSE')


dt = data.frame(table(data_train$diab))


model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
fit <- rpart(diab~., data=data.rose, method = "class", control = model.control)
# summary(fit)
predictions <- predict(fit, data_test, type="class")

f1_fun(predictions, data_test$diab)

accuracy.meas(data_test$diab, predictions)
roc.curve(data_test$diab, predictions)
ROSE_roc <- roc(as.numeric(data_test$diab), as.numeric(predictions))

graphics.off()
plot(ROSE_roc, print.auc=TRUE, auc.polygon=TRUE, 
     grid=c(0.1, 0.2),grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,auc.polygon.col="skyblue", 
     print.thres=TRUE,main='ROC curve with ROSE')


###########################################
#######           Xgboost          ########
###########################################

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

f1_fun(y_test, pre_xgb)

xgboost_roc <- roc(y_test, pre_xgb)
plot(xgboost_roc, print.auc=TRUE, auc.polygon=TRUE, 
     grid=c(0.1, 0.2),grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,auc.polygon.col="skyblue", 
     print.thres=TRUE,main='ROC curve')

