###########################################################################
## For CSE574 Project 2
## Fitting Models--
###########################################################################

rm(list = ls())

setwd(
  'C:/Users/Administrator/iCloudDrive/Documents/UBSpring2020/CSE574/Project/Project'
)


############ install the package ############
# install.packages('leaps')
# install.packages('lda')
# install.packages("ROSE")
# install.packages("DMwR")

############ load the package ############
library(leaps)
library(MASS)
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


################################################################
########        Logistics Regression                  ##########
################################################################

########### Oversampling data ########################
data.rose <- ROSE(diab ~ ., data = data_train, seed = 1)$data
data_train$diab <- as.factor(data_train$diab)
data.smote <- SMOTE(diab ~ ., data = data_train,k = 5, perc.over = 500)

y_train <- as.numeric(data.rose$diab) - 1


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
     main ="LR Training err - K predictors(Forward)", 
     ylab ="F1-score",
     xlab = "Number of Predictors",
     xlim = c(0, 69))
plot(lr_f1_test, type="l",
     main ="LR Test err- K predictors(Forward)", 
     ylab ="F1-score",
     xlab = "Number of Predictors",
     xlim = c(0, 69))

graphics.off()
par(mfrow=c(1,2))
plot(lr_AUC_train, type="l", 
     main ="LR AUC - K predictors(Forward)", 
     ylab ="F1-score",
     xlab = "Number of Predictors",
     xlim = c(0, 69))
plot(lr_AUC_test, type="l", 
     main ="LR AUC - K predictors(Forward)", 
     ylab ="F1-score",
     xlab = "Number of Predictors",
     xlim = c(0, 69))

which(lr_train_err==min(lr_train_err)) 
which(lr_test_err==min(lr_test_err))
which(lr_f1_train==max(lr_f1_train)) 
which(lr_f1_test==max(lr_f1_test))

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
     main ="LDA AUC - K predictors(Forward)", 
     ylab ="F1-score",
     xlab = "Number of Predictors",
     xlim = c(0, 69))
plot(lda_AUC_test, type="l", 
     main ="LDA AUC - K predictors(Forward)", 
     ylab ="F1-score",
     xlab = "Number of Predictors",
     xlim = c(0, 69))

