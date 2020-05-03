###################################################################################
#################### logistic regression
###################################################################################

rm(list = ls())

# setwd('C:/Users/Administrator/Google Drive/EAS 506 Project')
setwd('/Users/lvpin/Google Drive/EAS 506 Project')
load('dataset/preprocessed_dataset.Rdata')
load('dataset/diet_have.Rdata')
load('dataset/diet_nothave.Rdata')
load('dataset/diet.Rdata')


############ install the package ############ 
# install.packages('leaps')
# install.packages('lda')

############ load the package ############ 
library(leaps)
library(MASS)

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


######Split the data into training and test

set.seed(1001)
train <- sample(1:nrow(dataset), .80*nrow(dataset))
data_train <- dataset[train, -c(2)]
data_test <- dataset[-train, -c(2)]
diet_have <- diet_have[, -c(2)]
diet_nothave <- diet_nothave[, -c(2)]

y_train <- as.numeric(data_train$diab) - 1
y_test <- as.numeric(data_test$diab) - 1


######################################
###### forward subset selection ######
######################################

regfit.for <- regsubsets(diab~. - diab, data = data_train, nbest = 1, nvmax = 70, method = "forward")
my_sum <- summary(regfit.for)
summary(regfit.for)

select = summary(regfit.for)$outmat
lr_train_err <- NULL # store training error
lr_test_err <- NULL  # store test error
lr_f1_train <- NULL
lr_f1_test <- NULL 
for (i in 1:69){
  temp <- which(select[i,] == "*")
  temp <- temp + 1
  
  red.training <- data_train[, c(1,temp)]
  red.testing <- data_test[, c(1,temp)]
  
  red.fit <- glm(diab ~. , data = red.training, family = "binomial")
  
  pred.train = round(predict(red.fit, newdata = red.training, type = "response"))
  pred.test = round(predict(red.fit, newdata = red.testing, type = "response"))
  
  lr_train_err[i] <- sum(abs(pred.train - y_train)) / length(y_train)
  lr_test_err[i]  <-  sum(abs(pred.test - y_test)) / length(y_test)
  lr_f1_train[i] <- f1_fun(pred.train, y_train)
  lr_f1_test[i] <- f1_fun(pred.test, y_test)
  
}



f1 <- f1_fun(pred.train, y_train)

graphics.off()
par(mfrow=c(1,2))
plot(lr_train_err, type="l", 
     main ="LOG R err Training - K predictors(Forward)", 
     ylab ="Mean Absolute Error",
     xlab = "Number of Predictors",
     ylim = c(0,0.10),
     xlim = c(0, 69))
plot(lr_test_err, type="l",
     main ="LOG R err Test - K predictors(Forward)", 
     ylab ="Mean Absolute Error",
     xlab = "Number of Predictors",
     ylim = c(0, 0.10),
     xlim = c(0, 69))

graphics.off()
par(mfrow=c(1,2))
plot(lr_f1_train, type="l", 
     main ="LOG R err Training - K predictors(Forward)", 
     ylab ="Mean F1",
     xlab = "Number of Predictors",
     ylim = c(0,0.1),
     xlim = c(0, 69))
plot(lr_f1_test, type="l",
     main ="LOG R err Test - K predictors(Forward)", 
     ylab ="Mean F1",
     xlab = "Number of Predictors",
     ylim = c(0, 0.1),
     xlim = c(0, 69))

which(lr_train_err==min(lr_train_err)) 
which(lr_test_err==min(lr_test_err)) # the only one here is DR1TCAFF(caffeine) which is interesting

i = 47 ### select only 47 variables 


lr_train_err[i] * 100

########  Training error rate is 5.85%

lr_test_err[1] * 100

########  Testing error rate is 6.03%

which(select[i,] == "*")
temp <- which(select[i,] == "*")
temp <- temp + 1 

red.training <- data_train[, c(1,temp)]
diet_have_test <- diet_have[,c(1,temp)]
diet_nothave_test <- diet_nothave[,c(1,temp)]


lr.fit1 <- glm(diab ~., data = red.training, family = "binomial")
summary(lr.fit1)

have_test = round(predict(lr.fit1, newdata = diet_have_test, type = "response"))
have_err <- sum(abs(have_test - diet_have_test[,c(1)] )) / length(diet_have_test[,c(1)])
have_err* 100

#### input the candidate with diabetes into the model, the error rate is 98% !!!!

lr.fit2 <- glm(diab ~., data = red.training, family = "binomial")
nothave_test = round(predict(lr.fit2, newdata = diet_nothave_test, type = "response"))
nothave_err <- sum(abs(nothave_test - diet_nothave_test[,c(1)] )) / length(diet_nothave_test[,c(1)])
nothave_err * 100

#### input the candidates without diabetes into the model, the error rate is 16.17%


##################################################
## Linear Discriminant Analysis (LDA)
##################################################

lda_train_err<-NULL 
lda_test_err<-NULL  


for (i in 1:69){
  temp <- which(select[i,] == "*")
  temp <- temp + 1
  
  red.training <- data_train[, c(1,temp)] 
  red.testing <- data_test[,c(1,temp)]
  
  red.fit <- lda(diab ~., data = red.training)
  
  pred.train = predict(red.fit, newdata = red.training, type = "response")
  pred.test = predict(red.fit, newdata = red.testing, type = "response")
  
  y_hat_train <- as.numeric(pred.train$class) -1
  y_hat_test <- as.numeric(pred.test$class) -1
  
  lda_train_err[i] <- sum(abs(y_hat_train - y_train)) / length(y_train)
  lda_test_err[i]  <- sum(abs(y_hat_test - y_test)) / length(y_test)
}

graphics.off()
par(mfrow=c(1,2))
plot(lda_train_err, type="l", 
     main ="LDA err Training - K predictors", 
     ylab ="Mean Absolute Error",
     xlab = "No. of Predictors",
     ylim = c(0,0.10))
plot(lda_test_err, type="l",
     main ="LDA err Test - K predictors", 
     ylab ="Mean Absolute Error",
     xlab = "No. of Predictors",
     ylim = c(0,0.10))

which(lda_train_err == min(lda_train_err)) # 1
which(lda_test_err == min(lda_test_err))   # 1 2 3 

# Chose model of 1 predictors, which is DR1TCAFF(caffeine), the same as logistic regression subset selection, very interesting 

which(select[1,] == "*")
i = 1
temp <- which(select[i,] == "*")
temp <- temp + 1 

red.training <- data_train[, c(1,temp)] 
red.testing <- data_test[,c(1,temp)]

lda.fit <- lda(diab ~., data = red.training)
lda.fit #### mean of group DR1TCAFF(caffeine)
lda.fit$scaling

pred.train = predict(lda.fit, newdata = red.training, type = "response")
pred.test = predict(lda.fit, newdata = red.testing, type = "response")

lda_train_err[i]
lda_test_err[i]

red.training <- data_train[, c(1,temp)]
diet_have_test <- diet_have[,c(1,temp)]
diet_nothave_test <- diet_nothave[,c(1,temp)]


lda.fit1 <- lda(diab ~., data = red.training)
summary(lda.fit1)

have_test = predict(lda.fit1, newdata = diet_have_test, type = "response")
have_test_sign <- as.numeric(have_test$class) -1
have_err <- sum(abs(have_test_sign - diet_have_test[,c(1)] )) / length(diet_have_test[,c(1)])
have_err * 100

#### input the candidate with diabetes into the model, the error rate is 99.79% !!!!

lda.fit2 <- lda(diab ~., data = red.training)
nothave_test = predict(lda.fit2, newdata = diet_nothave_test, type = "response")
nothave_test_sign <- as.numeric(nothave_test$class) -1
nothave_err <- sum(abs(nothave_test_sign - diet_nothave_test[,c(1)] )) / length(diet_nothave_test[,c(1)])
nothave_err * 100
