#########################################################
## knn for total
#########################################################

#train_data = rep(0,length(dataset$diab))
#train_data
#train_data[dataset$diab > median(dataset$diab)] = 1
#dataset$diab

train = sample(1:nrow(dataset), nrow(dataset)*0.80)
train
test = -train
dataset_train = dataset[train,]
dataset_test = dataset[test,]
label_train = dataset_train$diab
label_train
label_test = dataset_test$diab

knn.train = dataset_train
knn.test = dataset_test
train.label = label_train
test.label = label_test
errors_knn <- c()
for (i in c(1:20)){
  prediction.knn <- knn(knn.train,knn.test,train.label,k = i)
  errors_knn<-c(errors_knn, 1-length(which(test.label == prediction.knn)) / length(test.label))
}
errors_knn

jpeg(file = "k-nn.jpg")
# main=\"Cars\"
k = 1:20
plot(k,errors_knn,type = "b",main="knn errors",xlab="k values",ylab='k-nn error')
dev.off()

min(errors_knn)







#########################################################
## knn for have disease
#########################################################
#train_data_diet_have = rep(0,length(diet_have$diab))
#train_data_diet_have[diet_have$diab > median(diet_have$diab)] = 1

train_diet_have = sample(1:nrow(diet_have), nrow(diet_have)*0.80)
#train
test_diet_have = -train_diet_have
#dataset_train_diet_have = diet_have[train_diet_have,]
dataset_test_diet_have = diet_have[test_diet_have,]
#label_train_diet_have = dataset_train_diet_have$diab
#label_train
label_test_diet_have = dataset_test_diet_have$diab

#knn.train_diet_have = dataset_train_diet_have
knn.test_diet_have = dataset_test_diet_have
#train_diet_have.label = label_train_diet_have
test_diet_have.label = label_test_diet_have
errors_knn_diet_have <- c()
for (j in c(1:20)){
  prediction_diet_have.knn <- knn(knn.train, knn.test_diet_have, train.label, k = j)
  errors_knn_diet_have<-c(errors_knn_diet_have, 1-length(which(test_diet_have.label == prediction_diet_have.knn)) / length(test_diet_have.label))
}
errors_knn_diet_have

jpeg(file = "k-nn (have disease).jpg")
# main=\"Cars\"
k = 1:20
plot(k,errors_knn_diet_have, type = "b",main="k-nn error for people who have disease",xlab="k values",ylab='k-nn error')
dev.off()

min(errors_knn_diet_have)