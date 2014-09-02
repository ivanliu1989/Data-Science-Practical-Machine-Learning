setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Practical-Machine-Learning")
library(kernlab)
library(caret)
data(spam)
inTrain<-createDataPartition(y=spam$type, p=.75,list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

# k-folds
set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list=T, returnTrain=T)
sapply(folds, length)
folds[[1]][1:10]
folds <- createFolds(y=spam$type, k=10, list=T, returnTrain=F)
sapply(folds, length)
folds[[1]][1:10]

# resampling
set.seed(32323)
folds <- createResample(y=spam$type, times=10,list=T)
sapply(folds,length)
folds[[1]][1:10]

# Time Slices
tme <- 1:1000
folds <- createTimeSlices(y=tme,initialWindow=20,horizon=10)
names(folds)
folds$train[[1]]
folds$test[[1]]
