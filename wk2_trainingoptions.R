setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Practical-Machine-Learning")
library(kernlab)
library(caret)
data(spam)
inTrain<-createDataPartition(y=spam$type, p=.75,list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
modelFit <- train(type~., data=training,method='glm')

args(train.default)
args(trainControl)

set.seed(1235)
modelFit2 <- train(type~., data=training, method='glm')
modelFit2
