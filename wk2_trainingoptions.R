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
