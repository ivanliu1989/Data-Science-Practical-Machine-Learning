setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Practical-Machine-Learning")
library(kernlab)
library(caret)
data(spam)
inTrain <- createDataPartition(y=spam$type,p=.75,list=F)
training <- spam[inTrain,]
testing<- spam[-inTrain,]
dim(training)

set.seed(32343)
modelFit <- train(type~., data=training, method='glm')
modelFit
modelFit$finalModel

predictions <- predict(modelFit, newdata=testing)
predictions

confusionMatrix(predictions, testing$type)
