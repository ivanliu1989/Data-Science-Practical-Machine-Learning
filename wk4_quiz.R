setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Practical-Machine-Learning")
library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test) 
set.seed(33833)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
fit1 <- train(y~.,method='rf',data=vowel.train)
fit2 <- train(y~.,method='gbm',data=vowel.train)
pred1 <- predict(fit1,vowel.test)
pred2 <- predict(fit2,vowel.test)

confusionMatrix(pred1, vowel.test$y)
confusionMatrix(pred2, vowel.test$y)


# Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)
fit1 <- train(diagnosis~., method='rf', data=training)
fit2 <- train(diagnosis~., method='gbm', data=training)
fit3 <- train(diagnosis~., method='lda', data=training)
pred1 <- predict(fit1, testing)
pred2 <- predict(fit2, testing)
pred3 <- predict(fit3, testing)
confusionMatrix(pred1, testing[,1])
confusionMatrix(pred2, testing[,1])
confusionMatrix(pred3, testing[,1])
combineDF <- data.frame(pred1,pred2,pred3,diagnosis=testing[,1])
combineFit <- train(diagnosis~.,method='rf', data=combineDF)
combinePred <- predict(combineFit, testing)
confusionMatrix(combinePred, testing[,1])


# Q3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
fit <- train(CompressiveStrength~., method='lasso', data=training)
fit
?plot.enet
data(diabetes)
attach(diabetes)
object <- enet(x,y,lambda=1)
par(mfrow=c(2,2))
plot(object)
plot(object,xvar="step")
detach(diabetes)
