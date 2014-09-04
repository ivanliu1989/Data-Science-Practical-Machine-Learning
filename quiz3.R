setwd('C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Practical-Machine-Learning')
# Q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
head(segmentationOriginal)
inTrain <- createDataPartition(y=segmentationOriginal$Case, p=.7, list=F)
train <- segmentationOriginal[which(segmentationOriginal$Case=='Train'),]
test <- segmentationOriginal[which(segmentationOriginal$Case=='Test'),]
set.seed(125)
fit <- train(Class~., method='rpart', data=segmentationOriginal)
print(fit$finalModel)
library(rattle)
fancyRpartPlot(fit$finalModel)
test$predict <- predict(fit, test)
head(test)
test$predict

# Q3
load('olive.rda')
head(olive)
olive = olive[,-1]
newdata = as.data.frame(t(colMeans(olive)))
fit <- train(Area~. , method='rf', data=olive)
predict(fit$finalModel, newdata)

# Q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
str(trainSA)
fit <- train(chd~age+alcohol+obesity+tobacco+typea+ldl, method='glm', family='binomial', data=trainSA)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
pred <- predict(fit, testSA)
missClass(testSA$chd, pred)
predT <- predict(fit, trainSA)
missClass(trainSA$chd, predT)

# Q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
head(vowel.train)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
fit <- train(y~., method='rf', data=vowel.train)
varImp(fit)
