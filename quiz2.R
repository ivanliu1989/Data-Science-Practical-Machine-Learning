library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=F)
head(testIndex)

data(concrete)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
head(training)
require(Hmisc)
plot(training$CompressiveStrength,pch=1,col=cut2(training$FlyAsh,m=20))

plot(training$CompressiveStrength,pch=1,col=cut2(training$Age,m=20))

plot(training$CompressiveStrength,pch=1,col=cut2(training$Cement,m=20))





library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
concrete$Superplasticizer
par(mfcol=c(1,2))
hist(log(concrete$Superplasticizer))
hist(concrete$Superplasticizer)





library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
str(training[,58:69])
pca <- preProcess(training[,58:69], method='pca')
pca.p <- predict(pca,training[,58:69])
plot(pca.p)
prcomp(training[,58:69])
pca$rotation
plot(prcomp(training[,58:69]))
# svm
svd1 <- svd(scale(training[,58:69]))
plot(svd1$d^2/sum(svd1$d^2))
