setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Practical-Machine-Learning")
library(kernlab)
library(caret)
data(spam)
inTrain <- createDataPartition(y=spam$type, p=.75, list=F)
training <- spam[inTrain,]
testing <-spam[-inTrain,]
par(mfcol=c(1,1))
hist(training$capitalAve,main='', xlab='ave, capital run length')
mean(training$capitalAve)
sd(training$capitalAve)
#standadizing
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)

testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve-mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)

# preProcess
preObj <- preProcess(training[,-58], method=c('center','scale'))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)
testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

set.seed(32323)
modelFit<- train(type~., data=training, preProcess=c('center','scale'),method='glm')
modelFit

# Box-Cox transforms
preObj <- preProcess(training[,-58], method=c('BoxCox'))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
png('preProcess.png')
par(mfrow=c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)
dev.off()

# Imputing data
set.seed(13343)
# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <- NA

# Impute and standardize
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj,training[,-58])$capAve

# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])
