library(kernlab);data(spam)
spam$capitalAveSq <- spam$capitalAve^2

library(ISLR); library(caret); data(Wage);
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass,data=training)
head(predict(dummies,newdata=training))

nsv <- nearZeroVar(training,saveMetrics=TRUE)
nsv

library(splines)
bsBasis <- bs(training$age,df=3) 
bsBasis
par(mfcol=c(1,1))
lm1 <- lm(wage ~ bsBasis,data=training)
png('splines.png')
plot(training$age,training$wage,pch=19,cex=0.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)
dev.off()
predict(bsBasis, age=testing$age)
