setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Practical-Machine-Learning")
library(ISLR); library(ggplot2); library(caret);
data(Wage); Wage <- subset(Wage,select=-c(logwage))
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)

featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")

qplot(age,wage,data=training)
qplot(age,wage,colour=jobclass,data=training)
qplot(age,wage,colour=education,data=training)

modFit<- train(wage ~ age + jobclass + education,
               method = "lm",data=training)
finMod <- modFit$finalModel
print(modFit)

png('lr2_residual.png')
plot(finMod,1,pch=19,cex=0.5,col="#00000010")
dev.off()
png('lr2_residual2.png')
qplot(finMod$fitted,finMod$residuals,colour=race,data=training)
dev.off()
png('lr2_residual3.png')
plot(finMod$residuals,pch=19)
dev.off()
png('lr2_pred.png')
pred <- predict(modFit, testing)
qplot(wage,pred,colour=year,data=testing)
dev.off()

modFitAll<- train(wage ~ .,data=training,method="lm")
pred <- predict(modFitAll, testing)
qplot(wage,pred,data=testing)