setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Practical-Machine-Learning")
library(ggplot2)
library(caret)
library(ISLR)
data(Wage)
summary(Wage)

inTrain<-createDataPartition(y=Wage$wage, p=.7,list=F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training);dim(testing)

png('feature.png')
featurePlot(x=training[,c('age','education','jobclass')],
            y=training$wage, 
            plot='pairs')
dev.off()
png('feature2.png')
qq <- qplot(age,wage,data=training, colour=jobclass)
dev.off()

png('feature3.png')
qq <- qplot(age,wage,data=training, colour=education)
qq+geom_smooth(method='lm',formula=y~x)
dev.off()
##
library(Hmisc)
cutWage <- cut2(training$wage, g=3)
table(cutWage)
png('feature4.png')
p1<-qplot(cutWage,age,data=training, fill=cutWage,geom=c('boxplot'))
p1
dev.off()
##
png('feature5.png')
p2<-qplot(cutWage,age,data=training, fill=cutWage,geom=c('boxplot','jitter'))
p2
dev.off()
##
t1 <- table(cutWage, training$jobclass)
t1
prop.table(t1,1)

png('feature6.png')
qplot(wage,colour=education,data=training, geom='density')
dev.off()