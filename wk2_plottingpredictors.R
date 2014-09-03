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
qplot(age,wage,data=training, colour=jobclass)
dev.off()

