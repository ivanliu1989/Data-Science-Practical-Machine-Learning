setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Practical-Machine-Learning")
data(iris)
library(ggplot2)
library(caret)
names(iris)
table(iris$Species)

inTrain <- createDataPartition(y=iris$Species, p=.7, list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(train)
dim(testing)
png('species.png')
qplot(Petal.Width,Sepal.Width,colour=Species,data=training)
dev.off()

modFit <- train(Species ~ .,method="rpart",data=training)
print(modFit$finalModel)

png('tree.png')
plot(modFit$finalModel, uniform=TRUE, 
     main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)
dev.off()
png('tree2.png')
library(rattle)
fancyRpartPlot(modFit$finalModel)
dev.off()

predict(modFit,newdata=testing)
