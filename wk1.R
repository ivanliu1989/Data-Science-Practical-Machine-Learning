setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Practical-Machine-Learning")
library(kernlab)
data(spam)
head(spam)

plot(density(spam$your[spam$type=='nonspam']),col='blue',main='',xlab='Frequency of "Your"')
lines(density(spam$your[spam$type=='spam']),col='red')
