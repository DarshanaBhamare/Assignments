library(caTools)
library(ggplot2)
library(caret)
library(class)
library(corrplot)
Zoo<-read.csv(file.choose())
Zoo<- Zoo[-1]

split <- sample.split(Zoo$type,SplitRatio = 0.75)
training_set <- subset(Zoo,split==TRUE)
test_set <- subset(Zoo,split==FALSE)
set.seed(1)

predicted.type <- NULL
error.rate <- NULL
for (i in 1:17) {
  predicted.type <- knn(train = training_set[1:16],test =  test_set[1:16],cl =training_set[,17],k=i)
  error.rate[i] <- mean(predicted.type==test_set$Type)
}


y_pred <- knn(train = training_set[-17],test=test_set[-17],cl=training_set[,17],k=3)
library(gmodels)
CrossTable(test_set[,17],y_pred)
mean(y_pred==test_set[,17])*100 
corrplot(cor(Zoo))

