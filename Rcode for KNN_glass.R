library(caTools)
library(ggplot2)
library(caret)
library(class)
library(corrplot)

glass <- read_csv(file.choose())
glass_scale <- scale(glass[,1:9])
Glass_data <- cbind(glass_scale,glass[10])

set.seed(123)

sample <- sample.split(Glass_data$Type,SplitRatio = 0.70)

train <- subset(Glass_data,sample==TRUE)

test <- subset(Glass_data,sample==FALSE)


predicted.type <- NULL
error.rate <- NULL

for (i in 1:10) {
  predicted.type <- knn(train[1:9],test[1:9],train$Type,k=i)
  error.rate[i] <- mean(predicted.type==test$Type)
  
}

knn.error <- as.data.frame(cbind(k=1:10,error.type =error.rate))
#visualisation 
ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')
#The above plot reveals that error is lowest when k=1 
Glass_pred1 <- knn(train[1:9],test[1:9],train$Type,k=1)
error <- mean(Glass_pred1!=test$Type)
CrossTable(Glass_pred1,test$Type)
corrplot(cor(Glass_data))


