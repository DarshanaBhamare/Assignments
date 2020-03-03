library(readr)
fraud_data <- read_csv(file.choose())
fraud_data$Fraud <- factor(ifelse(fraud_data$Taxable.Income>30000,"Good","Risky"))

library(caTools)
split <- sample.split(fraud_data$Fraud,SplitRatio =0.80)
train_data <- subset(fraud_data,split==TRUE)
test_data <- subset(fraud_data,split==FALSE)

#Preparing the model
library(rpart)
classifier <- rpart(formula = Fraud~.,data=train_data)
summary(classifier)
y_pred <- predict(classifier,newdata = test_data[-7],type='class')
library(gmodels)
CrossTable(test_data$Fraud,y_pred)
mean(test_data$Fraud==y_pred)

# Plotting the tree
plot(classifier)
text(classifier)

##Using randomForest
#Preparing the model
library(randomForest)
attach(train_data)
classifier_RF <- randomForest(formula = Fraud ~ City.Population + factor(Marital.Status) + Taxable.Income+
                             factor(Undergrad)+Work.Experience,data=train_data,importance=TRUE,na.action = na.roughfix)
summary(classifier_RF)
y_pred_RF <- predict(classifier_RF,newdata = test_data[-7],type='class')
library(gmodels)
CrossTable(test_data$Fraud,y_pred)
mean(test_data$Fraud==y_pred)

# Plotting the tree
varImpPlot(classifier_RF)


