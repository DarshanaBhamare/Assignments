install.packages("tree")
library(tree)

companydata<-read.csv(file.choose())
str(companydata)
#sales range
range(companydata$Sales)
attach(companydata)
#creating categorical variable based on sales
high<-ifelse(Sales >=8,"yes","no")
companydata<-data.frame(companydata,high)
names(companydata)
companydata=companydata[,-1]
#training and testing data
set.seed(123)
train<-sample(1:nrow(companydata),nrow(companydata)/2)
test<- -train
training_data<-companydata[train,]
test_data<-companydata[test,]
testing_high<-high[test]

companydata_tree<-tree(high~.,data = training_data)
print(companydata_tree)
plot(companydata_tree)
text(companydata_tree,pretty = 0)
pred_companydata<-predict(companydata_tree,test_data,type = "class")
mean(pred_companydata==testing_high)#accuracy comes 76%



###Using RANDOM FOREST
install.packages("randomForest")
library(randomForest)
company_random<-randomForest(high~.,training_data )
print(company_random)
attributes(company_random)
#prediction and confusion matrix on test_data
pred_rf<-predict(company_random,test_data)
head(pred_rf)
head(training_data)
confusionMatrix(pred_rf,test_data$high)
#error rate of RF
plot(company_random)
mean(pred_rf==testing_high)

