install.packages("naivebayes")
library(naivebayes)
library(ggplot2)
library(caret)
library(e1071)
library(psych)
SalaryData_Train <- read.csv(file.choose())
str(SalaryData_Train)
SalaryData_Train$educationno <- as.factor(SalaryData_Train$educationno)
class(SalaryData_Train)
SalaryData_Test <- read.csv(file.choose())
str(SalaryData_Test)
SalaryData_Test$educationno <- as.factor(SalaryData_Test$educationno)
class(SalaryData_Test)



NaiveModel <- naiveBayes(SalaryData_Train$Salary ~ ., data = SalaryData_Train)
NaiveModel
NaiveModel_pred <- predict(NaiveModel,SalaryData_Test)
mean(NaiveModel_pred==SalaryData_Test$Salary)
table(SalaryData_Test$Salary,NaiveModel_pred)
confusionMatrix(NaiveModel_pred,SalaryData_Test$Salary)

                