salaryData_train<-read.csv(file.choose())

library(kernlab)
str(salaryData_train)
salaryData_train$educationno <- as.factor(salaryData_train$educationno)
class(salaryData_train)
salaryData_test<-read.csv(file.choose())
str(salaryData_test)
salaryData_test$educationno <- as.factor(salaryData_test$educationno)
class(salaryData_test)
#building model
attach(salaryData_train)
salary_model<-ksvm(Salary~., data=salaryData_train,kernel="vanilladot")
salary_model
#prediction on test data
y_pred<-predict(salary_model,salaryData_test)
table(y_pred,salaryData_test$Salary)
mean(y_pred==salaryData_test$Salary)# 0.8464143

final <- y_pred == salaryData_test$Salary
table(final)
#FALSE  TRUE 
#2313 12747 