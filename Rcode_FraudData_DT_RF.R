library(tree)
install.packages("party")
library(party)
data<-read.csv(file.choose())
str(data)
#fraud data treating those who have taxable_income <= 30000 as "Risky" and others are "Good"

RiskyGood<-ifelse(data$Taxable.Income<=30000,"Risky","Good")
FD<-data.frame(data,RiskyGood)
#partition data into train and test
FD_train<-FD[1:300,]
FD_test<-FD[301:600,]
attach(FD)

##ctree:Recursive partitioning for continuous, ordered, nominal and multivariate response variables in a conditional inference framework
FD_tree<-ctree(RiskyGood ~ Undergrad+Marital.Status+City.Population+Work.Experience+Urban,data = FD_train)
plot(FD_tree)
text(FD_tree,pretty = 0)

#prediction

Pred_tree<-predict(FD_tree,newdata = FD_test)

mean(Pred_tree==FD_test$RiskyGood) #accuracy 82%
CrossTable(FD_test$RiskyGood,Pred_tree)
confusionMatrix(FD_test$RiskyGood,Pred_tree)



##Using Random Forest

FD_rf<-randomForest(RiskyGood~Undergrad+Marital.Status+City.Population+Work.Experience+Urban,data = FD_train)
FD_rf
pred_rf<-predict(FD_rf,FD_test)
confusionMatrix(FD_test$RiskyGood,pred_rf) #79%accuracy
plot(FD_rf)
