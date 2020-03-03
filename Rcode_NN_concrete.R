concreteData<-read.csv(file.choose())
attach(concreteData)
normalise<- function(X) {
  return((X-min(X))/(max(X)-min(X)))
}

#apply normalization function to data frame
concreteData_norm<-as.data.frame(lapply(concreteData,normalise))
#partitioning
train_data<-concreteData_norm[1:773,]
test_data<-concreteData_norm[774:1030,]

library(neuralnet)
colnames(concreteData)
concrete_model<-neuralnet(formula = strength~ cement+ slag + ash +water + 
                            superplastic +coarseagg +fineagg +age , hidden = 1,data = train_data)

plot(concrete_model) #error: 5.085897 steps: 2435

y_pred<-compute(concrete_model,test_data[1:8])
predicted_strength<-y_pred$net.result

cor(predicted_strength,test_data$strength)#0.8057665

#improving model performance by adding hidden layers
concrete_model1<-neuralnet(formula = strength~ cement+ slag + ash +water + 
                            superplastic +coarseagg +fineagg +age , hidden = 5,data = train_data)

plot(concrete_model1) #error: 1.720195 steps:12774

y_pred1<-compute(concrete_model1,test_data[1:8])
predicted_strength1<-y_pred1$net.result
cor(predicted_strength1,test_data$strength) #0.9348984
