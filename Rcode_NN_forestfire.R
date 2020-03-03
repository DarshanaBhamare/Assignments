forestfires<-read.csv(file.choose())

forestfires$month <- as.numeric(revalue (forestfires$month,
                                      c("jan"="1","feb"="2","mar"="3","apr"="4","may"="5",
                                        "jun"="6","jul"="7","aug"="8",
                                        "sep"="9","oct"="10","nov"="11","dec"="12")))
forestfires$day<-as.numeric(revalue (forestfires$day,c("mon"="1","tue"="2","wed"="3","thu"="4","fri"="5","sat"="6","sun"="7")))
forestfires<-forestfires[,1:11,31]
attach(forestfires)
normalise<- function(X) {
  return((X-min(X))/(max(X)-min(X)))
}
#apply normalization function to data frame
forestfires_norm<-as.data.frame(lapply(forestfires,normalise))
summary(forestfires_norm)
# Data Partition 
set.seed(123)

train <- forestfires_norm[1:361,]
test <- forestfires_norm[362:517,]
library(neuralnet)
forestfires_model<-neuralnet(formula = area~month+day+FFMC+DMC+DC+ISI 
                             + temp+ RH+wind+rain , hidden = 1 , data = train)
plot(forestfires_model) #error: 0.562153 steps:137

# Evaluating model performance
y_pred<-compute(forestfires_model,test[1:10])
pred_area<-y_pred$net.result

cor(pred_area,test$area) #0.08835264

