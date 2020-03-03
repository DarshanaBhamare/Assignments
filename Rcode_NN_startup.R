startup<-read.csv(file.choose())
install.packages("reval")

startup$State <- as.numeric(revalue(startup$State,
                                     c("New York"="0", "California"="1",
                                       "Florida"="2")))
startup<-as.data.frame(startup)
attach(startup)
normalise<- function(X) {
  return((X-min(X))/(max(X)-min(X)))
}
#apply normalization function to data frame
startup_norm<-as.data.frame(lapply(startup[,-4],normalise))
summary(startup_norm)

# Data Partition 
set.seed(123)
sample <- sample.split(startup_norm$Profit,SplitRatio = 0.70)
train <- subset(startup_norm,sample==TRUE)
test <- subset(startup_norm,sample==FALSE)

library(neuralnet)
startup_model<-neuralnet(formula = Profit~ R.D.Spend+Administration+Marketing.Spend,hidden = 1,data = train)
plot(startup_model) #error:0.040523 steps :190
# Evaluating model performance
y_pred<-compute(startup_model,test[1:3])
pred_profit<-y_pred$net.result

cor(pred_profit,test$Profit)#0.9524407
> 
