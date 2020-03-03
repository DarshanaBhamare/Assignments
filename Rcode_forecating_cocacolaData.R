install.packages("forecast")
library(forecast)
install.packages("fpp")
library(fpp)
library(smooth)
library(readxl)
Cocacola <- read_excel(file.choose())
plot(Cocacola$Sales,type="0")
Q1 <-  ifelse(grepl("Q1",Cocacola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",Cocacola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",Cocacola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",Cocacola$Quarter),'1','0')
CocacolaData<-cbind(Cocacola,Q1,Q2,Q3,Q4)
colnames(CocacolaData)
CocacolaData["t"]<- 1:42
#for exponential
CocacolaData["log_Sales"]<-log(CocacolaData["Sales"])
#for quadratic 
CocacolaData["t_square"]<-CocacolaData["t"]*CocacolaData["t"]
#data set ready

attach(CocacolaData)
#partitioning

train<-CocacolaData[1:30,]
test<-CocacolaData[31:42,]
#linear Model
linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
#rmse linear:714.0144483

###exponential
expo_model<-lm(log_Sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
#rmse_expo:552.282103

##Quadratic 
quad_model<-lm(Sales~t+t_square,data=train)
summary(quad_model)
quad_pred<-data.frame(predict(quad_model,interval='predict',newdata=test))
rmse_quad<-sqrt(mean((test$Sales-quad_pred$fit)^2,na.rm=T))
rmse_quad
#rmse_quad:646.27154

##Additive Seasonality

sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add
#rmse_sea_add: 1778.007

##Additive Seasonality with Quadratic
Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad 
#rmse_Add_sea_Quad value: 586.0533

##Additive Seasonality with Exponential
Add_sea_expo_model<-lm(Sales~t+log_Sales +Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_expo_model)
Add_sea_expo_pred<-data.frame(predict(Add_sea_expo_model,interval='predict',newdata=test))
rmse_Add_sea_expo<-sqrt(mean((test$Sales-Add_sea_expo_pred$fit)^2,na.rm=T))
rmse_Add_sea_expo 
#rmse_Add_sea_expo: 407.6664


##Multiplicative Seasonality
multi_sea_model<-lm(log_Sales~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
#rmse_multi_sea:1871.203

###Additive Seasonality with Exponential trend  has least RMSE value
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_quad","rmse_sea_add","rmse_Add_sea_quad","rmse_multi_sea","rmse_Add_sea_expo"),c(rmse_linear,rmse_expo,rmse_quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_Add_sea_expo))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

##combining training and testing data to build additive seasonality using exponential trend
Add_sea_expo_final<-lm(Sales~t+log_Sales +Q1+Q2+Q3+Q4,data=CocacolaData)
summary(Add_sea_expo_final)
#predicting new data
new_model_pred<-data.frame(predict(Add_sea_expo_final,newdata=CocacolaData,interval='predict'))
Quarter <- as.data.frame(CocacolaData$Quarter)

Final <- as.data.frame(cbind(Quarter,CocacolaData$Sales,Add_sea_expo_final$fitted.values))
