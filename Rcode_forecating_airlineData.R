library(forecast)
library(fpp)
library(readxl)
Airline<-read_excel(file.choose())
plot(Airline$Passengers,type="o")
X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )
colnames(X)<-month.abb
AirlinesData<-cbind(Airline,X)
colnames(AirlinesData)
AirlinesData["t"]<- 1:96

AirlinesData["log_Passenger"]<-log(AirlinesData["Passengers"])
AirlinesData["t_square"]<-AirlinesData["t"]*AirlinesData["t"]
attach(AirlinesData)
#partitioning
train<-AirlinesData[1:84,]

test<-AirlinesData[85:96,]
##linear Model

linear_model<-lm(Passengers~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear
#rmse_linear:53.19924

##Exponential 
expo_model<-lm(log_Passenger~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo
#rmse_expo:46.05736

##Quadratic 
Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad
#rmse_Quad: 48.05189

##Additive Seasonality
sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add
#rmse_sea_add:132.8198

#Additive Seasonality with Quadratic 
Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad
#rmse_Add_sea_Quad:26.36082

#Additive Seasonality with exponential
Add_sea_expo_model<-lm(Passengers~t+log_Passenger+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_expo_model)
Add_sea_expo_pred<-data.frame(predict(Add_sea_expo_model,interval='predict',newdata=test))
rmse_Add_sea_expo<-sqrt(mean((test$Passengers-Add_sea_expo_pred$fit)^2,na.rm=T))
rmse_Add_sea_expo
#rmse_Add_sea_expo:32.03251

##Multiplicative Seasonality
multi_sea_model<-lm(log_Passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea
# rmse_multi_sea:140.0632

#Multiplicative Seasonality Linear trend
multi_add_sea_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea
#rmse_multi_add_sea:10.51917

#Multiplicative Seasonality Quadratic trend
#Multiplicative Seasonality Linear trend
multi_add_sea_Quad_model<-lm(log_Passenger~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_Quad_model) 
multi_add_sea_pred_Quad<-data.frame(predict(multi_add_sea_Quad_model,newdata=test,interval='predict'))
rmse_multi_add_sea_Quad<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred_Quad$fit))^2,na.rm = T))
rmse_multi_add_sea_Quad
#rmse_multi_add_sea_Quad:18.37201

###Preparing table 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_Add_sea_expo","rmse_multi_sea","rmse_multi_add_sea","rmse_multi_add_sea_Quad"),
                       c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_Add_sea_expo,rmse_multi_sea,rmse_multi_add_sea,rmse_multi_add_sea_Quad))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
### Multiplicative Seasonality Linear trend  has least RMSE value

##combining training and testing data to build additive seasonality using exponential trend
new_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = AirlinesData)
new_model_pred<-data.frame(predict(new_model,newdata=AirlinesData,interval='predict'))
Month <- as.data.frame(Airline$Month)
Final <- as.data.frame(cbind(Month,AirlinesData$Passengers,new_model$fitted.values))
Final
