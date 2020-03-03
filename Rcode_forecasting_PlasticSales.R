library(forecast)
library(fpp)
library(smooth)
library(readxl)
plasticSales<-read.csv(file.choose())
plot(plasticSales$Sales,type="o")
X<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )
colnames(X)<-month.abb
plasticSalesData<-cbind(plasticSales,X)
colnames(plasticSalesData)
plasticSalesData["t"]<- 1:60
plasticSalesData["log_Sales"]<-log(plasticSalesData["Sales"])
plasticSalesData["t_square"]<-plasticSalesData["t"]*plasticSalesData["t"]
attach(plasticSalesData)
#partitioning
train<-plasticSalesData[1:42,]

test<-plasticSalesData[43:60,]
##linear Model

linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear
#rmse_linear:245.0228
##Exponential 
expo_model<-lm(log_Sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo
#rmse_expo:246.6658
##Quadratic 
Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad
#rmse_Quad: 324.6979

##Additive Seasonality
sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add
#rmse_sea_add:263.5833

#Additive Seasonality with Quadratic 
Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad
#rmse_Add_sea_Quad:120.8758

#Additive Seasonality with exponential
Add_sea_expo_model<-lm(Sales~t+log_Sales+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_expo_model)
Add_sea_expo_pred<-data.frame(predict(Add_sea_expo_model,interval='predict',newdata=test))
rmse_Add_sea_expo<-sqrt(mean((test$Sales-Add_sea_expo_pred$fit)^2,na.rm=T))
rmse_Add_sea_expo
#rmse_Add_sea_expo:55.80728

##Multiplicative Seasonality
multi_sea_model<-lm(log_Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea
# rmse_multi_sea:267.3339

#Multiplicative Seasonality Quadratic trend

multi_add_sea_Quad_model<-lm(log_Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_Quad_model) 
multi_add_sea_pred_Quad<-data.frame(predict(multi_add_sea_Quad_model,newdata=test,interval='predict'))
rmse_multi_add_sea_Quad<-sqrt(mean((test$Sales-exp(multi_add_sea_pred_Quad$fit))^2,na.rm = T))
rmse_multi_add_sea_Quad
#rmse_multi_add_sea_Quad: 147.508
###Preparing table 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_Add_sea_expo","rmse_multi_sea","rmse_multi_add_sea_Quad"),
                       c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_Add_sea_expo,rmse_multi_sea,rmse_multi_add_sea_Quad))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse) #rmse_Add_sea_expo	55.80728



###smoothening techniques
library(tseries)
tsplasticSales<-ts(plasticSalesData$Sales,frequency = 12,start = c(60))
#partitioning 
tsTrain<-tsplasticSales[1:48]
tsTest<-tsplasticSales[49:60]
#seasonal data
#converting time series object
tsTrain<-ts(tsTrain,frequency = 12)
tsTest<-ts(tsTest,frequency = 12)
#plotting time series data
plot(tsplasticSales)
#simple exponential model
ses_a<-ses(tsTrain,alpha = 0.2)
ses_a
ses_a_pred<-data.frame(predict(ses_a,h=4))
plot(forecast(ses_a,n.ahead=4))
library(StatMeasures)
sea_mape<- mape(ses_a_pred$Point.Forecast,tsTest)
##Error in mape(ses_a_pred$Point.Forecast, tsTest) : 
#Invalid input: yhat should be numeric or integer vector of predicted linear response