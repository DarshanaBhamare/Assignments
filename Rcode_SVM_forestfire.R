forestfires<-read.csv(file.choose())

forestfires$month <- as.numeric(revalue (forestfires$month,
                                      c("jan"="1","feb"="2","mar"="3","apr"="4","may"="5",
                                        "jun"="6","jul"="7","aug"="8",
                                        "sep"="9","oct"="10","nov"="11","dec"="12")))
forestfires$day<-as.numeric(revalue (forestfires$day,c("mon"="1","tue"="2","wed"="3","thu"="4","fri"="5","sat"="6","sun"="7")))

attach(forestfires)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forestfires$temp = normalize(forestfires$temp)
forestfires$RH   = normalize(forestfires$RH)
forestfires$wind = normalize(forestfires$wind)
forestfires$rain = normalize(forestfires$rain)
# Data Partition 
set.seed(123)
ind <- sample(2, nrow(forestfires), replace = TRUE, prob = c(0.7,0.3))
forestfires_train <- forestfires[ind==1,]
forestfires_test  <- forestfires[ind==2,]
str(forestfires)
# Building model 
forestfires_classifier<-ksvm(size_category~temp+rain+wind+RH,data=forestfires_train, kernel ="vanilladot")
forestfires_classifier
#compute
Pred<-predict(forestfires_classifier,forestfires_test)
table(Pred,forestfires_test$size_category)
mean(Pred==forestfires_test$size_category) # 0.7354839

result<-Pred==forestfires_test$size_category
table(result)#FALSE  TRUE 
              #41   114 
