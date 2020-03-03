library(readr)
crime_data <- read.csv(file.choose())
View(crime_data)
mydata<- crime_data[,2:5]
normalised_crime_data<- scale(mydata)
View(normalised_crime_data)
d<- dist(normalised_crime_data,method = "euclidean")
fit<-hclust(d,method="complete")
fit
plot(fit)# dendrogram
plot(fit , hang = -1)
rect.hclust(fit,k=4,border = "red")
group<-cutree(fit,k=4)
membership<-as.matrix(group)
final_crime_data<-data.frame(mydata,membership)
View(final_crime_data)
#cluster details
final_crime_data1<-final_crime_data[,c(ncol(final_crime_data),1:(ncol(final_crime_data)>1))]

##k-means clustering
fit1<-kmeans(normalised_crime_data,4)
str(fit1)
final_kmeans<-data.frame(fit1$cluster,crime_data)
wss=(nrow(normalised_crime_data)-1 * sum(apply(normalised_crime_data,2,var)))
for (i in 1:7)
wss[i]=sum(kmeans(normalised_crime_data,centers = i)$withinss)
plot(1:7,wss,type = "b",xlab = "number of clusters",ylab = "within groups SS")
title(sub = "k means clustering screenplot")
#by k means clustering screenplot we can see it bended out in 4-5 so we can build 4-5 clusters




