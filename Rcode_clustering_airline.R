library(readxl)
eastwest_airline <- read_excel("E:/Darshana_assignments/Clustering/EastWestAirlines.xlsx", sheet = "data")
eastwest_airline<-eastwest_airline[,2:12]
normalised_data<-scale(eastwest_airline)
d<-dist(normalised_data,method = "euclidean")
fit<-hclust(d,method = "complete")
plot(fit) #dendrogram
groups <- cutree(fit, k=5) #cut tree into 3 clusters
rect.hclust(fit,k=3,border = "red")
membership<-as.matrix(groups)
final<-data.frame(eastwest_airline,membership)
#cluster details
final1<-final[,c(ncol(final),1:(ncol(final)>1))]

##k-means clustering
fit1<-kmeans(normalised_data,5)
str(fit1)
final_kmeans<-data.frame(fit1$cluster,eastwest_airline)
#to see how many clusters should build
wss=(nrow(normalised_data)-1 * sum(apply(normalised_data,2,var)))
for (i in 1:7)
  wss[i]=sum(kmeans(normalised_data,centers = i)$withinss)
plot(1:7,wss,type = "b",xlab = "number of clusters",ylab = "within groups SS")
title(sub = "k means clustering screenplot")
#by k means clustering screenplot we can see it bended out in 5-6 so we can build 5-6 clusters

