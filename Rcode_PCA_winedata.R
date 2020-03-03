library(readr)
wine_dataset <- read_csv(file.choose())
library(caTools)
set.seed(123)
split <- sample.split(wine_dataset$Type,SplitRatio = 0.8)
training_set <- subset(wine_dataset,split==TRUE)
test_set <- subset(wine_dataset,split==FALSE)

#Preprocessing 
library(caret)
pca <- preProcess(x=training_set[-1],method="pca",pcaComp = 3)
training_set_new <- predict(pca,training_set)
test_set_new <- predict(pca,test_set)

#Clustering upon both training and test data

#Hierarchial Clustering
dendogram <- hclust(d=dist(test_set_new[-1],method='euclidean'),method='ward.D')
plot(dendogram,hang=-1)
#Optimal number of clusters=2
hc <- hclust(d=dist(test_set_new[-1],method='euclidean'),method='complete')
y_hc <- cutree(hc,2)
test_set_new <- cbind(test_set_new,y_hc)
library(gmodels)
CrossTable(test_set_new$y_hc,test_set_new$Type)
accuracy_hc <- mean(test_set_new$y_hc==test_set_new$Type)*100

#kmeans
wss <- vector()
for (i in 1:10)
  wss[i] <- sum(kmeans(test_set_new[-c(1,5)],centers=i)$withinss)
plot(1:10,wss,type='b',xlab='NO. of Clusters',ylab='Within Sum of Squares')
#elbow curves shows optimal number of clusters as 2
y_kmeans <- kmeans(test_set_new[-c(1,5)],centers = 2)
test_set_new <- cbind(test_set_new,y_kmeans$cluster)
library(gmodels)
CrossTable(test_set_new$`y_kmeans$cluster`,test_set_new$Type)
accuracy_kmeans<- mean(test_set_new$`y_kmeans$cluster`==test_set_new$Type)*100
