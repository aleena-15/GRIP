getwd()
iris  = read.csv('Iris.csv',header= TRUE )
set.seed(12345)
library(factoextra)
library(cluster)
library(dummies)
###To remove the column
iris = subset(iris, select = -c(1))
##To check if we have n/a values
is.na(iris)
## Dummy variable
iris_1 <- dummy.data.frame(iris, names=c("Species"))

#scale each variable to have a mean of 0 and sd of 1
iris <- scale(iris_1, center=TRUE, scale=TRUE)

##To check how many clusters we need to take:
fviz_nbclust(iris, kmeans, method = "wss")
  ## For this plot it appear that there is a bit of an elbow or "bend" at k = 3 clusters.
#perform k-means clustering with k = 3 clusters
k2 <- kmeans(iris, centers = 3, nstart = 25)
k2
#plot results of final k-means model
fviz_cluster(k2, data = iris)
   ##1 indicates Iris-setosa
   ##2 indicates Iris-versicolor
   ##3 indicates Iris-virginica
#find means of each cluster
aggregate(iris, by=list(cluster=k2$cluster), mean)
#add cluster assigment to original data
final_data <- cbind(iris, cluster = k2$cluster)
