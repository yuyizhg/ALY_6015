# Iris Dataset
str(iris)

#Classification Test:
#Split into training and test datasets
set.seed(1234)
ind <- sample(2, nrow(iris), replace=T, prob=c(0.7, 0.3))
iris.train <- iris[ind==1, ]
iris.test <- iris[ind==2, ]

#Build a decision tree
library(party)
iris.formula <- Species ~ Sepal.Length + Sepal.Width +
  Petal.Length + Petal.Width
iris.ctree <- ctree(iris.formula, data=iris.train)
plot(iris.ctree)

#Predict on test data
pred <- predict(iris.ctree, newdata = iris.test)

#Check prediction result
table(pred, iris.test$Species)


#Clustering Test:
#K-means Clustering
set.seed(8953)
iris2 <- iris

#Remove class IDs
iris2$Species <- NULL

#K-means clustering
iris.kmeans <- kmeans(iris2, 3)

#Check result
table(iris$Species, iris.kmeans$cluster)

#Plot clusters and their centers
plot(iris2[c("Sepal.Length", "Sepal.Width")], col=iris.kmeans$cluster)
points(iris.kmeans$centers[, c("Sepal.Length", "Sepal.Width")],
       col=1:3, pch="*", cex=5)

#Density-based Clustering
library(fpc)
iris2 <- iris[-5] # remove class IDs

#DBSCAN clustering
ds <- dbscan(iris2, eps = 0.42, MinPts = 5)

#Compare clusters with original class IDs
table(ds$cluster, iris$Species)

#1-3: clusters; 0: outliers or noise
plotcluster(iris2, ds$cluster)


#fviz_nbclust() function
library(factoextra)
library(NbClust)
fviz_nbclust(iris2, kmeans, method = "wss")
