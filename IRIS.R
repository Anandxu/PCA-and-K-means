library(cluster)
library(factoextra)
library(NbClust)
library(fpc)
library(ggplot2)
library(ggfortify)

data(iris)
head(iris)
iris2 <- iris[1:4]

#PCA
PCAtemp <- prcomp(iris2)
summary(PCAtemp)
PCAtemp$rotation

screeplot(PCAtemp, type = "lines")
abline(v = 2,lty =2, col = "red")

autoplot(PCAtemp)
autoplot(PCAtemp, data = iris, col = "Species", loadings = TRUE, loadings.label = TRUE)

#k-means
k.max <- 15
wss <- sapply(1:k.max, function(k){kmeans(iris2, k, nstart = 10)$tot.withinss})
plot(1:k.max,wss,type = "b",pch = 19, frame = FALSE,
     xlab = "Number of clusters K",ylab = "Total within-clusters sum of squares")
abline(v = 3,lty =2, col = "red")

fviz_nbclust(iris2, kmeans, method = c("silhouette"))

iris.kmeans <- kmeans(iris2,3)
autoplot(iris.kmeans, data = iris2, frame = TRUE, frame.type = "norm")

dis <- dist(iris2)^2
sil <- silhouette(iris.kmeans$cluster, dis)
plot(sil)

plot(iris$Species,iris.kmeans$cluster)
