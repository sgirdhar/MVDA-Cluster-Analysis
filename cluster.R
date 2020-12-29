# Ward hierarchical clustering

d = dist(iris, method = "euclidean")
fit = hclust(d, method = "ward")
plot(fit)
groups = cutree(fit, k = 5)
rect.hclust(fit, k = 5, border = "red")


# Avg hierarchical clustering

d2 = dist(iris, method = "euclidean")
fit2 = hclust(d2, method = "average")
plot(fit2)
groups2 = cutree(fit2, k = 5)
rect.hclust(fit2, k = 5, border = "red")


# Complete hierarchical clustering

d3 = dist(iris, method = "euclidean")
fit3 = hclust(d3, method = "complete")
plot(fit3)
groups3 = cutree(fit3, k = 5)
rect.hclust(fit3, k = 5, border = "red")


# Single hierarchical clustering

d4 = dist(iris, method = "euclidean")
fit4 = hclust(d4, method = "single")
plot(fit4)
groups4 = cutree(fit4, k = 5)
rect.hclust(fit4, k = 5, border = "red")


##########################################################
  
# K-means clustering with 3 clusters
iris_data = iris[,1:4]
head(iris_data)

fit5 = kmeans(iris_data, 3)
fit5

# K-means clustering with 2 clusters
iris_data = iris[,1:4]
head(iris_data)

fit6 = kmeans(iris_data, 2)
fit6


# 

library(cluster)

clusplot(iris_data, fit5$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

clusplot(iris_data, fit6$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)
