setwd("D:/MS/SEM 3/MVDA/Exercises/MVDA-Cluster-Analysis")

diet = read.csv("Cluster_Analysis_Survey.csv", header = TRUE, sep = ",")

##################### Data preprocssing  ####################

names(diet)
# diet = cbind(diet[,2:3], diet[,5:length(diet)])

# removed timestamp and empty column
diet = diet[, -1]
diet = diet[, -3]

# impute geneder 'prefer not to say' to 'female
diet[9,length(diet)] = 'Female'

diet$region = diet$Which.Region.do.you.belong.to.
diet$age = diet$How.old.are.you...years.completed.
diet$height = diet$What.is.your.height...in.cm.
diet$weight = diet$What.is.your.weight...in.Kg.
diet$meals_per_day = diet$How.many.times.do.you.eat.in.a.day..
diet$fruits = diet$How.many.days.in.a.week.do.you.eat.fruits.
diet$meat = diet$How.many.days.in.a.week.do.you.eat.meat.
diet$dairy = diet$How.many.days.in.a.week.do.you.eat.dairy.products.
diet$sweets = diet$How.many.days.in.a.week.do.you.eat.sweets.
diet$fast_food = diet$How.many.days.in.a.week.do.you.eat.fast.food.
diet$skip_breakfast = diet$How.many.days.in.a.week.do.you.skip.breakfast.
diet$alcohol = diet$How.many.days.in.a.week.do.you.consume.alcoholic.beverages.
diet$main_meal = diet$Most.frequent.source.of.your.main.meal..
diet$geneder = diet$What.is.your.gender..1

attach(diet)

# delete old columns
diet = diet[, -c(1:14)]

# impute 'most frequent meal' Eggs => Home Cooked
diet$main_meal = as.character(diet$main_meal)
diet$main_meal = replace(diet$main_meal, diet$main_meal == 'Eggs', 'Home cooked')

nrow(diet)
region1 = seq(0, 0, length.out = nrow(diet))
region2 = seq(0, 0, length.out = nrow(diet))

region1[diet$region=='Asia'] = 1
region1[diet$region=='Europe'] = 0
region1[diet$region=='Other'] = 0

region2[diet$region=='Asia'] = 0
region2[diet$region=='Europe'] = 1
region2[diet$region=='Other'] = 0

diet$region1 = region1
diet$region2 = region2

diet$geneder = as.character(diet$geneder)
diet$geneder = replace(diet$geneder, diet$geneder == 'Male', '0')
diet$geneder = replace(diet$geneder, diet$geneder == 'Female' , '1')
diet$geneder = as.factor(diet$geneder)

# diet$geneder[is.na(diet$geneder)] = 0

main_meal1 = seq(0, 0, length.out = nrow(diet))
main_meal2 = seq(0, 0, length.out = nrow(diet))

main_meal1[diet$main_meal == 'Home cooked'] = 1
main_meal1[diet$main_meal == 'Pre-cooked/microwave'] = 0
main_meal1[diet$main_meal == 'Canteen'] = 0

main_meal2[diet$main_meal == 'Home cooked'] = 0
main_meal2[diet$main_meal == 'Pre-cooked/microwave'] = 1
main_meal2[diet$main_meal == 'Canteen'] = 0

diet$main_meal1 = main_meal1
diet$main_meal2 = main_meal2

diet = diet[, -1]
diet = diet[, -12]

# convert meals_per_day '6 or more' to 6
diet$meals_per_day = as.character(diet$meals_per_day)
diet$meals_per_day = replace(diet$meals_per_day, diet$meals_per_day == '6 or more', '6')

diet$meals_per_day = as.factor(diet$meals_per_day)
diet$geneder = as.factor(diet$geneder)
# typeof(diet$geneder)

# BMI calculation 
bmi = seq(0, 0, length.out = nrow(diet))
bmi = diet$weight / ((diet$height / 100) ^ 2)
diet$bmi = bmi


################## Cluster Analaysis - Hierarchical ####################

# Here the analysis has been done with distance methpd 'euclidean'

# Ward hierarchical clustering

# Using the dendrogram to find the optimal number of clusters
dendrogram = hclust(d = dist(diet, method = 'euclidean'), method = 'ward.D2')
plot(dendrogram,
     main = paste('Dendrogram of diet'),
     xlab = 'User',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the diet
hc = hclust(d = dist(diet, method = 'euclidean'), method = 'ward.D2')
y_hc = cutree(hc, 2)
rect.hclust(hc, k = 2, border = "red")

# Visualising the clusters
library(cluster)
clusplot(diet,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE)


# Avg hierarchical clustering

dendrogram_avg = hclust(d = dist(diet, method = 'euclidean'), method = 'average')
plot(dendrogram_avg,
     main = paste('Dendrogram of diet'),
     xlab = 'User',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the diet
hc_avg = hclust(d = dist(diet, method = 'euclidean'), method = 'average')
y_hc_avg = cutree(hc_avg, 2)
rect.hclust(hc_avg, k = 2, border = "red")

# Visualising the clusters
library(cluster)
clusplot(diet,
         y_hc_avg,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE)


# Complete hierarchical clustering

dendrogram_max = hclust(d = dist(diet, method = 'euclidean'), method = 'complete')
plot(dendrogram_max,
     main = paste('Dendrogram of diet'),
     xlab = 'User',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the diet
hc_max = hclust(d = dist(diet, method = 'euclidean'), method = 'complete')
y_hc_max = cutree(hc_max, 3)
rect.hclust(hc_max, k = 3, border = "red")

# Visualising the clusters
library(cluster)
clusplot(diet,
         y_hc_max,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE)


# Single hierarchical clustering

dendrogram_min = hclust(d = dist(diet, method = 'euclidean'), method = 'single')
plot(dendrogram_min,
     main = paste('Dendrogram of diet'),
     xlab = 'User',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the diet
hc_min = hclust(d = dist(diet, method = 'euclidean'), method = 'single')
y_hc_min = cutree(hc_min, 2)
rect.hclust(hc_min, k = 2, border = "red")

# Visualising the clusters
library(cluster)
clusplot(diet,
         y_hc_min,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE)


# Here the analysis has been done with distance methpd 'minkowski'

# Ward hierarchical clustering

# Using the dendrogram to find the optimal number of clusters
dendrogram = hclust(d = dist(diet, method = 'minkowski'), method = 'ward.D2')
plot(dendrogram,
     main = paste('Dendrogram of diet'),
     xlab = 'User',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the diet
hc = hclust(d = dist(diet, method = 'minkowski'), method = 'ward.D2')
y_hc = cutree(hc, 2)
rect.hclust(hc, k = 2, border = "red")

# Visualising the clusters
library(cluster)
clusplot(diet,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE)


# Avg hierarchical clustering

dendrogram_avg = hclust(d = dist(diet, method = 'minkowski'), method = 'average')
plot(dendrogram_avg,
     main = paste('Dendrogram of diet'),
     xlab = 'User',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the diet
hc_avg = hclust(d = dist(diet, method = 'minkowski'), method = 'average')
y_hc_avg = cutree(hc_avg, 2)
rect.hclust(hc_avg, k = 2, border = "red")

# Visualising the clusters
library(cluster)
clusplot(diet,
         y_hc_avg,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE)


# Complete hierarchical clustering

dendrogram_max = hclust(d = dist(diet, method = 'minkowski'), method = 'complete')
plot(dendrogram_max,
     main = paste('Dendrogram of diet'),
     xlab = 'User',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the diet
hc_max = hclust(d = dist(diet, method = 'minkowski'), method = 'complete')
y_hc_max = cutree(hc_max, 3)
rect.hclust(hc_max, k = 3, border = "red")

# Visualising the clusters
library(cluster)
clusplot(diet,
         y_hc_max,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE)


# Single hierarchical clustering

dendrogram_min = hclust(d = dist(diet, method = 'minkowski'), method = 'single')
plot(dendrogram_min,
     main = paste('Dendrogram of diet'),
     xlab = 'User',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the diet
hc_min = hclust(d = dist(diet, method = 'minkowski'), method = 'single')
y_hc_min = cutree(hc_min, 2)
rect.hclust(hc_min, k = 2, border = "red")

# Visualising the clusters
library(cluster)
clusplot(diet,
         y_hc_min,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE)


################## Cluster Analaysis - k means ####################
# removal of main_meal
# diet = diet[,-15]
# diet = diet[,-15]

# Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(diet, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the diet
set.seed(29)
kmeans = kmeans(x = diet, centers = 2)
y_kmeans = kmeans$cluster

# Visualising the clusters
library(cluster)
clusplot(diet,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of all people'))



############# cluster analysis for group1 => sweets, weight ###############

group1 = cbind(diet$sweets, diet$weight)

####### kmeans

# Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss_group1 = vector()
for (i in 1:10) wcss_group1[i] = sum(kmeans(group1, i)$withinss)
plot(1:10,
     wcss_group1,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the group1
set.seed(29)
kmeans_group1 = kmeans(x = group1, centers = 2)
y_kmeans_group1 = kmeans_group1$cluster

# Visualising the clusters
library(cluster)
clusplot(group1,
         y_kmeans_group1,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of people eating sweets no. of days in a week vs their weight'),
         xlab = 'Sweets',
         ylab = 'Weight')

####### Hierarchical

# Using the dendrogram to find the optimal number of clusters
dendrogram_group1 = hclust(d = dist(group1, method = 'euclidean'), method = 'ward.D2')
plot(dendrogram_group1,
     main = paste('Dendrogram of group1'),
     xlab = 'User',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the group1
hc_group1 = hclust(d = dist(group1, method = 'euclidean'), method = 'ward.D2')
y_hc_group1 = cutree(hc_group1, 2)
rect.hclust(hc_group1, k = 2, border = "red")

# Visualising the clusters
library(cluster)
clusplot(group1,
         y_hc_group1,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of people eating sweets no. of day in a week vs their weight'),
         xlab = 'Sweets',
         ylab = 'Weight')



############# cluster analysis for group2 => gender, bmi ###############

group2 = cbind(diet$geneder, diet$bmi)

####### kmeans

# Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss_group2 = vector()
for (i in 1:10) wcss_group2[i] = sum(kmeans(group2, i)$withinss)
plot(1:10,
     wcss_group2,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the group1
set.seed(29)
kmeans_group2 = kmeans(x = group2, centers = 2)
y_kmeans_group2 = kmeans_group2$cluster

# Visualising the clusters
library(cluster)
clusplot(group2,
         y_kmeans_group2,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of gender vs bmi'),
         xlab = 'Gender',
         ylab = 'Weight')

####### Hierarchical

# Using the dendrogram to find the optimal number of clusters
dendrogram_group2 = hclust(d = dist(group2, method = 'euclidean'), method = 'ward.D2')
plot(dendrogram_group2,
     main = paste('Dendrogram of group2'),
     xlab = 'User',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the group1
hc_group2 = hclust(d = dist(group2, method = 'euclidean'), method = 'ward.D2')
y_hc_group2 = cutree(hc_group2, 2)
rect.hclust(hc_group2, k = 2, border = "red")

# Visualising the clusters
library(cluster)
clusplot(group2,
         y_hc_group2,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of gender vs bmi'),
         xlab = 'Gender',
         ylab = 'BMI')



############# cluster analysis for group3 => meals_per_day, bmi ###############

group3 = cbind(diet$meals_per_day, diet$bmi)

group3_kmeans_clust = kmeans(group3, 5)
clusplot(group3, group3_kmeans_clust$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

wcss = vector()
for (i in 1:20) wcss[i] = sum(kmeans(group3, i)$withinss)
plot(1:20, wcss, type = 'b', main = paste('The Elbow Method'), xlab = 'Number of clusters', ylab = 'WCSS')

