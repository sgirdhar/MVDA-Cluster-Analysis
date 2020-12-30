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

# Let's create logical groups


################## Cluster Analaysis - Hierarchical ####################

# Ward hierarchical clustering

d = dist(diet, method = "euclidean")
fit = hclust(d, method = "ward")
plot(fit)
groups = cutree(fit, k = 5)
rect.hclust(fit, k = 5, border = "red")


# Avg hierarchical clustering

d1 = dist(diet, method = "euclidean")
fit1 = hclust(d1, method = "average")
plot(fit1)
groups1 = cutree(fit1, k = 5)
rect.hclust(fit1, k = 5, border = "red")


# Complete hierarchical clustering

d2 = dist(diet, method = "euclidean")
fit2 = hclust(d2, method = "complete")
plot(fit2)
groups2 = cutree(fit2, k = 5)
rect.hclust(fit2, k = 5, border = "red")


# Single hierarchical clustering

d3 = dist(diet, method = "euclidean")
fit3 = hclust(d3, method = "single")
plot(fit3)
groups3 = cutree(fit3, k = 5)
rect.hclust(fit3, k = 5, border = "red")


################## Cluster Analaysis - k means ####################
# removal of main_meal
# diet = diet[,-15]
# diet = diet[,-15]

library(cluster)

# K-means clustering with 3 clusters
fit4 = kmeans(diet, 3)
fit4
clusplot(diet, fit4$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

fit5 = kmeans(diet, 2)
fit5
clusplot(diet, fit5$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

library(ggplot2)
library(g)

color =  c("red", "blue", "green", "black")
clus4 = cutree(diet, 4)
plot(as.phylo(diet), type = "fan",  tip.color = colors[clus4], label.offset = 1, cex = 0.7)


############# k means for group1 ###############

# group1 => sweets, weight
group1 = cbind(diet$sweets, diet$weight)
group1_kmeans_clust = kmeans(group1, 4)
clusplot(group1, group1_kmeans_clust$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

############# k means for group2 ###############

#group2 => alcohol, region1, region2
set.seed(38)
group2 = cbind(diet$alcohol, diet$region1, diet$region2)
group2_kmeans_clust = kmeans(group2, 3)
clusplot(group2, group2_kmeans_clust$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

############# k means for group3 ###############

#group2 => meals_per_day, bmi

group3 = cbind(diet$meals_per_day, diet$bmi)
group3_kmeans_clust = kmeans(group3, 5)
clusplot(group3, group3_kmeans_clust$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

wcss = vector()
for (i in 1:20) wcss[i] = sum(kmeans(group3, i)$withinss)
plot(1:20, wcss, type = 'b', main = paste('The Elbow Method'), xlab = 'Number of clusters', ylab = 'WCSS')

