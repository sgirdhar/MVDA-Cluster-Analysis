setwd("D:/MS/SEM 3/MVDA/Exercises/MVDA-Cluster-Analysis")

diet = read.csv("Cluster_Analysis_Survey.csv", header = TRUE, sep = ",")

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



