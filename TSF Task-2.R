#By: Samruddhi Santosh Dusane
#Task-1: Prediction using Unsupervised ML
#Objective: Predict the optimum number of clusters 
#and represent it visually.

#Loading the dataset
library(datasets)
data(iris)
View(iris)


#Checking for missing values
summary(is.na(iris))

#View statistical summary of dataset
summary(iris)

#Removing the "Species" column
df <- iris[,-5]
View(df)

#Normalizing the variables
nm <- function(x){
  return((x - min(x))/(max(x)-min(x)))}

df$Sepal.Length <- nm(df$Sepal.Length)
df$Sepal.Width <- nm(df$Sepal.Width)
df$Petal.Length <- nm(df$Petal.Length)
df$Petal.Width <- nm(df$Petal.Width)
View(df)

#Plotting of Elbow curve
library(factoextra)
fviz_nbclust(df, kmeans, method = "wss")

#Here at cluster 3 we are getting a proper elbow
#therefore we can consider 3 clusters.

#Using function kmeans
set.seed(123)
km.res <- kmeans(df, 3) 
km.res

#Representation of Cluster plot
library(factoextra)
fviz_cluster(km.res, data = df)  #visualization of partitioning, uses principal components if number of columns > 2

#Verify results of clustering
par(mfrow=c(2,2), mar=c(5,4,2,2))
# Plot to see how Sepal.Length and Sepal.Width
#data points have been distributed in clusters
plot(df[c(1,2)], col=km.res$cluster)
# Plot to see how Sepal.Length and Sepal.Width
#data points have been distributed originally as
#per "class" attribute in dataset
plot(df[c(1,2)], col=iris$Species)

#Similarly for Petal.Length and Petal.Width 
#data points
plot(df[c(3,4)], col=km.res$cluster)
plot(df[c(3,4)], col=iris$Species)

#Create Confusion matrix
table(km.res$cluster, iris$Species)

#Result of table shows that Cluster 1 corresponds to 
#Virginica, Cluster 2 corresponds to Versicolor and 
#Cluster 3 to Setosa.
#Total number of correctly classified instances are: 50+47+36= 133
#Total number of incorrectly classified instances are: 14+3=17
#Accuracy = 133/(133+17) = 0.8866 i.e our model has achieved 88.67% accuracy!
