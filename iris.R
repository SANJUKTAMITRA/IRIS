#import the iris data 
Iris <- read.csv("IRIS.csv",stringsAsFactors = FALSE)

#check for duplicates and missing value

sum(is.na(Iris))
unique(Iris)

#no duplicates and missing values
#check for outliers 

hist(Iris$Sepal_Length)
hist(Iris$Sepal_Width)
boxplot(Iris$Sepal_Width)
hist(Iris$Petal_Length)

hist(Iris$Petal_Width)

#only sepal_width contain outliers, we will delete it 



Iris1 <- Iris[,-5]
quantile(Iris1$Sepal_Width,seq(0,1,0.01))

Iris1[which(Iris1$Sepal_Width > 4.151),2] <- 4.2

str(Iris1)
#EDA
plot(iris$Sepal_Length,iris$Sepal_Width)


ggplot(iris1,aes(x=Sepal_Length,y=Sepal_Width,col=Class)) + geom_point()

ggplot(iris1,aes(x=Petal_Length,y=Petal_Width,col=Class)) + geom_point()

#create clusters Kmeans

clus1 <- kmeans(Iris1,centers =3,nstart = 150)

clus1$centers

rsq <- c()

for(i in 1:20){
  clus <- kmeans(Iris1,centers = i,nstart = 150)
  rsq[i] <- clus$betweenss/clus$totss
}

plot(rsq)

#elbow point 3,4,5

Iris$cluster <- clus1$cluster


rsq[3]
#88.4%
#model evaluation

table(Iris$Class,Iris$cluster)

#virginica is 36 and 14

#hiearchical clustering

dist <- dist(Iris1)

hclus1 <- hclust(dist,method="single")
hclus2 <- hclust(dist,method="complete")
plot(hclus1)
plot(hclus2)

#hclus2 is better and will be used further
rect.hclust(hclus2,k=3,border = "pink")
rect.hclust(hclus2,k=4,border = "red")

iris_cut <- cutree(hclus2,k=3)

Iris$HCluster <- iris_cut

#model evaluation

table(Iris$Class,Iris$HCluster)

#versicolor is 23 and 27

#plot the centroids

#write the Iris file for analysis in Tableau

write.csv(Iris,"Iris.csv")

#Logistic regression model to predict the class which is a 3 level variable
