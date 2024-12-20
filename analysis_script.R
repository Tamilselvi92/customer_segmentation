# customer segmentation
customer_data=read.csv("C:/Users/tamil/Desktop/Git_hub_project/customer_segmentation/Mall_Customers.csv")
print(str(customer_data))
print(head(customer_data))
print(names(customer_data))
print(summary(customer_data$Age))
print("Age")
print(sd(customer_data$Age))
print(summary(customer_data$Annual.Income..k..))
print(sd(customer_data$Annual.Income..k..))
print(summary(customer_data$Spending.Score..1.100.))
print(sd(customer_data$Spending.Score..1.100.))


# Customer Gender Visualization
a= table(customer_data$Gender)
barplot(a,main="Gender",
        ylab="count",
        xlab="Gender comparison",
        col=rainbow(2),
        legend=rownames(a))

#Visualize a pie chart to observe the ratio of male and female distribution
pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep="")
library(plotrix)
pie3D(a,labels=lbs,main="Male Vs Female ratio")

# Analyzing Age wise trends

hist(customer_data$Age,
     col="pink",
     main="Agewise trends",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)

boxplot(customer_data$Age,
        col="pink",
        main="Boxplot for Descriptive Analysis of Age")

#Analyzing Annual Income wise trends
hist(customer_data$Annual.Income..k..,
     col="brown",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)
plot(density(customer_data$Annual.Income..k..),
     col="#78deb2",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density")
polygon(density(customer_data$Annual.Income..k..),
        col="#78deb2")

#Customer Spending
print(summary(customer_data$Spending.Score..1.100.))
boxplot(customer_data$Spending.Score..1.100.,
        horizontal = TRUE,
        col="#dedb78",
        main="Customer spending score"
        )
hist(customer_data$Spending.Score..1.100.,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="#4700ed",
     labels=TRUE)

#Elbow Method
library(purrr)
set.seed(247)
# function to calculate total intra-cluster sum of square 
icss<-function(k){
  kmeans(customer_data[,3:5],k,iter.max=100,nstart =100,algorithm = "Lloyd")$tot.withinss
}
k.values<-1:10
icss_values<-map_dbl(k.values, icss)
plot(k.values, icss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")

#Sillhoute method
library(cluster) 
library(gridExtra)
library(grid)
k2<-kmeans(customer_data[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
s2<-plot(silhouette(k2$cluster,dist(customer_data[,3:5],"euclidean")))

k3<-kmeans(customer_data[,3:5],3,iter.max=100,nstart=50,algorithm="Lloyd")
s3<-plot(silhouette(k3$cluster,dist(customer_data[,3:5],"euclidean")))

k4<-kmeans(customer_data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s4<-plot(silhouette(k4$cluster,dist(customer_data[,3:5],"euclidean")))

library(NbClust)
library(factoextra)
fviz_nbclust(customer_data[,3:5],kmeans,method = "silhouette")

set.seed(125)
stat_gap <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

