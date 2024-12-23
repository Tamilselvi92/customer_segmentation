# Customer segmentation

# Data upload

customer_data=read.csv("C:/Users/tamil/Desktop/Git_hub_project/customer_segmentation/Mall_Customers.csv")
# To get the summary of Data

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

# Visualizations

# Gender wise 

a= table(customer_data$Gender)
barplot(a,main="Gender wise comparison",
        ylab="count",
        xlab="Gender",
        col=rainbow(2),
        legend=rownames(a))

# Using a pie chart to observe the ratio of male and female distribution

pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep="")
library(plotrix)
pie3D(a,labels=lbs,main="Male Vs Female ratio")

# Age wise 

hist(customer_data$Age,
     col="pink",
     main="Age wise Comparison",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)

boxplot(customer_data$Age,
        col="pink",
        main="Boxplot for Descriptive Analysis of Age")

# Annual Income wise comparison

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

# Analyzing the Customer Spending

print(summary(customer_data$Spending.Score..1.100.))
boxplot(customer_data$Spending.Score..1.100.,
        horizontal = TRUE,
        col="#dedb78",
        main="Customer spending score"
        )
hist(customer_data$Spending.Score..1.100.,
     main="Histogram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="#4700ed",
     labels=TRUE)

# Elbow Method 

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

# Sillhoute method

library(cluster) 
library(gridExtra)
library(grid)

# Repeating the same steps to get an optimized value

k1<-kmeans(customer_data[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
s1<-plot(silhouette(k1$cluster,dist(customer_data[,3:5],"euclidean")))

k2<-kmeans(customer_data[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
s2<-plot(silhouette(k2$cluster,dist(customer_data[,3:5],"euclidean")))

k3<-kmeans(customer_data[,3:5],3,iter.max=100,nstart=50,algorithm="Lloyd")
s3<-plot(silhouette(k3$cluster,dist(customer_data[,3:5],"euclidean")))

k4<-kmeans(customer_data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s4<-plot(silhouette(k4$cluster,dist(customer_data[,3:5],"euclidean")))

k5<-kmeans(customer_data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s5<-plot(silhouette(k5$cluster,dist(customer_data[,3:5],"euclidean")))

k6<-kmeans(customer_data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s6<-plot(silhouette(k6$cluster,dist(customer_data[,3:5],"euclidean")))

k7<-kmeans(customer_data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s7<-plot(silhouette(k7$cluster,dist(customer_data[,3:5],"euclidean")))

k8<-kmeans(customer_data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s8<-plot(silhouette(k8$cluster,dist(customer_data[,3:5],"euclidean")))

k9<-kmeans(customer_data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s9<-plot(silhouette(k9$cluster,dist(customer_data[,3:5],"euclidean")))

k10<-kmeans(customer_data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s10<-plot(silhouette(k10$cluster,dist(customer_data[,3:5],"euclidean")))

# Gap statistic
library(NbClust)
library(factoextra)
silhouette_graph<-fviz_nbclust(customer_data[,3:5], kmeans, method = "silhouette")
plot(silhouette_graph)

set.seed(125)
stat_gap <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
stat_gap_graph<-fviz_gap_stat(stat_gap)
plot(stat_gap_graph)

# Visualizing the Clustering Results using the First Two Principle Components

# k7

k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
print(k6)

pcclust=prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
print(summary(pcclust))
print(pcclust$rotation[,1:2])

# Segmenting the clusters

set.seed(804)
plot(ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Customers based on Annual Income", subtitle = "Using K-means Clustering")
)



plot(ggplot(customer_data, aes(x =Spending.Score..1.100., y = Age)) + 
       geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
       scale_color_discrete(name=" ",
                            breaks=c("1", "2", "3", "4", "5","6"),
                            labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
       ggtitle("Segments of Customers based on age", subtitle = "Using K-means Clustering")
)



kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}
digCluster<-k6$cluster; dignm<-as.character(digCluster); 

# K-means clusters

plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("bottomright",unique(dignm),fill=unique(kCols(digCluster)))
























