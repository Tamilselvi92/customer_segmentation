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