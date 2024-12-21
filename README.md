# Customer Segmentation

## Project Introduction
This machine learning project employs K-means clustering, which is the primary approach for clustering unlabelled datasets.

## Technologies

### Languages
R

### Package

cluster, factoextra, ggplot2, gridExtra, NbClust, plotrix, purrr

## Project Description

### About

Customer Segmentation is the process of division of customer base into several groups of individuals that share a similarity in different ways that are relevant to marketing such as gender, age, interests, and miscellaneous spending habits.

### Problem Statement

You maintaining a mall, and you have some basic information on your customers via membership cards, such as Customer ID, age, gender, annual income, and spending score. A Spending Score is something you assign to a customer based on predefined characteristics such as customer behaviour and purchasing data. You want to divide your clients into different clusters so that you may effectively target them in order to increase sales.

### Dataset Used 


The Dataset used for this project is the <a href="https://www.kaggle.com/datasets/vjchoudhary7/customer-segmentation-tutorial-in-python">Mall Customers</a> Dataset. This dataset has 5 different features. The feartures and their short descirption is mentioned in the table below:


   | Column Name     |   Description    |
 |    :----:   |          :-: |
 | Customer ID      | A Unique Identification Feature assigned to each customer  |
| Gender        |   Customers are classified on the basis of their genders viz. Male and Female   |
 | Age |  Customers are classified on the basis of their Age |
 | Annual Income | This Feature illustrates the Annual Income of Customers in Thousands |
 | Spending Score | This is a feature in which a special spending score is assinged to each customer based on his/her buying behaviour and net spend |
 
### Algorithm Used

## K-means Algorithm

While using the k-means clustering algorithm, the first step is to indicate the number of clusters (k) that we wish to produce in the final output. The algorithm starts by selecting k objects from dataset randomly that will serve as the initial centers for our clusters. These selected objects are the cluster means, also known as centroids. Then, the remaining objects have an assignment of the closest centroid. This centroid is defined by the Euclidean Distance present between the object and the cluster mean. We refer to this step as “cluster assignment”. When the assignment is complete, the algorithm proceeds to calculate new mean value of each cluster present in the data. After the recalculation of the centers, the observations are checked if they are closer to a different cluster. Using the updated cluster mean, the objects undergo reassignment. This goes on repeatedly through several iterations until the cluster assignments stop altering. The clusters that are present in the current iteration are the same as the ones obtained in the previous iteration.

# Methods

Elbow Method

Silhouette method

Gap statistic

## Future Scope

The Future Scope of this project will be to built a user friendly web interface using the <a href="https://shiny.rstudio.com/">Shiny Package</a> offered by R Studio. With the help of Shiny we can create really nice looking interfaces for our R project.



