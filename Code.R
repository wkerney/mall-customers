install.packages("dplyr")
install.packages("cluster")
install.packages("caret")
install.packages("randomForest")
install.packages("ggplot2")
install.packages("knitr")
library(dplyr)
library(cluster)
library(randomForest)
library(ggplot2)
library(caret)
library(knitr)
# Load the dataset
data <- read.csv("Mall_Customers.csv")

# Preview the dataset
head(data)
str(data)

# Data cleaning
data_cleaned <- data %>%
  distinct() %>%
  na.omit()

# Basic exploration
summary(data_cleaned)

# Calculate average spending
avg_spending <- mean(data_cleaned$Spending.Score..1.100.)
avg_spending  # Average spending score

# Spending by gender
gender_spending <- data_cleaned %>%
  group_by(Gender) %>%
  summarize(AverageSpending = mean(Spending.Score..1.100.))

# Spending by age group
age_spending <- data_cleaned %>%
  mutate(AgeGroup = cut(Age, breaks = c(0, 20, 30, 40, 50, 60, Inf), labels = c("0-20", "21-30", "31-40", "41-50", "51-60", "60+"))) %>%
  group_by(AgeGroup) %>%
  summarize(AverageSpending = mean(Spending.Score..1.100.))

# Determine the optimal number of clusters using the elbow method
set.seed(123)
wss <- sapply(1:10, function(k) {
  kmeans(data_cleaned[, c("Annual.Income..k..", "Spending.Score..1.100.")], centers = k, nstart = 10)$tot.withinss
})

# Plot the elbow curve to identify the optimal number of clusters
ggplot(data.frame(k = 1:10, wss = wss), aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method for Determining Optimal Clusters")

# Assuming the optimal number of clusters is 5
kmeans_result <- kmeans(data_cleaned[, c("Annual.Income..k..", "Spending.Score..1.100.")], centers = 5, nstart = 10)

# Assign cluster labels to the dataset
data_cleaned$Cluster <- kmeans_result$cluster

# Scatter plot of clusters
ggplot(data_cleaned, aes(x = Annual.Income..k.., y = Spending.Score..1.100., color = as.factor(Cluster))) +
  geom_point(size = 3) +
  labs(title = "Customer Segmentation by K-means Clustering",
       color = "Cluster")

# Bar plot of average spending by age group
ggplot(age_spending, aes(x = AgeGroup, y = AverageSpending)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(title = "Average Spending by Age Group")

# Bar plot of average spending by gender
ggplot(gender_spending, aes(x = Gender, y = AverageSpending)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Average Spending by Gender")
