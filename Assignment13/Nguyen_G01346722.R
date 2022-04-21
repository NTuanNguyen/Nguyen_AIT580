###------------------
###Regression and Clustering
###------------------

###Students Name: Tuan Nguyen
###GNumber: G01346722

library(ggplot2)
setwd('D:/Work/GMU -US/Spring 2022/AIT 580/Nguyen_AIT580/AIT580-master/data')
data <- read.csv('EmployeeAttrition.csv')

##### REGRESSION #####

# a).Show the scatter plot with relationship curve between TotalWorkingYears and
# MonthlyIncome. Briefly explain your observation in the plot

ggplot(data = data, aes(x = TotalWorkingYears, y = MonthlyIncome))+
  geom_point(color = 'black', fill = 'yellow', shape = 21)+
  geom_smooth()

cat('Based on visualization of these 2 attributes, it appears that there is 
a positive relation ship curve between working years and monthly income. 
Furthermore, the slope of the curve changes drastically at working year = 20,
and the data appears to be concentrate in 2 clusters.
')

# b). b. Show the scatter plot with relationship curve between Age and DistanceFromHome.
# Briefly explain your observation in the plot 

ggplot(data = data, aes(x = Age, y = DistanceFromHome))+
  geom_point(color = 'black', fill = 'yellow', shape = 21)+
  geom_smooth()

cat(' From the visualization there appears to be no special 
relationship between the 2 attributes, as the relationship curve follows a straight horizontal line')

# c. Calculate Correlation for (a) and (b) and explain the values to support 
# your answer in (a) and (b)

cor_a = cor(data$TotalWorkingYears,data$MonthlyIncome)

sprintf('The correlation between TotalWorkingYears and MonthlyIncome is: %f',cor_a)
cat('This agrees with previous observation on a positive correlation between the 2 
attributes, as cor > 0.7 implies strong correlation')

cor_b = cor(data$Age,data$DistanceFromHome)

sprintf('The correlation between Age and DistanceFromHome is: %f',cor_b)
cat('This correlation value is approximately zero, which implies no correlation between
the two attributes, as visualized in previous section')

# d. Linear Regression

lm1 = lm(MonthlyIncome ~ TotalWorkingYears,data = data)
summary(lm1)

cat('Based on summary of the linear model, p-value is significantly smaller than alpha = 5%,
meaning the variable TotalWorkingYears has statistical significant in predicting MonthlyIncome.
The coeffcient shows that when working year increase of 1, MonthlyIncome incrase by 467.66.
The R-squared show that the model explains 59.71% of variation in MonthlyIncome')

##### CLUSTERING #####

# a.Use K-means Clustering algorithm to find groups between HourlyRate and
# TotalWorkingYears. Use number of clusters as 3. Explain how each group is different
# from another in terms of employees representing those groups.

data_sub = subset(data, select = c('HourlyRate','TotalWorkingYears'))

# Set seed for clustering
set.seed(2)
k_3 = kmeans(data_sub,3)

# Plot clusters using ggplot
data$cluster3 = as.character(k_3$cluster)
#ref: https://rpubs.com/aephidayatuloh/clustervisual
ggplot()+
  geom_point(data = data, 
           mapping = aes(x = TotalWorkingYears, 
                         y = HourlyRate, 
                         colour = cluster3)) +
  geom_point(mapping = aes_string(x = k_3$centers[, "TotalWorkingYears"], 
                                  y = k_3$centers[, "HourlyRate"]),
             color = "red", size = 4)

# Aggregate and create dataframe of mean value for each columns based on result
# of clustering
k_3_data = aggregate(data, by=list(cluster=k_3$cluster), mean)
# Remove rows with all NA values
k_3_data = k_3_data[,colSums(is.na(k_3_data))<nrow(k_3_data)]
print(k_3_data)

cat('From the data table, we can see the mean of value in each columns are roughly equal.
From the plot, the 3 clusters shows no special patterns, and are clustered into 3 clusters
of roughly equal size')

# b. Use number of clusters as 5. What did you observe? Did you see any split of groups
# observed in (a)? Observe the splitting groups and explain in terms of employees
# representing those groups

set.seed(2)
k_5 = kmeans(data_sub,5)

data$cluster5 = as.character(k_5$cluster)

ggplot()+
  geom_point(data = data, 
             mapping = aes(x = TotalWorkingYears, 
                           y = HourlyRate, 
                           colour = cluster5)) +
  geom_point(mapping = aes_string(x = k_5$centers[, "TotalWorkingYears"], 
                                  y = k_5$centers[, "HourlyRate"]),
             color = "red", size = 4)

k_5_data = aggregate(data, by=list(cluster=k_5$cluster), mean)
k_5_data = k_5_data[,colSums(is.na(k_5_data))<nrow(k_5_data)]
print(k_5_data)

cat('Compare to 3 means model, 5 means model form more disctinct cluster. Visually we can see that
the bottom portion of the graph has been split into 3 different clusters instead of 1. Based on
the mean value of columns for each cluster, we can see a clear distinction in several columns 
such as Age, HourlyRate,JobLevel, MonthlyIncome, ...etc.
It is clear that the k=5 means performs better compare to k=3 means model.')
