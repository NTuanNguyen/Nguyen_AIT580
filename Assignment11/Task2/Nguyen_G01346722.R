###------------------
###Visualization
###------------------

###Students Name: Tuan Nguyen
###GNumber: G01346722


rm(list=ls())

data <- read.csv('D:/Work/GMU -US/Spring 2022/AIT 580/Nguyen_AIT580/AIT580-master/data/EmployeeAttrition.csv')


#tidyverse include ggplot2 package, plotly convert normal ggplot to interactive plot
library(tidyverse)
library(plotly)

# a.Create Histogram for Age 

plot1 <- data %>% 
  ggplot(aes(x = Age)) + 
  geom_histogram(binwidth = 1,color = 'black', fill = 'yellow') + 
  labs(title = 'Histogram of Employee Age') +
  theme_light()

plot1i <- ggplotly(plot1)

# b.Create Scatter Plot for Age and Monthly Income

plot2 <- data %>% 
  ggplot(aes(x = Age, y = MonthlyIncome)) + 
  geom_point(color = 'black', fill = 'yellow', shape = 21) + 
  labs(title = 'Employee Age versus Monthly Income',
       x = 'Age',
       y = 'Monthly Income') +
  theme_light()

plot2
 
