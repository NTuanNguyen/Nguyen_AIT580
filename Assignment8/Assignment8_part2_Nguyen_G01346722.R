library(tidyverse) # Ref: https://ggplot2.tidyverse.org/


df <- read.csv('D:/Work/GMU -US/Spring 2022/AIT 580/Assignments/AIT580-master/data/Acme.csv')

# 1. Identify data types for each attribute in the dataset (5 points)

str(df)

#2. Produce a summary statistic for each attribute in the dataset (5 points)

summary(df)

#3. Produce visualizations for each attribute (Hint: use hist() function) (5 points)

library(gridExtra) #Ref: https://cran.r-project.org/web/packages/gridExtra/index.html


plot1 <- df %>% 
  ggplot(aes(x = Years))+
  geom_histogram(stat = "count", fill = "yellow", color = "black")+
  labs(y = "count",
       title = "Histogram of Years")

plot2 <- df %>%
  ggplot(aes(x = StSalary))+
  geom_histogram(stat = "count", fill = "cyan", color = "black")+
  labs(y = "count",
       title = "Histogram of Salary")

plot3 <- df %>% 
  ggplot(aes(x = Gender, fill = Gender))+
  geom_bar(color = "black")+
  scale_x_discrete(labels = c("Female","Male"))+
  labs(title = "Histogram of Gender")+
  theme(legend.position = "none")

plot4 <- df %>%
  ggplot(aes(x = Degree, fill = Degree))+
  geom_bar(color = "black")+
  scale_x_discrete(labels = c("BS","MS","PhD"))+
  labs(title = "Histogram of Degree")+
  theme(legend.position = "none")

grid.arrange(plot1,plot2,plot3,plot4, ncol = 2, nrow = 2)

# 4.Display the relationship between
    #a. Years of Experience and Starting Salary for all employees (5 points)

library(ggpubr) #Ref: https://rpkgs.datanovia.com/ggpubr/

plot5 <- df %>% 
  ggscatter(x = "Years", y = "StSalary",
            shape = 21, color = "black", fill = "cyan",
            add = "reg.line", conf.int = FALSE, 
            cor.coef = TRUE, cor.method = "pearson",
            title = "All employee")

    #b. Years of Experience and Starting Salary for each gender (5 points)

plot6 <- df %>% subset(Gender == "F") %>%
  ggscatter(x = "Years", y = "StSalary",
            shape = 21, color = "black", fill = "cyan",
            add = "reg.line", conf.int = FALSE, 
            cor.coef = TRUE, cor.method = "pearson",
            title = "Female")

plot7 <- df %>% subset(Gender == "M") %>%
  ggscatter(x = "Years", y = "StSalary",
            shape = 21, color = "black", fill = "cyan",
            add = "reg.line", conf.int = FALSE, 
            cor.coef = TRUE, cor.method = "pearson",
            title = "Male")

grid.arrange(plot5,plot6,plot7, nrow = 3)

    #c. Years of Experience and Starting Salary for each degree (5 points)

plot8 <- df %>% subset(Degree == "BS") %>%
  ggscatter(x = "Years", y = "StSalary",
            xlim = c(1,10),
            shape = 21, color = "black", fill = "cyan",
            add = "reg.line", conf.int = FALSE, 
            cor.coef = TRUE, cor.method = "pearson",
            title = "Bachelor")

plot9 <- df %>% subset(Degree == "MS") %>%
  ggscatter(x = "Years", y = "StSalary",
            xlim = c(1,10),
            shape = 21, color = "black", fill = "cyan",
            add = "reg.line", conf.int = FALSE, 
            cor.coef = TRUE, cor.method = "pearson",
            title = "Master")

plot10 <- df %>% subset(Degree == "PhD") %>%
  ggscatter(x = "Years", y = "StSalary",
            xlim = c(1,10),
            shape = 21, color = "black", fill = "cyan",
            add = "reg.line", conf.int = FALSE, 
            cor.coef = TRUE, cor.method = "pearson",
            title = "Doctorate")

grid.arrange(plot8,plot9,plot10, nrow = 3)

# 5. Find the correlation between Starting Salary and Years of Experience? (5 points)

  #Shown in plot5, R = 0.71 and p-value of 3.1e-8 (pearson), which mean there is a correlation between
  # and starting salary

  #a. Is the correlation different for each gender? (5 points)
    #For Male, R = 0.67 with p = 6.1e-5 (pearson), and for Female, R=0.75 with p = 3.3e-4 (pearson)
    #This shows a stronger correlate between years and salary in Female compare to Male.

  #b. Is the correlation different for each degree? (5 points)
    #for BS, R=0.36/p=0.12 (pearson)
    #for MS, R=0.45/p=0.12 (pearson)
    #for PhD, R=0.046/p=0.88 (pearson)
    #For each type of degree there are no indicator 
    #of correlation between years and starting salary.


# 6. What can you conclude about Acme with respect to gender bias 
#after your overall analysis? (5 points)

# Other factors are needed to be analyze before drawing the final conclusion
# First is the statistics related to starting salary for each gender

df_m = subset(df,Gender == "M", select = "StSalary")
summary(df_m)
df_f = subset(df,Gender == "F", select = "StSalary")
summary(df_f)

#Both the Mean and Median is higher for Male compare to Female,
#this support the claim related to gender bias in ACME

#Second is checking for other factors that affects StSalary, 
#Years has good correlation with salary, other factor is Degree

df$Degree = factor(df$Degree,levels = c('BS','MS','PhD'))

cor(as.numeric(df$Degree),df$StSalary, method = 'spearman')
cor(as.numeric(df$Degree),df$Years, method = 'spearman')

#Spearman correlation was used during Degree is ordinal variable,
#it shows that Degree correlate well with Salary (R=0.685), and extremely well with Years (R=0.941)
#This shows that better degree resulting in higher salary. Next step is looking at degree composition
#between man and woman.

df %>% ggplot(aes(x = Degree, fill = Gender))+ geom_histogram(stat = "count")

#The majority of Doctors are Male, which translate into higher starting salary,
#and explain the higher mean/median for Male compare to Female.

#After detailed analysis, I conclude that there is not enough evidence to show 
#that ACME is biased to a gender when deciding starting salary for employee.
