###------------------
###Hypothesis Testing
###------------------

###Students Name: Tuan Nguyen
###GNumber: G01346722

setwd("D:/Work/GMU -US/Spring 2022/AIT 580/Nguyen_AIT580")

rm(list=ls())

data <- read.csv('AIT580-master/data/EmployeeAttrition.csv')


# Your hypothesis testings here...

#1. If the MonthlyIncome of Males is greater than Females

#extract data
group1 <- as.numeric(unlist(subset(data, Gender == "Male", select = MonthlyIncome)))
group2 <- as.numeric(unlist(subset(data, Gender == "Female", select = MonthlyIncome)))

#check normality of both group
shapiro.test(group1)
shapiro.test(group2)

cat("Null hypothesis: sample followed normal distribution \n
p-value of both test lower than 1%, reject Null \n
Both samples does not approximate normal distribution")

wilcox.test(group1,group2, paired = FALSE, alternative = "greater")

cat("Null: MonthlyIncome of Male is equal or smaller compare to Female \n
Alternative: MonthlyIncome of Male is Greater compare to female \n
Continuous data, non-parametric distributions, samples are not paired \n
select 2 samples Wilcoxon test (Mann-Whitney test) \n
p-value: 0.9558, larger than 5% \n
Not enough evidence to reject Null \n
MonthlyIncome of Males is NOT greater than Females")

#2. If the WorkLifeBalance of Males is less than Females (5 points)


group1 <- as.numeric(unlist(subset(data, Gender == "Male", select = WorkLifeBalance)))
group2 <- as.numeric(unlist(subset(data, Gender == "Female", select = WorkLifeBalance)))

t.test(group1,group2, paired = FALSE, alternative = "less")

cat("Null: WorkLifeBalance of Male is greater or equal compare to Female \n
Alternative: WorkLifeBalance of Male is less compare to female \n
Discrete data, non-normal distribution, samples are not paired \n
However, value are bounded [1:4], and large sample size, t-test is applicable \n
select 2 samples unpaired t-test \n
p-value: 0.4577, larger than 5% \n
Not enough evidence to reject Null \n
WorkLifeBalance of Males is NOT less than Females")

#3. If the YearsAtCompany of Single is less than Married (5 points)


group1 <- as.numeric(unlist(subset(data, MaritalStatus == "Single", select = YearsAtCompany)))
group2 <- as.numeric(unlist(subset(data, MaritalStatus == "Married", select = YearsAtCompany)))

shapiro.test(group1)
shapiro.test(group2)

cat("Null hypothesis: sample followed normal distribution \n
p-value of both test lower than 1%, reject Null \n
Both samples does not approximate normal distribution")

wilcox.test(group1,group2, paired = FALSE, alternative = "less")

cat("Null: YearsAtCompany of Single is equal or greater compare to Female \n
Alternative: YearsAtCompany of Married is less compare to female \n
Continuous data, non-parametric distributions, samples are not paired \n
select 2 samples Wilcoxon test (Mann-Whitney test) \n
p-value: 0.001047, close to 0, smaller than 5%\n
This show statistical significance, reject Null hypothesis \n
YearsAtCompany of Single is LESS than YearsAtCompany of Married")

#4. If the EnvironmentalSatisfaction of Attrition=Yes is less than Attrition=No (5 points)

group1 <- as.numeric(unlist(subset(data, Attrition == "Yes", select = EnvironmentSatisfaction)))
group2 <- as.numeric(unlist(subset(data, Attrition == "No", select = EnvironmentSatisfaction)))

t.test(group1,group2, paired = FALSE, alternative = "less")

cat("Null: EnvironmentSatisfaction of Attrition=Yes is greater or equal compare to Attrition=No \n
Alternative: EnvironmentSatisfaction of Attrition=Yes is less compare to Attrition=No \n
Discrete data, non-normal distribution, samples are not paired \n
However, value are bounded [1:4], and large sample size, t-test is applicable \n
select 2 samples unpaired t-test \n
p-value: 0.0001046, close to 0, smaller than 5% \n
This show statistical significance, reject Null hypothesis \n
EnvironmentSatisfaction of Attrition=Yes is LESS than Attrition=No")

#5. If the MonthlyIncome of Manager is greater than Laboratory Technician (5 points)

group1 <- as.numeric(unlist(subset(data, JobRole == "Manager", select = MonthlyIncome)))
group2 <- as.numeric(unlist(subset(data, JobRole == "Laboratory Technician", select = MonthlyIncome)))

shapiro.test(group1)
shapiro.test(group2)

cat("Null hypothesis: sample followed normal distribution \n
p-value of both test lower than 1%, reject Null \n
Both samples does not approximate normal distribution")

wilcox.test(group1,group2, paired = FALSE, alternative = "greater")


cat("Null: MonthlyIncome of Manager is equal or less than Laboratory Technician \n
Alternative: MonthlyIncome of Manager is Greater than Laboratory Technician \n
Continuous data, non-parametric distributions, samples are not paired \n
select 2 samples Wilcoxon test (Mann-Whitney test) \n
p-value: 0.8891, larger than 5% \n
Not enough evidence to reject Null \n
MonthlyIncome of Manager is NOT greater than Laboratory Technician")

#6. If YearsAtCompany and DailyRate are correlated with each other (5 points)

summary(lm(YearsAtCompany ~ DailyRate, data = data))

cat("Perform Regression to compare YearsAtCompany and DailyRate \n
Null: There are no correlation \n
R-squared value is close to 0 \n
p-value = 0.192, larger than 5%, which indicate no statistical significance \n
Not enough evidence to reject Null  \n
Result does not show correlation between YearsAtCompany and DailyRate \n
")

#7. If YearsAtCompany and MonthlyIncome are correlated with each other (5 points)

summary(lm(YearsAtCompany ~ MonthlyIncome, data = data))

cat("Perform Regression to compare YearsAtCompany and DailyRate \n
Null: There are no correlation \n
Adjusted R-squared value is 0.264, which can revert to R = 0.513,\n which shows moderate level of correlation  \n
p-value < 2.2e-16, smaller than 5%, which indicate high statistical significance \n
Reject Null, as result show moderate correlation between YearsAtCompany and MonthlyIncome \n
")

#8. If YearsAtCompany varies depending on individual's MaritalStatus (5 points)

#convert MaritalStatus from character to factor
data$MaritalStatus = as.factor(data$MaritalStatus)

summary(aov(YearsAtCompany ~ MaritalStatus, data=data))

cat("Perform ANOVA to check dependency \n
Null: YearsAtCompany does not depend on MaritalStatus \n
Result shows p-value smaller than 5%, but larger than 1% \n
Theres sufficient evidence to reject Null \n
YearsAtCompany does varies depend on MaritalStatus
")

#9. If MonthlyIncome varies depending on individual's PerformanceRating (5 points)

#convert PerformanceRating from character to factor
data$PerformanceRating = as.factor(data$PerformanceRating)

summary(aov(MonthlyIncome ~ PerformanceRating, data=data))

cat("Perform ANOVA to check dependency \n
Null: MonthlyIncome does not depend on PerformanceRating \n
Result shows p-value = 0.512, larger than 5% \n
Theres not sufficient evidence to reject Null \n
MonthlyIncome does not varies depend on PerformanceRating
")

#10. If MonthlyIncome varies depending on individual's WorkLifeBalance (5 points)

#convert WorkLifeBalance from character to factor
data$WorkLifeBalance = as.factor(data$WorkLifeBalance)

summary(aov(MonthlyIncome ~ WorkLifeBalance, data=data))

cat("Perform ANOVA to check dependency \n
Null: MonthlyIncome does not depend on WorkLifeBalance \n
Result shows p-value = 0.607, larger than 5% \n
Theres not sufficient evidence to reject Null \n
MonthlyIncome does not varies depend on WorkLifeBalance
")


