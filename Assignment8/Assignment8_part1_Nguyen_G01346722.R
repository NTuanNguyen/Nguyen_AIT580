###--------------------------------------
#Student Name:
#GNumber:
###--------------------------------------

rm(list=ls())

data <- read.csv('D:/Work/GMU -US/Spring 2022/AIT 580/Nguyen_AIT580/AIT580-master/data/EmployeeAttrition.csv')

# this is just for testing to use "print" statement.
print(data[1,])



# a. Find the number of rows and columns in the dataset (5 points)

sprintf("Number of rows: %i ", nrow(data))
sprintf("Number of columns: %i ", ncol(data))

# b. Find the maximum Age in the dataset (5 points)

sprintf("Maximum Age: %.3f ", max(data$Age))

# c. Find the minimum DailyRate in the dataset (5 points)

sprintf("Minimum DailyRate: %.3f ", min(data$DailyRate))

# d. Find the average/mean MontlyIncome in the dataset (5 points)

sprintf("Mean MonthlyIncome: %.3f ", round(mean(data$MonthlyIncome),3))

# e. How many employees rated WorkLifeBalance as 1 (5 points)


sprintf("Number of employees: %i ", nrow(subset(data, WorkLifeBalance == 1)))

# f. What percent of total employees have TotalWorkingYears less than equal to 5? Also calculate the percentage for TotalWorkingYears greater than 5 (5 points)

perc <-   round(nrow(subset(data, TotalWorkingYears <= 5))*100 / nrow(data),3)
cat("Percentage of total employees with working years: \n\t Less than equal 5: ",perc,
    "% \n\t More than 5: ", 100-perc, "%")
rm(perc)

# g. Print EmployeeNumber, Department and MaritalStatus for those employees whose Attrition is Yes and RelationshipSatisfaction is 1 and YearsSinceLastPromotion is greater than 3 (10 points)

print(
  subset(data, Attrition == "Yes" & RelationshipSatisfaction == 1 & YearsSinceLastPromotion > 3, 
         select = c("EmployeeNumber","Department","MaritalStatus"))
)

# h. Find the mean, median, mode, standard deviation and frequency distribution of EnvironmentSatisfaction for males and females separately. (Hint: For frequency distribution use table() function (10 points)


    ## Custom Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#Reference: https://r-lang.com/mode-in-r/

    ## mean, median, mode, standard deviation for male and female 

male_data <- subset(data,Gender == "Male",select = "EnvironmentSatisfaction")

male_data <- list("Mean" = round(mean(male_data$EnvironmentSatisfaction),3),
                  "Median" = round(median(male_data$EnvironmentSatisfaction),3),
                  "Mode" = round(getmode(male_data$EnvironmentSatisfaction),3),
                  "Stdv" = round(sd(male_data$EnvironmentSatisfaction),3))

female_data <- subset(data,Gender == "Female",select = "EnvironmentSatisfaction")

female_data <- list("Mean" = round(mean(female_data$EnvironmentSatisfaction),3),
                  "Median" = round(median(female_data$EnvironmentSatisfaction),3),
                  "Mode" = round(getmode(female_data$EnvironmentSatisfaction),3),
                  "Stdv" = round(sd(female_data$EnvironmentSatisfaction),3))

stat <- do.call(rbind, Map(data.frame, Male=male_data, Female=female_data))

print(stat)

rm(male_data,female_data,stat)

    ##frequency distribution 
temp <- subset(data,,select = c("Gender","EnvironmentSatisfaction"))

tab <- data.frame(as.data.frame.matrix(table(temp)))

tab <- cbind(tab, total = rowSums(tab))

tab[1,]*100/tab[1,5]

tab[2,]*100/tab[2,5]

rm(temp,tab)

                  