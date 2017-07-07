### References -----------------
# 1. https://www.kaggle.io/svf/441884/647b8c07ae7a081c547af6d9324351c1/__results__.html#
# 2. https://www.kaggle.com/aiswaryaramachandran/d/ludobenistant/hr-analytics/exploratory-analysis
## Problem-------------------
#   Why are our best and most experienced employees
#     leaving prematurely? Try to predict which valuable employees
#       will leave next. Fields in the dataset include:
#         Employee satisfaction level
#         Last evaluation
#         Number of projects
#         Average monthly hours
#         Time spent at the company
#         Whether they have had a work accident
#         Whether they have had a promotion in the last 5 years
#         Sales
#         Salary
#         Whether the employee has left
#############
##### 1. Clear objects, call libraries and read file -----------------
rm(list=ls()) ; gc()
library(dplyr)       # select statement, %>% etc
library(corrplot)    # Correlation plots
library(ggplot2)     
library(gridExtra)   # Grid of plots

# 2. Set working directory
setwd("")
hr<-read.csv("hr.csv",header = T)
dim(hr)
names(hr)
str(hr)
View(hr)

###  3. First visualisations ------------
# This graph present the correlations between each variables.
#   The size of the bubbles reveal the significance of the correlation,
#    while the colour present the direction (either positive or negative).
#  On average people who leave have a low satisfaction level,
#   they work more and didn't get promoted within the past five years.
# 3.1
HR_correlation <- hr %>% select(satisfaction_level:promotion_last_5years)
# 3.2
M <- cor(HR_correlation)
M
M <- as.data.frame(M)
View(M)
# The areas of circles or squares show the absolute value
#  of corresponding correlation coefficients. 
#    Color intensity and the size of the circle are proportional
#     to the correlation coefficients. 
# 3.3
corrplot(M)
#corrplot(M,cl.ratio=0.2, cl.align="r")
# 3.4
corrplot(M, method="circle", diag = F, addCoef.col = "grey")
# 3.5
corrplot(M, method="pie", diag = F, addCoef.col = "grey")
# 3.6
corrplot(M, type="upper")
corrplot.mixed(M)
# Reorder correlation matrix
# 3.7
corrplot(M, order ="AOE")
corrplot(M, order="hclust", addrect=2)

### Who is leaving?--------------

# 4. Filter only those who have left
hr_left <- hr %>% filter(left==1)
# 4.1 How many?
nrow(hr_left)

# 5. Let's create a data frame with only the people
#  that have left the company, so we can visualise
#   what is the distribution of each features:

# 5.1 Divide plot window in 1 X 3 part
#     R colour names http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
par(mfrow=c(1,3))
# 5.2
hist(hr_left$satisfaction_level,col="cadetblue4", main = "Satisfaction level") 
hist(hr_left$last_evaluation_score,col="cadetblue4", main = "Last evaluation")
hist(hr_left$average_montly_hours_worked,col="cadetblue4", main = "Average montly hours")

# 6. What about accidents and salary
#     Divide plot window in 1 X 2 parts
par(mfrow=c(1,2))
# 6.1
hist(hr_left$Work_accident,col="lightblue", main = "Work accident")
plot(hr_left$salary,col="lightblue", main = "Salary")

# From above plots We can see why we don't want to retain everybody.
#  Some people don't work well as we can see from their evaluation, 
#   but clearly there are also many good workers that leave.

### 7. Why good people leave?----------------

# 7.1 How many among those leaving are Good people
nrow(hr_left)
# 7.2
hr_good_leaving_people <- hr_left %>% filter(last_evaluation_score >= 0.70 | time_spend_company >= 4 | number_projects > 5)
nrow(hr_good_leaving_people)

# 7.3 Percentage of good people leaving
nrow(hr_good_leaving_people)/nrow(hr_left)

# 8. Let's analyse data of only the most valuable employees (whether left or NOT left) 
#   and see why they tend to leave.
# 8.1
hr_allgood_people <- hr %>% filter(last_evaluation_score >= 0.70 | time_spend_company >= 4 | number_projects > 5)
# 8.2
hr_allgood_people_select <- hr_allgood_people %>% select(satisfaction_level, number_projects: promotion_last_5years)
# 8.3
M <- cor(hr_allgood_people_select)

# 8.4 From below plot it's much clearer. On average valuable employees 
#  that 'left' are not satisfayed, work on many projects, spend
#   many hours in the company each month and aren't promoted.
corrplot(M, method="circle")
summary(hr_allgood_people)

### 9. From which dept people are leaving most ----------------

# 9.1 Order salary levels
#     Field 'sales' is a misnomer. It actually means 'departments'
hr$salary<-ordered(hr$salary,levels=c("low","medium","high"))

# Management Department, has the least attrition rate as it has a
#   higher proportion of highly paid employees. There is almost
#    no attrition in High Salary Paid Employees.

# 9.2. How are total employees distributed department wise  
plot_department<-ggplot(hr,aes(x=department))+geom_bar(fill="pink")+coord_flip()
plot_department
# 9.3 . How is salary-scale distributed (sort of histogram of salary)
plot_salary<-ggplot(hr,aes(x=salary))+geom_bar(fill="lightblue")+coord_flip()
plot_salary

# 9.4. Create a department-wise bar chart and show in stacked fashion, proportion of people left
# geom_bar(position="fill") makes it easier to compare proportions
plot_department_left<-ggplot(hr,aes(x=department,fill=as.factor(left)))+geom_bar(position="fill")+coord_flip() # +scale_fill_brewer(palette="PiYG")
plot_department_left
# 9.5. How is salary distributed between those who left and did not. Did high salaried people leave?
plot_salary_left<-ggplot(hr,aes(x=salary,fill=as.factor(left)))+geom_bar(position="fill")+coord_flip()+scale_fill_brewer(palette="PiYG")
plot_salary_left

# 9.6. Department wise, how salaried emploees are distributed. Does every dept have same ratio of low, medium and high
#    salaried people?
plot_salary_department<-ggplot(hr,aes(x=department,fill=salary))+geom_bar(position="fill")+scale_fill_brewer(palette="PiYG")+coord_flip()
plot_salary_department

# 9.7 Arrange all above plots in  a grid
grid.arrange(plot_department,plot_salary,plot_department_left,plot_salary_left,plot_salary_department,ncol=2)

### 10. Median satisfaction level for the employee who left the company-----------

# 10.1 The median, satisfaction level for employee who left the company are
#  lower compared to satisfies employees.
plot_sal_left_satisfaction<-ggplot(hr,aes(x=salary,y=satisfaction_level))+geom_boxplot()
plot_sal_left_satisfaction

################ FINISH ###################################
