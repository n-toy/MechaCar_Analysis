### Objective:
#      Predict MPG of MechaCar given our data set
#      Use a linear model using a number of variables within the dataset
### Details:
#  Which variables/coefficients provided a non-random amount of variance to the mpg values in the dataset?
#  Is the slope of the linear model considered to be zero? Why or why not?
#  Does this linear model predict mpg of MechaCar prototypes effectively? Why or why not?

#Clear environment
rm(list = ls())

#library import
library(tidyverse)

#read data
mecha_car = read.csv("~/Data Bootcamp/MechaCar_Analysis/DATA/MechaCar_mpg.csv",
                     stringsAsFactors = FALSE)
#View our data
head(mecha_car)

#Understand correlation of variables on mpg
correlation_mech = cor(mecha_car)
#Print the correlation of all variables on mpg
print(correlation_mech[6,])

#Lets create a multivariate linear model and see what variables have the greatest impact on MPG

mecha_lm = lm(mpg~ 
                vehicle.length + ground.clearance + AWD
              ,data = mecha_car)

print(summary(mecha_lm))
#vehicle.length,ground.clearance,AWD, and vehicle.weight seem to have the greatest effect

# spoiler.angle seem to have the least effect



### Objective:
#      create a summary statistics table for the suspension coil's pounds-per-inch continuous variable
### Details:
#   Be sure to include:
#     Mean
#     Median
#     Variance
#     Standard Deviation

#read data
suspension = read.csv("~/Data Bootcamp/MechaCar_Analysis/DATA/Suspension_coil.csv",
                     stringsAsFactors = FALSE)

#Group data by manufacturing lot
suspension_summary = suspension %>% group_by(Manufacturing_Lot) %>% summarize(mean(PSI), median(PSI),var(PSI),sd(PSI,na.rm=TRUE))
print(suspension_summary)



### Objective:
#      Determine if the suspensions coils PSI results are statistically different form the mean population results of 1,500 PSI
### Details:
#   Use the T-Test!

#Run the t-test with PSI and the true population mean being 1,500 (as told by module)
t.test(suspension$PSI,mu = 1500)











