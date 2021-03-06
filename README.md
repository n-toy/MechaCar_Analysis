# MechaCar_Analysis
MechaCar Write Up text file pasted below:



MPG Regression
===============================================================================

The MechaCar Analysis contains the below variables for each MechaCar prototype:
  Vehicle Length
  Vehicle Weight
  Spoiler Angle
  Ground Clearance
  AWD
  Miles per Gallon

Based off of the data provided about MechaCar prototypes the variables vehicle length, ground clearance, vehicle weight, and AWD are the most important factors for miles per gallon. There exists variables with a positive correlation to higher MPG, and lower MPG. 

Variables associated with higher MPG:
  Vehicle Length
  Vehicle Clearance
  Vehicle Weight
  
Variables associated with lower MPG:
  AWD

All variables except for spoiler angle seem to show a non-random amount of variance to the MPG values in the dataset. The slope of the multivariate linear model not be zero. This is because MPG is dependent heavily on vehicle length and ground clearance, and gives the linear model a non-zero slope. If MPG was not correlated with any variables in the dataset there would not be a successful linear model, and the result would be a linear model with a 0 slope. 

The Linear model boasts an effective R^2 value of 0.71 which shows a strong correlation of our variables predicting MPG. This shows that the variables of vehicle length, vehicle weight, ground clearance, and AWD are correlated with MPG gain/loss. 


Suspension Coil Summary
===============================================================================

Using the Suspension Coil Dataset, the PSI readings for types of suspension is analyzed and explored. For a given suspension from a manufacturing lot there is a PSI reading, and a vehicle ID associated with each record. The PSI readings are broken down into mean, median, variance, and standard variation for the three types of manufacturing lots.

The PSI readings for each type of manufacturing lot become:
---------------------------------------------------------------------------------

|Manufacturing_Lot|Average PSI| Median PSI| Variance PSI| Standard Deviation PSI|

|             Lot1|       1500|       1500|         1.15|                   1.07| 

|             Lot2|       1500|       1499|         10.1|                   3.18|

|             Lot3|       1499|       1498|         220.|                   14.8|

---------------------------------------------------------------------------------

Both Lot1 and Lot2 measurements show low variance and standard deviation. This means that for each vehicle suspension from each manufacturing lot has repeatable results--there is no variation/very little variation in the PSI readings for these two lots. However, Lot3 leaves much to be desired. Lot3 has very high variance and standard deviation in the PSI readings meaning that the PSI readings from Lot3 are sporadic and all over the place. 
If the design specifications for MechaCar suspension coils dictates that the variance of the suspension coils must not exceed 100 PSI, then Lot1 and Lot2 satisfy the design specifications. Lot3 does not meet the specifications because it has a variance of 220 PSI which exceeds the 100 PSI requirement. 




Suspension Coil T-Test
===============================================================================

The T-Test is used to understand if the suspension coil's PSI results are statistically different from the population mean of 1,500 PSI. The T-Test will allow us to compare the mean of our dataset to our presumed mean. For the T-Test it requires that we set up a null hypothesis and an alternative hypothesis. 

The null hypothesis is what will be assumed to be true, and the T-Test will give us the significance of our dataset mean compared to the presumed mean and whether there is a significant difference. The null hypothesis will be that there is no statistical difference between our dataset mean and the presumed mean of 1,500. The alternate hypothesis will be that there is a statistical difference between our dataset mean and the presumed mean of 1,500. 

Running the T-Test for our dataset yields a P-Value of 0.5117. To reject the null hypothesis the P-Value should be < 0.05. The results show that our dataset mean is NOT statistically different from the presumed population mean. Both means are statistically similar. 


Design Your Own Study--write a short description of a study that can quantify how the MechaCar outperforms the competition
===============================================================================

The MechaCar is slated to be AutoRUs' newest and greatest car in company history. Or so we think it will be. It is important to study similar vehicles to the MechaCar to understand competition at our price point, and type of car. To perform a study across competitors the data points that would be most impactful would be:
  Cost
  Fuel efficiency (MPG)
  Drivetrain (Gas/Electric)
  Transmission (if applicable)
  Vehicle weight
  Vehicle length
  Competing car sales (year to year)
  Competing car ratings (safety/consumer/efficiency)
  
  
MechaCar needs to offer something that is unique/different compared to competition. Does MechaCar shoot to make a unique vehicle with top-tier fuel efficiency, long wheel base, and ridiculously light?

It is possible to analyze the vehicle weight, length and fuel efficiency compared to competition. Our Null Hypothesis would be that MechaCar is similar to competition in the 3 factors. The alternate hypothesis for fuel efficiency would mean MechaCar is more/less efficient competition. The alternate hypothesis for vehicle length would mean that MechaCar is longer/shorter compared to competition. A Two-Sample T-Test allows us to compare two sample means versus each other to understand the statistical difference between each sample means. 
  
How about understanding similar cars and their sales/ratings? If the data on similar car sales year over year was available, we can see how each make/model of competitors has sold year over year. With this data we could run an Analysis of variance (ANOVA) test to compare the ratings/sales to see if there are certain make/models that perform better than others. The null hypothesis would be that all make/models of MechaCar competitors sell the same--the group means are all the same. The alternate hypothesis would be that some make/models perform statistically different compared to others. If the alternate hypothesis is accepted it would lead us to understand if a make/model is selling better than others and elements of design to consider, or if the make/model is selling worse than others it can be an example of what not to incorporate into MechaCar.





