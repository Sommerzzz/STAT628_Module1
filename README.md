# STAT628_Module1: Body Fat Calculation

## Cheng Lu, Wenyan Zhou, Zong Guo

The project is to find a simple, robust and accurate way to measure body fat percentage of males using clinically available measurements. Module1_summary.ipynb is the executive summary of our data analysis which provide a concise, replicable and clear description of our statistical analysis and findings. There are three folders providing more details about analysis.

**Code folder**: "m1.R" contains the code used to remove the outliers, perform different ways (AIC, BIC, Mallow's Cp, Lasso) to do variable selection, draw the diagnostic plot. 

**Image folder**: Any images produced during analysis is included in this folder.

**Data folder**: The are 252 observations with 17 variables in the raw data set called BodyFat.csv.
Percent body fat from Siri's (1956) equation  
Density determined from underwater weighing  
Age (years)  
Weight (lbs)  
Height (inches)  
Adioposity (bmi)
Neck circumference (cm)  
Chest circumference (cm)  
Abdomen 2 circumference (cm)  
Hip circumference (cm)  
Thigh circumference (cm)  
Knee circumference (cm)  
Ankle circumference (cm)  
Biceps (extended) circumference (cm)  
Forearm circumference (cm)  
Wrist circumference (cm)  
BodyFat_clean.csv is a file contains data with six outliers removed.
