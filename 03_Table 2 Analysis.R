# ==============================================================================================================================================
# 
# Nutritional intelligence in FinnFoodPics
# created by Arsene Kanyamibwa (arsene.kanyamibwa@helsinki.fi) 
# Purpose: Check assumptions, run Welch t‑tests, and compute Welch-style Cohen's d.
# Script: Group comparisons (HPF vs non‑HPF) on nutritional and rating variables
# 
#
# created: 18.06.2025
# last updated: 07.10.2025 
# ============================================================================================================================================== 

#Clear environment
rm(list=ls())

# Install packages once (comment out during normal use)
# install.packages(c("tidyverse", "car", "effsize"))

# Load libraries used in this script
{
library(tidyverse)  # data handling and piping
library(car)        # Levene's test for homogeneity of variance
library(effsize)    # standard effect size functions (not used for Welch d here)
}

# Set working directory and load data
# Windows path (lab server)
setwd("P:/h345/obrain_labspace/Projects/PhD_projects/MARVEL/02_MARVEL I_Macronutrient study/03_Experiment/Aim 1/FoodRating task/04_Analysis")

# Mac path – uncomment when running on Mac
# setwd("/Volumes/h345/obrain_labspace/Projects/PhD_projects/MARVEL/02_MARVEL I_Macronutrient study/03_Experiment/Aim 1/FoodRating task/04_Analysis")

food_data <- read.csv("0_Complete_pictureset_data.csv")

# Drop first column (index column created when exporting from R/Excel)
food_data <- food_data[, -1]

# Convert categorical variables to factors for downstream analyses
{
food_data$Category       <- factor(food_data$Category)
food_data$Palatability   <- factor(food_data$Palatability)
food_data$NOVA_Group     <- factor(food_data$NOVA_Group)
food_data$Energy_levels  <- factor(food_data$Energy_levels)
food_data$Energy_levels2 <- factor(food_data$Energy_levels2)
food_data$NOVA_Group2    <- factor(food_data$NOVA_Group2)
}
#Set explicit factor order for energy levels (used in plotting / modelling)
{
food_data$Energy_levels <- factor(
  food_data$Energy_levels,
  levels = c("HED", "MED", "LED")
)
food_data$Energy_levels2 <- factor(
  food_data$Energy_levels2,
  levels = c("VHED", "HED", "MED", "LED", "VLED")
)
}
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Split data by palatability category
food_data_HPF  <- food_data[food_data$Palatability == "HPF", ]
food_data_NHPF <- food_data[food_data$Palatability == "Not HPF", ]

###Assumptions check 
#Normality tests (Shapiro-Wilk)
#Portion
shapiro.test(food_data_HPF$Portion)
#W = 0.83872, p-value = 0.000558 #data is NOT normally distributed
shapiro.test(food_data_NHPF$Portion)
#W = 0.48411, p-value = 3.125e-11 #data is NOT normally distributed

#...............................................................................
#Protein
shapiro.test(food_data_HPF$Protein)
#W =  0.90107, p-value = 0.01212 #data is NOT normally distributed
shapiro.test(food_data_NHPF$Protein)
#W =  0.77354, p-value = 8.132e-07 #data is NOT normally distributed

#...............................................................................
#Fibre
shapiro.test(food_data_HPF$Fibre)
#W =  0.60113, p-value = 1.498e-07 #data is NOT normally distributed
shapiro.test(food_data_NHPF$Fibre)
#W =  0.71023, p-value = 5.278e-08 #data is NOT normally distributed

#...............................................................................
#Price
shapiro.test(food_data_HPF$Actual_price)
#W =  0.81994, p-value = 0.0002449 #data is NOT normally distributed
shapiro.test(food_data_NHPF$Actual_price)
#W =  0.71157, p-value = 5.571e-08 #data is NOT normally distributed
#...............................................................................

#Liking
shapiro.test(food_data_HPF$Liking)
#W =  0.98053, p-value = 0.8629 #data is normally distributed
shapiro.test(food_data_NHPF$Liking)
#W = 0.97611, p-value = 0.486 #data is normally distributed

# Most variables show clear deviations from normality in at least one group.

#...............................................................................
#Test of Equality of Variances (Levene's test)
#We use center = median → Brown–Forsythe test: more robust to non-normality (DEFAULT)
leveneTest(Portion ~ Palatability, data = food_data)
#      Df F value Pr(>F)
#group  1  0.0084 0.9274 Equal variance
#      70 
#...............................................................................

leveneTest(Protein ~ Palatability, data = food_data)
#      Df F value Pr(>F)
#group  1  2.4115  0.125 Equal variance
#      70 
#...............................................................................

leveneTest(Fibre ~ Palatability, data = food_data)
#      Df F value Pr(>F)  
#group  1  4.3933 0.0397 * Variance NOT Equal
#      70
#...............................................................................

leveneTest(Actual_price ~ Palatability, data = food_data)
#      Df F value  Pr(>F)  
#group  1  4.8458 0.03101 *  Variance NOT Equal
#      70 
#...............................................................................

leveneTest(Liking ~ Palatability, data = food_data)
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
#group  1  1.0628 0.3061 Equal Variance
#      70

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Analysis shown in Table 2 (Welch t-test)
#Given the absence of normality and several unequal variances we chose to conduct a  Welch's t-test
#for all variables as it can handle non normal  variables better than Mann Whitney can handle unequal variances

##Portion
t.test(Portion ~ Palatability, data = food_data)
#t = -0.33795, df = 69.414, p-value = 0.7364
#alternative hypothesis: true difference in means between group HPF and group Not HPF is not equal to 0
#95 percent confidence interval:
#  -14.20695  10.09046
#sample estimates:
#  mean in group HPF mean in group Not HPF 
#39.52107              41.57932 
#...............................................................................

##Protein
t.test(Protein ~ Palatability, data = food_data) 
#t = -2.3488, df = 66.009, p-value = 0.02184
#alternative hypothesis: true difference in means between group HPF and group Not HPF is not equal to 0
#95 percent confidence interval:
# -2.998174 -0.243057
#sample estimates:
#    mean in group HPF mean in group Not HPF 
#             2.669526              4.290141
#...............................................................................

#Fibre
t.test(Fibre ~ Palatability, data = food_data)
#t = -2.4345, df = 64.469, p-value = 0.01769
#alternative hypothesis: true difference in means between group HPF and group Not HPF is not equal to 0
#95 percent confidence interval:
#  -1.2354143 -0.1218187
#sample estimates:
#  mean in group HPF mean in group Not HPF 
#0.3742046             1.0528211 
#...............................................................................

#Price
t.test(Actual_price ~ Palatability, data = food_data)
#t = -1.3602, df = 55.567, p-value = 0.1793
#alternative hypothesis: true difference in means between group HPF and group Not HPF is not equal to 0
#95 percent confidence interval:
#  -0.26662189  0.05099589
#sample estimates:
#  mean in group HPF mean in group Not HPF 
#0.3609873             0.4688003
#...............................................................................
#Liking
t.test(Liking ~ Palatability, data = food_data)
#t = -0.20222, df = 52.245, p-value = 0.8405
#alternative hypothesis: true difference in means between group HPF and group Not HPF is not equal to 0
#95 percent confidence interval:
#  -3.010512  2.459244
#sample estimates:
#mean in group HPF mean in group Not HPF 
#         59.45017              59.72581 

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Cohen's D calculation was replicated on JASP software version 0.17.3.0.

#Cohen.d function uses the weighted pooled variance but we wanted the Unpooled SD (Welch-style / average-variance meaning each group’s variance is separate
#So we got different values for effect size when using the formula and comparing it with JASP.
#Therefore we manually calculated unpooled SD (unpooled SD, matches JASP unequal-variance d)
# d = (M1 - M2) / sqrt((sd1^2 + sd2^2) / 2)


#Welch test effect size 
{
x <- food_data_HPF$Portion
y <- food_data_NHPF$Portion
mean_x <- mean(x)
mean_y <- mean(y)
sd_x   <- sd(x)
sd_y   <- sd(y)
}
# "Welch-style" Cohen's d (same as JASP's unequal-variance version)
d_welch <- (mean_x - mean_y) / sqrt((sd_x^2 + sd_y^2) / 2)
d_welch
#[1] -0.0769817
#...............................................................................

#Protein
#Welch test effect size 
{
x <- food_data_HPF$Protein
y <- food_data_NHPF$Protein
mean_x <- mean(x)
mean_y <- mean(y)
sd_x   <- sd(x)
sd_y   <- sd(y)
}
# "Welch-style" Cohen's d (same as JASP's unequal-variance version)
d_welch <- (mean_x - mean_y) / sqrt((sd_x^2 + sd_y^2) / 2)
d_welch
#[1] -0.5269068
#...............................................................................

#Fibre
#Welch test effect size 
{
x <- food_data_HPF$Fibre
y <- food_data_NHPF$Fibre
mean_x <- mean(x)
mean_y <- mean(y)
sd_x   <- sd(x)
sd_y   <- sd(y)
}
# "Welch-style" Cohen's d (same as JASP's unequal-variance version)
d_welch <- (mean_x - mean_y) / sqrt((sd_x^2 + sd_y^2) / 2)
d_welch
#[1] -0.543616
#...............................................................................

#Price
#Welch test effect size 
{
x <- food_data_HPF$Actual_price
y <- food_data_NHPF$Actual_price
mean_x <- mean(x)
mean_y <- mean(y)
sd_x   <- sd(x)
sd_y   <- sd(y)
}
# "Welch-style" Cohen's d (same as JASP's unequal-variance version)
d_welch <- (mean_x - mean_y) / sqrt((sd_x^2 + sd_y^2) / 2)
d_welch
#[1] -0.2975381
#...............................................................................

#Liking
#Welch test effect size 
{
x <- food_data_HPF$Liking
y <- food_data_NHPF$Liking
mean_x <- mean(x)
mean_y <- mean(y)
sd_x   <- sd(x)
sd_y   <- sd(y)
}
# "Welch-style" Cohen's d (same as JASP's unequal-variance version)
d_welch <- (mean_x - mean_y) / sqrt((sd_x^2 + sd_y^2) / 2)
d_welch
#[1] -0.04956615

#:::::::::::::::::::::::::::::::::THE END:::::::::::::::::::::::::::::::::::::::
