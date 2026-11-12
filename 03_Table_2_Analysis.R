# ==============================================================================================================================================
# 
# Nutritional intelligence in FinnFoodPics
# created by Arsene Kanyamibwa (arsene.kanyamibwa@helsinki.fi) 
# Purpose: Check assumptions, run Welch t‑tests, and compute Welch-style Cohen's d.
# Script: Group comparisons (HPF vs non‑HPF) on nutritional and rating variables
# 
#
# created: 18.06.2025
# Last updated: 08.09.2026 (Familiarity and Freqeuncy statistics added)
# ============================================================================================================================================== 

#Clear environment
# Run this script in a fresh R session (RStudio: Session > Restart R).

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
# Portion
shapiro.test(food_data_HPF$Portion)
#W = 0.83872, p-value = 0.000558 #data is NOT normally distributed
shapiro.test(food_data_NHPF$Portion)
#W = 0.48411, p-value = 3.125e-11 #data is NOT normally distributed

#...............................................................................
# Protein
shapiro.test(food_data_HPF$Protein)
#W =  0.90107, p-value = 0.01212 #data is NOT normally distributed
shapiro.test(food_data_NHPF$Protein)
#W =  0.77354, p-value = 8.132e-07 #data is NOT normally distributed

#...............................................................................
# Fibre
shapiro.test(food_data_HPF$Fibre)
#W =  0.60113, p-value = 1.498e-07 #data is NOT normally distributed
shapiro.test(food_data_NHPF$Fibre)
#W =  0.71023, p-value = 5.278e-08 #data is NOT normally distributed

#...............................................................................
# Price
shapiro.test(food_data_HPF$Actual_price)
#W =  0.81994, p-value = 0.0002449 #data is NOT normally distributed
shapiro.test(food_data_NHPF$Actual_price)
#W =  0.71157, p-value = 5.571e-08 #data is NOT normally distributed
#...............................................................................
# Liking
shapiro.test(food_data_HPF$Liking)
#W =  0.98053, p-value = 0.8629 #data is normally distributed
shapiro.test(food_data_NHPF$Liking)
#W = 0.97611, p-value = 0.486 #data is normally distributed

#...............................................................................
# Familiarity 
shapiro.test(food_data_HPF$Familiarity)
#W =  0.97904, p-value = 0.8268 #data is normally distributed
shapiro.test(food_data_NHPF$Familiarity)
#W = 0.90885, p-value = 0.00206 #data is NOT normally distributed

#...............................................................................
# Frequency of consumption
shapiro.test(food_data_HPF$Frequency_of_consumption)
#W = 0.85706, p-value = 0.001302  #data is NOT normally distributed
shapiro.test(food_data_NHPF$Frequency_of_consumption)
#W = 0.89987, p-value = 0.001078  #data is NOT normally distributed
#...............................................................................
# Carbohydrate
shapiro.test(food_data_HPF$Carbohydrate)
# W = 0.91197, p-value = 0.02207  #data is NOT normally distributed
shapiro.test(food_data_NHPF$Carbohydrate)
# W = 0.88207, p-value = 0.0003201  #data is NOT normally distributed

#...............................................................................
# Fat
shapiro.test(food_data_HPF$Fat)
# W = 0.97063, p-value = 0.5976  #data is normally distributed
shapiro.test(food_data_NHPF$Fat)
# W = 0.8748, p-value = 0.000199  #data is NOT normally distributed

#...............................................................................
# Fibre
shapiro.test(food_data_HPF$Fibre)
# W = 0.60113, p-value = 1.498e-07  #data is NOT normally distributed
shapiro.test(food_data_NHPF$Fibre)
# W = 0.71023, p-value = 5.278e-08  #data is NOT normally distributed

#...............................................................................
# Sugars
shapiro.test(food_data_HPF$Sugars)
# W = 0.88426, p-value = 0.004993  #data is NOT normally distributed
shapiro.test(food_data_NHPF$Sugars)
# # W = 0.8748, p-value = 0.000199  #data is NOT normally distributed


# Most variables show clear deviations from normality in at least one group.

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

##           Test of Equality of Variances (Levene's test)

## We use center = median → Brown–Forsythe test: more robust to non-normality (DEFAULT)
# Portion
leveneTest(Portion ~ Palatability, data = food_data)
#      Df F value Pr(>F)
#group  1  0.0084 0.9274 # Equal variance
#      70 
#...............................................................................
# Protein
leveneTest(Protein ~ Palatability, data = food_data)
#      Df F value Pr(>F)
#group  1  2.4115  0.125 # Equal variance
#      70 
#...............................................................................
# Fibre
leveneTest(Fibre ~ Palatability, data = food_data)
#      Df F value Pr(>F)  
#group  1  4.3933 0.0397 * # Variance NOT Equal
#      70
#...............................................................................
# Price
leveneTest(Actual_price ~ Palatability, data = food_data)
#      Df F value  Pr(>F)  
#group  1  4.8458 0.03101 *  # Variance NOT Equal
#      70 
#...............................................................................
# Liking
leveneTest(Liking ~ Palatability, data = food_data)
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
#group  1  1.0628 0.3061 # Equal Variance
#      70
#...............................................................................
# Familiarity
leveneTest(Familiarity ~ Palatability, data = food_data)
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
#group  1  1.4064 0.2397  # Equal variance
#       70  
#...............................................................................
# Frequency of consumption
leveneTest(Frequency_of_consumption ~ Palatability, data = food_data)
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value  Pr(>F)  
#group  1   3.027 0.08629 . # Equal variance (marginal)
#      70  
#...............................................................................
#...............................................................................
# Carbohydrate
leveneTest(Carbohydrate ~ Palatability, data = food_data)
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value  Pr(>F)  
#group  1  4.2392 0.04322 * # Unequal variance
#       70  
#...............................................................................
# Fat
leveneTest(Fat ~ Palatability, data = food_data)
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
#group  1  2.4675 0.1207  # Equal variance
#       70               
#...............................................................................
# Sugars
leveneTest(Sugars ~ Palatability, data = food_data)
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
#group  1  0.1225 0.7274  # Equal variance
#       70               
#...............................................................................
# Sodium
leveneTest(Sodium ~ Palatability, data = food_data)
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value    Pr(>F)    
#group  1  11.825 0.0009885 *** # Unequal variance
#       70                      

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##             Analysis shown in Table 2 (Welch t-test)
## Given the absence of normality and several unequal variances we chose to conduct a  Welch's t-test
## for all variables as it can handle non normal variables better than Mann Whitney can handle unequal variances

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
#    0.3742046             1.0528211 
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
#  alternative hypothesis: true difference in means between group HPF and group Not HPF is not equal to 0
#  95 percent confidence interval:
#  -3.010512  2.459244
#sample estimates:
#mean in group HPF mean in group Not HPF 
#         59.45017              59.72581 

#...............................................................................
#Familiarity
t.test(Familiarity ~ Palatability, data = food_data)
# t = -1.2809, df = 51.189, p-value = 0.206
# alternative hypothesis: true difference in means between group HPF and group Not HPF is not equal to 0
# 95 percent confidence interval:
#   -7.344141  1.622545
# sample estimates:
#  mean in group HPF   mean in group Not HPF 
#    70.74389                73.60469 
#...............................................................................
#Frequency of consumption 
t.test(Frequency_of_consumption ~ Palatability, data = food_data)
# t = -1.8869, df = 68.841, p-value = 0.06339
# alternative hypothesis: true difference in means between group HPF and group Not HPF is not equal to 0
# 95 percent confidence interval:
#  -2.12592872  0.05921318
#sample estimates:
#  mean in group HPF mean in group Not HPF 
# 2.443548              3.476906 
#...............................................................................
# Carbs
t.test(Carbohydrate ~ Palatability, data = food_data)
# t = -1.7132, df = 68.151, p-value = 0.09122
# CI -7.9431668  0.6043257, 
# Mean -> HPF = 11.62400  NHPF = 15.29342 
#...............................................................................
# Fat
t.test(Fat ~ Palatability, data = food_data)
# t = 2.8274, df = 63.4, p-value = 0.006272
# CI 0.6854521 3.9884335
# Mean -> HPF =  6.663842  NHPF =  4.326899
#...............................................................................
# Sugars
t.test(Sugars ~ Palatability, data = food_data)
# t = 0.11384, df = 69.985, p-value = 0.9097
# CI -3.228746  3.619659
# Mean -> HPF =  6.086964  NHPF =  5.891507 
#...............................................................................
# Sodium
t.test(Sodium ~ Palatability, data = food_data)
# t = 2.5222, df = 30.88, p-value = 0.01703
# CI  0.02209377 0.20894296
# Mean -> HPF =  0.21250322    NHPF =  0.09698485 

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#Cohen's D calculation was replicated on JASP software version 0.17.3.0.

#Cohen.d function uses the weighted pooled variance but we wanted the Unpooled SD (Welch-style / average-variance meaning each group’s variance is separate
#So we got different values for effect size when using the formula and comparing it with JASP.
#Therefore we manually calculated unpooled SD (unpooled SD, matches JASP unequal-variance d)
# d = (M1 - M2) / sqrt((sd1^2 + sd2^2) / 2)

## Welch test effect size 
# Portion
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
# [1] -0.0769817

#...............................................................................
# Protein
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
# [1] -0.5269068

#...............................................................................

# Fibre
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
# [1] -0.543616

#...............................................................................
# Price
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
# [1] -0.2975381

#...............................................................................
# Liking
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
# [1] -0.04956615

#...............................................................................
# Familiarity 
#Welch test effect size 
{
  x <- food_data_HPF$Familiarity
  y <- food_data_NHPF$Familiarity
  mean_x <- mean(x)
  mean_y <- mean(y)
  sd_x   <- sd(x)
  sd_y   <- sd(y)
}
# "Welch-style" Cohen's d (same as JASP's unequal-variance version)
d_welch <- (mean_x - mean_y) / sqrt((sd_x^2 + sd_y^2) / 2)
d_welch
# [1] -0.3148276

#...............................................................................
# Frequency of consumption
#Welch test effect size 
{
  x <- food_data_HPF$Frequency_of_consumption
  y <- food_data_NHPF$Frequency_of_consumption
  mean_x <- mean(x)
  mean_y <- mean(y)
  sd_x   <- sd(x)
  sd_y   <- sd(y)
}
# "Welch-style" Cohen's d (same as JASP's unequal-variance version)
d_welch <- (mean_x - mean_y) / sqrt((sd_x^2 + sd_y^2) / 2)
d_welch
# [1] -0.4397802

#...............................................................................
#   Carbohydrate
#Welch test effect size 
{
  x <- food_data_HPF$Carbohydrate
  y <- food_data_NHPF$Carbohydrate
  mean_x <- mean(x)
  mean_y <- mean(y)
  sd_x   <- sd(x)
  sd_y   <- sd(y)
}
# "Welch-style" Cohen's d (same as JASP's unequal-variance version)
d_welch <- (mean_x - mean_y) / sqrt((sd_x^2 + sd_y^2) / 2)
d_welch
# -0.4007859

#...............................................................................
# Fat
#Welch test effect size 
{
  x <- food_data_HPF$Fat
  y <- food_data_NHPF$Fat
  mean_x <- mean(x)
  mean_y <- mean(y)
  sd_x   <- sd(x)
  sd_y   <- sd(y)
}
# "Welch-style" Cohen's d (same as JASP's unequal-variance version)
d_welch <- (mean_x - mean_y) / sqrt((sd_x^2 + sd_y^2) / 2)
d_welch
#  0.6728515

#...............................................................................
# Sugars
#Welch test effect size 
{
  x <- food_data_HPF$Sugars
  y <- food_data_NHPF$Sugars
  mean_x <- mean(x)
  mean_y <- mean(y)
  sd_x   <- sd(x)
  sd_y   <- sd(y)
}
# "Welch-style" Cohen's d (same as JASP's unequal-variance version)
d_welch <- (mean_x - mean_y) / sqrt((sd_x^2 + sd_y^2) / 2)
d_welch
# 0.02621674

#...............................................................................
# Sodium
#Welch test effect size 
{
  x <- food_data_HPF$Sodium
  y <- food_data_NHPF$Sodium
  mean_x <- mean(x)
  mean_y <- mean(y)
  sd_x   <- sd(x)
  sd_y   <- sd(y)
}
# "Welch-style" Cohen's d (same as JASP's unequal-variance version)
d_welch <- (mean_x - mean_y) / sqrt((sd_x^2 + sd_y^2) / 2)
d_welch
# 0.661655

#:::::::::::::::::::::::::::::::::THE END:::::::::::::::::::::::::::::::::::::::














