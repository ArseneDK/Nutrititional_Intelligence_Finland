# ==============================================================================
# 
# Nutritional intelligence in FinnFoodPics
# created by Arsene Kanyamibwa (arsene.kanyamibwa@helsinki.fi), and reviewed by Sherwin NJ. (sherwin.nedaeijanbesaraei@helsinki.fi).
# Purpose: Check assumptions, run Welch t‑tests,
# and compute Welch-style Cohen's d.
# Script: Group comparisons (HPF vs non‑HPF) on nutritional and rating variables
# 
#
# created: 18.06.2025
# updated: 08.09.2026 (Familiarity and Freqeuncy statistics added)
# Updated: 16.11.2026, comments add, light refactoring for clearity, Sherwin NJ.
# ==============================================================================

# Run this script in a fresh R session (RStudio: Session > Restart R).

# 
#   #:::::  Major numbered sections
#   #=====  Numbered subsections
#   #-----  Output separators within a subsection
# Plain comments describe individual commands or interpretation

#::::::::::::::::::::::::::::::::::: 1. SETUP ::::::::::::::::::::::::::::::::::
#============================== 1.1 PACKAGE SETUP ==============================
{
required_packages <- c("tidyverse", "car", "effsize", "here")

missing_packages <- required_packages[
 !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
 install.packages(
   missing_packages,
   repos = "https://cloud.r-project.org"
 )
}

invisible(
 lapply(required_packages, library, character.only = TRUE)
)
}

#========================= 1.2 DATA IMPORT AND CLEANING ========================

{
 # Import data from the repository-relative data directory.
 # Drop the first column, which is an export index column.
 food_data <- read.csv(
   here("data", "0_Complete_pictureset_data.csv")
 )[, -1]

 # Convert categorical variables to factors for downstream analyses.
 # Set explicit factor order for energy levels used in modelling.
 food_data <- food_data |>
   mutate(
     Category       = factor(Category),
     Palatability   = factor(Palatability),
     NOVA_Group     = factor(NOVA_Group),
     NOVA_Group2    = factor(NOVA_Group2),
     Energy_levels  = factor(
       Energy_levels,
       levels = c("HED", "MED", "LED")
     ),
     Energy_levels2 = factor(
       Energy_levels2,
       levels = c("VHED", "HED", "MED", "LED", "VLED")
     )
   )
}

#========================== 1.3 SUBSET BY PALATABILITY =========================
food_data_HPF  <- food_data[food_data$Palatability == "HPF", ]
food_data_NHPF <- food_data[food_data$Palatability == "Not HPF", ]

#::::::::::::::::::::::::::::: 2. ASSUMPTION CHECKS ::::::::::::::::::::::::::::
#============================= 2.1 NORMALITY CHECKS ============================
# Shapiro-Wilk normality tests for each variable and palatability group.
# Results with p < .05 indicate a significant deviation from normality.
#================================ 2.1.1 PORTION ================================
shapiro.test(food_data_HPF$Portion)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_HPF$Portion
# W = 0.83872, p-value = 0.000558
# -> data is NOT normally distributed
#-------------------------------------------------------------------------------

shapiro.test(food_data_NHPF$Portion)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_NHPF$Portion
# W = 0.48411, p-value = 3.125e-11
# -> data is NOT normally distributed
#-------------------------------------------------------------------------------


#================================ 2.1.2 PROTEIN ================================
shapiro.test(food_data_HPF$Protein)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_HPF$Protein
# W = 0.90107, p-value = 0.01212
# -> data is NOT normally distributed
shapiro.test(food_data_NHPF$Protein)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_NHPF$Protein
# W = 0.77354, p-value = 8.132e-07
# -> data is NOT normally distributed
#-------------------------------------------------------------------------------


#================================= 2.1.3 FIBRE =================================
shapiro.test(food_data_HPF$Fibre)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_HPF$Fibre
# W = 0.60113, p-value = 1.498e-07
# -> data is NOT normally distributed
shapiro.test(food_data_NHPF$Fibre)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_NHPF$Fibre
# W = 0.71023, p-value = 5.278e-08
# -> data is NOT normally distributed
#-------------------------------------------------------------------------------


#================================= 2.1.4 PRICE =================================
shapiro.test(food_data_HPF$Actual_price)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_HPF$Actual_price
# W = 0.81994, p-value = 0.0002449
# -> data is NOT normally distributed
shapiro.test(food_data_NHPF$Actual_price)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_NHPF$Actual_price
# W = 0.71157, p-value = 5.571e-08
# -> data is NOT normally distributed
#-------------------------------------------------------------------------------


#================================= 2.1.5 LIKING ================================
shapiro.test(food_data_HPF$Liking)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_HPF$Liking
# W = 0.98053, p-value = 0.8629
# -> data is normally distributed
shapiro.test(food_data_NHPF$Liking)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_NHPF$Liking
# W = 0.97611, p-value = 0.486
# -> data is normally distributed
#-------------------------------------------------------------------------------


#============================== 2.1.6 FAMILIARITY ==============================
shapiro.test(food_data_HPF$Familiarity)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_HPF$Familiarity
# W = 0.97904, p-value = 0.8268
# -> data is normally distributed
shapiro.test(food_data_NHPF$Familiarity)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_NHPF$Familiarity
# W = 0.90885, p-value = 0.00206
# -> data is NOT normally distributed
#-------------------------------------------------------------------------------


#======================== 2.1.7 FREQUENCY OF CONSUMPTION =======================
shapiro.test(food_data_HPF$Frequency_of_consumption)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_HPF$Frequency_of_consumption
# W = 0.85706, p-value = 0.001302
# -> data is NOT normally distributed
shapiro.test(food_data_NHPF$Frequency_of_consumption)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_NHPF$Frequency_of_consumption
# W = 0.89987, p-value = 0.001078
# -> data is NOT normally distributed
#-------------------------------------------------------------------------------


#============================== 2.1.8 CARBOHYDRATE =============================
shapiro.test(food_data_HPF$Carbohydrate)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_HPF$Carbohydrate
# W = 0.91197, p-value = 0.02207
# -> data is NOT normally distributed
shapiro.test(food_data_NHPF$Carbohydrate)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_NHPF$Carbohydrate
# W = 0.88207, p-value = 0.0003201
# -> data is NOT normally distributed
#-------------------------------------------------------------------------------


#================================== 2.1.9 FAT ==================================
shapiro.test(food_data_HPF$Fat)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_HPF$Fat
# W = 0.97063, p-value = 0.5976
# -> data is normally distributed
shapiro.test(food_data_NHPF$Fat)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_NHPF$Fat
# W = 0.8748, p-value = 0.0001995
# -> data is NOT normally distributed
#-------------------------------------------------------------------------------

#================================= 2.1.10 FIBRE ================================
# Fibre
shapiro.test(food_data_HPF$Fibre)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_HPF$Fibre
# W = 0.60113, p-value = 1.498e-07
# -> data is NOT normally distributed
shapiro.test(food_data_NHPF$Fibre)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_NHPF$Fibre
# W = 0.71023, p-value = 5.278e-08
# -> data is NOT normally distributed

#-------------------------------------------------------------------------------
#================================ 2.1.11 SUGARS ================================
# Sugars
shapiro.test(food_data_HPF$Sugars)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_HPF$Sugars
# W = 0.88426, p-value = 0.004993
# -> data is NOT normally distributed
shapiro.test(food_data_NHPF$Sugars)
#------------------------------------ OUTPUT -----------------------------------
#        Shapiro-Wilk normality test
#
# data:  food_data_NHPF$Sugars
# W = 0.68929, p-value = 2.321e-08
# -> data is NOT normally distributed


# Most variables show clear deviations from normality in at least one group.

#-------------------------------------------------------------------------------

#========================= 2.2 HOMOGENEITY OF VARIANCE =========================
# Levene's test uses the median centre (Brown-Forsythe variant), which is
# more robust to non-normality.
#================================ 2.2.1 PORTION ================================
leveneTest(Portion ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
#      Df F value Pr(>F)
#group  1  0.0084 0.9274 # Equal variance
#      70
#-------------------------------------------------------------------------------
#================================ 2.2.2 PROTEIN ================================
leveneTest(Protein ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
#      Df F value Pr(>F)
#group  1  2.4115  0.125 # Equal variance
#      70
#-------------------------------------------------------------------------------
#================================= 2.2.3 FIBRE =================================
leveneTest(Fibre ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
#      Df F value Pr(>F)
#group  1  4.3933 0.0397 * # Variance NOT Equal
#      70
#-------------------------------------------------------------------------------
#================================= 2.2.4 PRICE =================================
leveneTest(Actual_price ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
#      Df F value  Pr(>F)
#group  1  4.8458 0.03101 *  # Variance NOT Equal
#      70
#-------------------------------------------------------------------------------
#================================= 2.2.5 LIKING ================================
leveneTest(Liking ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
#group  1  1.0628 0.3061 # Equal Variance
#      70
#-------------------------------------------------------------------------------
#============================== 2.2.6 FAMILIARITY ==============================
leveneTest(Familiarity ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
#group  1  1.4064 0.2397  # Equal variance
#       70
#-------------------------------------------------------------------------------
#======================== 2.2.7 FREQUENCY OF CONSUMPTION =======================
leveneTest(Frequency_of_consumption ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value  Pr(>F)
#group  1   3.027 0.08629 . # Equal variance (marginal)
#      70
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#============================== 2.2.8 CARBOHYDRATE =============================
leveneTest(Carbohydrate ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value  Pr(>F)
#group  1  4.2392 0.04322 * # Unequal variance
#       70
#-------------------------------------------------------------------------------
#================================== 2.2.9 FAT ==================================
leveneTest(Fat ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
#group  1  2.4675 0.1207  # Equal variance
#       70
#-------------------------------------------------------------------------------
#================================ 2.2.10 SUGARS ================================
leveneTest(Sugars ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
#group  1  0.1225 0.7274  # Equal variance
#       70
#-------------------------------------------------------------------------------
#================================ 2.2.11 SODIUM ================================
leveneTest(Sodium ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value    Pr(>F)
#group  1  11.825 0.0009885 *** # Unequal variance
#       70

#::::::::::::::::::::::::::::::: 3. WELCH T-TESTS ::::::::::::::::::::::::::::::
# Welch's t-tests are used for all variables because they do not assume equal
# variances and are appropriate under the observed non-normality.

#================================= 3.1 PORTION =================================
t.test(Portion ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
#t = -0.33795, df = 69.414, p-value = 0.7364
# alternative hypothesis: true difference in means
# between group HPF and group Not HPF is not equal to 0
#95 percent confidence interval:
#  -14.20695  10.09046
#sample estimates:
#  mean in group HPF mean in group Not HPF
#39.52107              41.57932
#-------------------------------------------------------------------------------

#================================= 3.2 PROTEIN =================================
t.test(Protein ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
#t = -2.3488, df = 66.009, p-value = 0.02184
# alternative hypothesis: true difference in means
# between group HPF and group Not HPF is not equal to 0
#95 percent confidence interval:
# -2.998174 -0.243057
#sample estimates:
#    mean in group HPF mean in group Not HPF
#             2.669526              4.290141
#-------------------------------------------------------------------------------

#================================== 3.3 FIBRE ==================================
t.test(Fibre ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
#t = -2.4345, df = 64.469, p-value = 0.01769
# alternative hypothesis: true difference in means
# between group HPF and group Not HPF is not equal to 0
#95 percent confidence interval:
#  -1.2354143 -0.1218187
#sample estimates:
#  mean in group HPF mean in group Not HPF
#    0.3742046             1.0528211
#-------------------------------------------------------------------------------

#================================== 3.4 PRICE ==================================
t.test(Actual_price ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
#t = -1.3602, df = 55.567, p-value = 0.1793
# alternative hypothesis: true difference in means
# between group HPF and group Not HPF is not equal to 0
#95 percent confidence interval:
#  -0.26662189  0.05099589
#sample estimates:
#  mean in group HPF mean in group Not HPF
#0.3609873             0.4688003
#-------------------------------------------------------------------------------
#================================== 3.5 LIKING =================================
t.test(Liking ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
# t = -0.20222, df = 52.245, p-value = 0.8405
#  alternative hypothesis: true difference in means
# between group HPF and group Not HPF is not equal to 0
#  95 percent confidence interval:
#  -3.010512  2.459244
#sample estimates:
#mean in group HPF mean in group Not HPF
#         59.45017              59.72581

#-------------------------------------------------------------------------------
#=============================== 3.6 FAMILIARITY ===============================
t.test(Familiarity ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
# t = -1.2809, df = 51.189, p-value = 0.206
# alternative hypothesis: true difference in means
# between group HPF and group Not HPF is not equal to 0
# 95 percent confidence interval:
#   -7.344141  1.622545
# sample estimates:
#  mean in group HPF   mean in group Not HPF
#    70.74389                73.60469
#-------------------------------------------------------------------------------
#========================= 3.7 FREQUENCY OF CONSUMPTION ========================
t.test(Frequency_of_consumption ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
# t = -1.8869, df = 68.841, p-value = 0.06339
# alternative hypothesis: true difference in means
# between group HPF and group Not HPF is not equal to 0
# 95 percent confidence interval:
#  -2.12592872  0.05921318
#sample estimates:
#  mean in group HPF mean in group Not HPF
# 2.443548              3.476906
#-------------------------------------------------------------------------------
#=============================== 3.8 CARBOHYDRATE ==============================
t.test(Carbohydrate ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
# t = -1.7132, df = 68.151, p-value = 0.09122
# CI -7.9431668  0.6043257,
# Mean -> HPF = 11.62400  NHPF = 15.29342
#-------------------------------------------------------------------------------
#=================================== 3.9 FAT ===================================
t.test(Fat ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
# t = 2.8274, df = 63.4, p-value = 0.006272
# CI 0.6854521 3.9884335
# Mean -> HPF =  6.663842  NHPF =  4.326899
#-------------------------------------------------------------------------------
#================================= 3.10 SUGARS =================================
t.test(Sugars ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
# t = 0.11384, df = 69.985, p-value = 0.9097
# CI -3.228746  3.619659
# Mean -> HPF =  6.086964  NHPF =  5.891507
#-------------------------------------------------------------------------------
#================================= 3.11 SODIUM =================================
t.test(Sodium ~ Palatability, data = food_data)
#------------------------------------ OUTPUT -----------------------------------
# t = 2.5222, df = 30.88, p-value = 0.01703
# CI  0.02209377 0.20894296
# Mean -> HPF =  0.21250322    NHPF =  0.09698485

#::::::::::::::::::::::::: 4. WELCH-STYLE EFFECT SIZES :::::::::::::::::::::::::
# Cohen's d was replicated against JASP 0.17.3.0 using the average-variance
# denominator rather than the weighted pooled variance.

#Cohen.d function uses the weighted pooled variance but we
# wanted the Unpooled SD (Welch-style / average-variance meaning
# each group’s variance is separate
#So we got different values for effect size when using the formula
# and comparing it with JASP.
# Therefore we manually calculated unpooled SD
# (unpooled SD, matches JASP unequal-variance d)
# d = (M1 - M2) / sqrt((sd1^2 + sd2^2) / 2)

## Welch test effect size
#================================= 4.1 PORTION =================================
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
#------------------------------------ OUTPUT -----------------------------------
# [1] -0.0769817
#-------------------------------------------------------------------------------
# Protein
#Welch test effect size
#================================= 4.2 PROTEIN =================================
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
#------------------------------------ OUTPUT -----------------------------------
# [1] -0.5269068

#-------------------------------------------------------------------------------

# Fibre
#Welch test effect size
#================================== 4.3 FIBRE ==================================
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
#------------------------------------ OUTPUT -----------------------------------
# [1] -0.543616

#-------------------------------------------------------------------------------
# Price
#Welch test effect size
#================================== 4.4 PRICE ==================================
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
#------------------------------------ OUTPUT -----------------------------------
# [1] -0.2975381

#-------------------------------------------------------------------------------
# Liking
#Welch test effect size
#================================== 4.5 LIKING =================================
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
#------------------------------------ OUTPUT -----------------------------------
# [1] -0.04956615
#-------------------------------------------------------------------------------
# Familiarity
#Welch test effect size
#=============================== 4.6 FAMILIARITY ===============================
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
#------------------------------------ OUTPUT -----------------------------------
# [1] -0.3148276
#-------------------------------------------------------------------------------
# Frequency of consumption
#Welch test effect size
#========================= 4.7 FREQUENCY OF CONSUMPTION ========================
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
#------------------------------------ OUTPUT -----------------------------------
# [1] -0.4397802
#-------------------------------------------------------------------------------

#=============================== 4.8 CARBOHYDRATE ==============================
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
#------------------------------------ OUTPUT -----------------------------------
# -0.4007859
#-------------------------------------------------------------------------------

#=================================== 4.9 FAT ===================================
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
#------------------------------------ OUTPUT -----------------------------------
#  0.6728515
#-------------------------------------------------------------------------------

#================================= 4.10 SUGARS =================================
# Welch test effect size
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
#------------------------------------ OUTPUT -----------------------------------
# 0.02621674
#-------------------------------------------------------------------------------

#================================= 4.11 SODIUM =================================
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
#------------------------------------ OUTPUT -----------------------------------
# 0.661655

#:::::::::::::::::::::::::::::::::THE END:::::::::::::::::::::::::::::::::::::::