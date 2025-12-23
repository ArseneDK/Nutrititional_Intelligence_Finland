# ==============================================================================================================================================
# 
# Nutritional intellgence in FinnFoodPics
# created by Arsene Kanyamibwa (arsene.kanyamibwa@helsinki.fi) and reviewed by Manon Chédeville (manon.chedeville@helsinki.fi)
# Puprpose: Test effects of palatability and category on ratings, accounting for repeated measures per participant.
# Script: Mixed-effects models for food ratings (EED, satiety, liking, etc.)
#
# created: 18.06.2025
# last updated: 08.10.2025
#
# ============================================================================================================================================== 
#Clear environment
rm(list=ls())

# Install packages once (comment out in regular use)
# install.packages(c(
#   "tidyverse", "ggpubr", "dplyr", "knitr", "effsize",
#   "Matrix", "lme4", "broom.mixed", "emmeans", "performance",
#   "lmerTest", "sjstats", "see", "parameters"
# ))

# Load libraries used in this script
{
library(tidyverse)    # data handling and plotting
library(ggpubr)       # publication-ready plots (if needed later)
library(knitr)        # reporting
library(dplyr)        # data manipulation
library(Matrix)       # required by lme4
library(lme4)         # mixed-effects models
library(lmerTest)     # p-values/df for lmer models
library(emmeans)      # estimated marginal means (if needed)
library(performance)  # model diagnostics (check_model)
library(broom.mixed)  # tidy model outputs
library(sjstats)      # additional model summaries/effect sizes
library(parameters)   # model_parameters for CIs and fixed/random effects
}
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Set working directory (use the one appropriate for your system)
# Windows path (lab server)
setwd("P:/h345/obrain_labspace/Projects/PhD_projects/MARVEL/02_MARVEL I_Macronutrient study/03_Experiment/Aim 1/FoodRating task/03_Data/05_Food Ratings Data")
food_data_long <- read.csv("0_Complete_pictureset_data_long.csv")

# Mac path (mounted volume) – uncomment when running on Mac
# setwd("/Volumes/h345/obrain_labspace/Projects/PhD_projects/MARVEL/02_MARVEL I_Macronutrient study/03_Experiment/Aim 1/FoodRating task/03_Data/05_Food Ratings Data")
# food_data_long <- read.csv("0_Complete_pictureset_data_long.csv")
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Data import and cleaning

# Import picture‑set data
food_data <- read.csv("0_Complete_pictureset_data.csv")

# Drop first column (index column created when exporting from R/Excel)
food_data_long <- food_data_long[, -1]

# Convert categorical variables to factors for modelling
{
  food_data_long$Category       <- factor(food_data_long$Category)
  food_data_long$Palatability   <- factor(food_data_long$Palatability)
  food_data_long$NOVA_Group     <- factor(food_data_long$NOVA_Group)
  food_data_long$Energy_levels  <- factor(food_data_long$Energy_levels)
  food_data_long$Energy_levels2 <- factor(food_data_long$Energy_levels2)
  food_data_long$NOVA_Group2    <- factor(food_data_long$NOVA_Group2)
}
# Set explicit factor order for energy levels (used in plotting / modelling)
{
food_data_long$Energy_levels <- factor(
  food_data_long$Energy_levels,
  levels = c("HED", "MED", "LED")
)
food_data_long$Energy_levels2 <- factor(
  food_data_long$Energy_levels2,
  levels = c("VHED", "HED", "MED", "LED", "VLED")
)
}
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Random-intercept model: EED predicted by palatability and category,
# with participant-specific intercepts to account for repeated ratings.

#Model 1: Estimated energy density (EED)
EED_null <- lmer(
  Estimated_ED ~ Palatability + Category + (1 | Participant),
  data = food_data_long
)

summary(EED_null)
#
#REML criterion at convergence: 38341.9

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.9767 -0.5711  0.1292  0.6706  2.9289 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept)  98.97    9.948  
#Residual                301.49   17.363  
#Number of obs: 4464, groups:  Participant, 62

#Fixed effects:
#                          Estimate   Std. Error   df    t value  Pr(>|t|)    
#   (Intercept)           55.0450     1.4697  102.6913  37.452   <2e-16 ***
#  PalatabilityNot HPF    0.4384     0.6134  4399.0000   0.715    0.475    
#  CategoryCombo          9.2320     0.7164  4399.0000  12.887   <2e-16 ***
#  CategoryFat            9.2444     0.7126  4399.0000  12.973   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) PlNHPF CtgryC
#PltbltyNHPF -0.398              
#CategoryCmb -0.406  0.493       
#CategoryFat -0.342  0.330  0.595

# Visual checks of model assumptions (residuals and normality)
visuals_assumptions <- data.frame(
  predicted=predict(EED_null),
  residual = residuals(EED_null),
  referrer = food_data_long$Palatability
  ) 

# Residuals vs fitted: check homoscedasticity and non-linearity
ggplot(visuals_assumptions,aes(x=predicted,y=residual, colour= referrer))+
  geom_point() + 
  geom_hline(yintercept=0, lty=3)

# Histogram of residuals: approximate normality check
ggplot(visuals_assumptions,aes(x=residual)) + 
  geom_histogram(bins=20, color="black") 

#QQ-plot of residuals: more formal visual normality check
ggplot(visuals_assumptions,aes(sample=residual)) + 
  stat_qq() + 
  stat_qq_line()

# Check random effects structure and overall model performance
check_model(EED_null)   # global diagnostics (performance package) -> (Linearity, Posterior predictive check, Variance Collinearity, Normality of residuals and random effect)

.............................................
## Extract fixed and random effects with confidence intervals
model_ci_eed <- model_parameters(EED_null, ci = 0.95)
print(model_ci_eed)
# Fixed Effects 

#Parameter              | Coefficient |   SE |         95% CI | t(4458) |      p
#-------------------------------------------------------------------------------
#(Intercept)            |       55.04 | 1.47 | [52.16, 57.93] |   37.45 | < .001
#Palatability [Not HPF] |        0.44 | 0.61 | [-0.76,  1.64] |    0.71 | 0.475 
#Category [Combo]       |        9.23 | 0.72 | [ 7.83, 10.64] |   12.89 | < .001
#Category [Fat]         |        9.24 | 0.71 | [ 7.85, 10.64] |   12.97 | < .001

# Random Effects 

#Parameter                   | Coefficient
#-----------------------------------------
#SD (Intercept: Participant) |        9.95
#SD (Residual)               |       17.36
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Model 2: Expected satiety

ES_null <- lmer(
  Expected_Satiety ~ Palatability + Category + (1 | Participant),
  data = food_data_long
)

summary(ES_null)
#REML criterion at convergence: 39106.2
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.2977 -0.7079 -0.0335  0.7104  3.3527 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept) 163.9    12.80   
#Residual                356.3    18.87   
#Number of obs: 4464, groups:  Participant, 62

#Fixed effects:
#                       Estimate    Std. Error  df      t value Pr(>|t|)    
#(Intercept)             35.3937     1.8194   90.0367  19.453  < 2e-16 ***
# PalatabilityNot HPF    3.6337     0.6668 4399.0000   5.450 5.32e-08 ***
# CategoryCombo          0.8132     0.7787 4399.0000   1.044    0.296    
#CategoryFat            7.9855     0.7746 4399.0000  10.309  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) PlNHPF CtgryC
#PltbltyNHPF -0.350              
#CategoryCmb -0.357  0.493       
#CategoryFat -0.301  0.330  0.595

# Visual checks for model 2 (same structure as above)
visuals_assumptions <- data.frame(
  predicted = predict(ES_null),
  residual  = residuals(ES_null),
  referrer  = food_data_long$Palatability
)

ggplot(visuals_assumptions, aes(x = predicted, y = residual, colour = referrer)) +
  geom_point() +
  geom_hline(yintercept = 0, lty = 3)

ggplot(visuals_assumptions, aes(x = residual)) +
  geom_histogram(bins = 20, color = "black")

ggplot(visuals_assumptions, aes(sample = residual)) +
  stat_qq() +
  stat_qq_line()

# Global model diagnostics
check_model(ES_null)

# Fixed and random effects with 95% CIs
model_ci_es <- model_parameters(ES_null, ci = 0.95)
print(model_ci_es)
# Fixed Effects 

#Parameter              | Coefficient |   SE |         95% CI | t(4458) |      p
#-------------------------------------------------------------------------------
#(Intercept)            |       35.39 | 1.82 | [31.83, 38.96] |   19.45 | < .001
#Palatability [Not HPF] |        3.63 | 0.67 | [ 2.33,  4.94] |    5.45 | < .001
#Category [Combo]       |        0.81 | 0.78 | [-0.71,  2.34] |    1.04 | 0.296 
#Category [Fat]         |        7.99 | 0.77 | [ 6.47,  9.50] |   10.31 | < .001

# Random Effects 

#Parameter                   | Coefficient
#-----------------------------------------
#  SD (Intercept: Participant) |       12.80
#  SD (Residual)               |       18.87

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Model 3: Liking

Liking_null <- lmer(
  Liking ~ Palatability + Category + (1 | Participant),
  data = food_data_long
)

summary(Liking_null)
#REML criterion at convergence: 36364.1

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-4.3422 -0.5551  0.0169  0.6836  3.0143 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept)  19.63    4.43   
#Residual                196.43   14.02   
#Number of obs: 4464, groups:  Participant, 62

#Fixed effects:
#                      Estimate   Std. Error  df      t value Pr(>|t|)    
#(Intercept)           54.1296     0.8270  217.0299  65.451  < 2e-16 ***
#PalatabilityNot HPF    3.0178     0.4951 4399.0000   6.095  1.19e-09 ***
#CategoryCombo          6.5527     0.5783 4399.0000  11.332  < 2e-16 ***
#CategoryFat            3.4474     0.5752 4399.0000   5.993  2.22e-09 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) PlNHPF CtgryC
#PltbltyNHPF -0.571              
#CategoryCmb -0.583  0.493       
#CategoryFat -0.491  0.330  0.595

# Visual checks for model 3
visuals_assumptions <- data.frame(
  predicted = predict(Liking_null),
  residual  = residuals(Liking_null),
  referrer  = food_data_long$Palatability
)

ggplot(visuals_assumptions, aes(x = predicted, y = residual, colour = referrer)) +
  geom_point() +
  geom_hline(yintercept = 0, lty = 3)

ggplot(visuals_assumptions, aes(x = residual)) +
  geom_histogram(bins = 20, color = "black")

ggplot(visuals_assumptions, aes(sample = residual)) +
  stat_qq() +
  stat_qq_line()

# Global model diagnostics
check_model(Liking_null)

# Estimated fixed effects (EMMs-style output) and random effects
model_ci_liking <- model_parameters(Liking_null, ci = 0.95)
print(model_ci_liking)
# Fixed Effects 

#Parameter              | Coefficient |   SE |         95% CI | t(4458) |      p
#-------------------------------------------------------------------------------
#(Intercept)            |       54.13 | 0.83 | [52.51, 55.75] |   65.45 | < .001
#Palatability [Not HPF] |        3.02 | 0.50 | [ 2.05,  3.99] |    6.10 | < .001
#Category [Combo]       |        6.55 | 0.58 | [ 5.42,  7.69] |   11.33 | < .001
#Category [Fat]         |        3.45 | 0.58 | [ 2.32,  4.58] |    5.99 | < .001

# Random Effects 

#Parameter                   | Coefficient
#-----------------------------------------
#SD (Intercept: Participant) |        4.43
#SD (Residual)               |       14.02
#plots
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Model 4: Frequency of consumption

fof_null <- lmer(
  Frequency_of_consumption ~ Palatability + Category + (1 | Participant),
  data = food_data_long
)

summary(fof_null)
#REML criterion at convergence: 27028.8

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-1.7815 -0.6293 -0.3291  0.5088  5.4068 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept)  2.122   1.457   
#Residual                24.259   4.925   
#Number of obs: 4464, groups:  Participant, 62

#Fixed effects:
#                         Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)            1.6009     0.2821  242.2598   5.674 3.96e-08 ***
#PalatabilityNot HPF    1.2476     0.1740 4399.0000   7.170 8.73e-13 ***
#CategoryCombo          0.2929     0.2032 4399.0000   1.442     0.15    
#CategoryFat            2.0356     0.2021 4399.0000  10.070  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) PlNHPF CtgryC
#PltbltyNHPF -0.589              
#CategoryCmb -0.600  0.493       
#CategoryFat -0.506  0.330  0.595

# Visual checks for model 4
visuals_assumptions <- data.frame(
  predicted = predict(fof_null),
  residual  = residuals(fof_null),
  referrer  = food_data_long$Palatability
)

ggplot(visuals_assumptions, aes(x = predicted, y = residual, colour = referrer)) +
  geom_point() +
  geom_hline(yintercept = 0, lty = 3)

ggplot(visuals_assumptions, aes(x = residual)) +
  geom_histogram(bins = 20, color = "black")

ggplot(visuals_assumptions, aes(sample = residual)) +
  stat_qq() +
  stat_qq_line()

# Global model diagnostics
check_model(fof_null)

# Extract fixed and random effects for reporting
model_ci_fof <- model_parameters(fof_null, ci = 0.95)
print(model_ci_fof)
# Fixed Effects 

#Parameter              | Coefficient |   SE |        95% CI | t(4458) |      p
#------------------------------------------------------------------------------
#(Intercept)            |        1.60 | 0.28 | [ 1.05, 2.15] |    5.67 | < .001
#Palatability [Not HPF] |        1.25 | 0.17 | [ 0.91, 1.59] |    7.17 | < .001
#Category [Combo]       |        0.29 | 0.20 | [-0.11, 0.69] |    1.44 | 0.150 
#Category [Fat]         |        2.04 | 0.20 | [ 1.64, 2.43] |   10.07 | < .001

# Random Effects 

#Parameter                   | Coefficient
#-----------------------------------------
#SD (Intercept: Participant) |        1.46
#SD (Residual)               |        4.93

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Model 5: Perceived healthiness

health_null <- lmer(
  Healthiness ~ Palatability + Category + (1 | Participant),
  data = food_data_long
)

summary(health_null)
#REML criterion at convergence: 40895.2
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.7583 -0.7224 -0.1277  0.7342  2.9512 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept)  59.56    7.718  
#Residual                541.92   23.279  
#Number of obs: 4464, groups:  Participant, 62
#
#Fixed effects:
#  Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)           36.3391     1.4050  201.1411  25.863  < 2e-16 ***
#  PalatabilityNot HPF    6.2193     0.8223 4399.0000   7.563 4.77e-14 ***
#  CategoryCombo        -12.3966     0.9605 4399.0000 -12.907  < 2e-16 ***
#  CategoryFat            6.4610     0.9554 4399.0000   6.763 1.53e-11 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) PlNHPF CtgryC
#PltbltyNHPF -0.559              
#CategoryCmb -0.570  0.493       
#CategoryFat -0.480  0.330  0.595

# Visual checks for model 5
visuals_assumptions <- data.frame(
  predicted = predict(health_null),
  residual  = residuals(health_null),
  referrer  = food_data_long$Palatability
)

ggplot(visuals_assumptions, aes(x = predicted, y = residual, colour = referrer)) +
  geom_point() +
  geom_hline(yintercept = 0, lty = 3)

ggplot(visuals_assumptions, aes(x = residual)) +
  geom_histogram(bins = 20, color = "black")

ggplot(visuals_assumptions, aes(sample = residual)) +
  stat_qq() +
  stat_qq_line()

# Global model diagnostics
check_model(health_null)

# Extract fixed and random effects with CIs
model_ci_health <- model_parameters(health_null, ci = 0.95)
print(model_ci_health)

# Fixed Effects 

#Parameter              | Coefficient |   SE |           95% CI | t(4458) |      p
#---------------------------------------------------------------------------------
#(Intercept)            |       36.34 | 1.41 | [ 33.58,  39.09] |   25.86 | < .001
#Palatability [Not HPF] |        6.22 | 0.82 | [  4.61,   7.83] |    7.56 | < .001
#Category [Combo]       |      -12.40 | 0.96 | [-14.28, -10.51] |  -12.91 | < .001
#Category [Fat]         |        6.46 | 0.96 | [  4.59,   8.33] |    6.76 | < .001

# Random Effects 

#Parameter                   | Coefficient
#  -----------------------------------------
#SD (Intercept: Participant) |        7.72
#SD (Residual)               |       23.28

#:::::::::::::::::::::::::::THE END:::::::::::::::::::::::::::::::::::::::::::::

